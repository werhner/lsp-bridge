#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2022 Andy Stewart
#
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import threading
import os
import subprocess
import json
import socket
import traceback
import time
from core.utils import *
from contextlib import contextmanager


def is_ipv6(address):
    """Check if address is IPv6"""
    try:
        socket.inet_pton(socket.AF_INET6, address)
        return True
    except socket.error:
        return False


class SendMessageException(Exception):
    pass


class ContainerConnectionException(Exception):
    pass


class RemoteFileClient(threading.Thread):
    def __init__(self, ssh_conf, server_port, callback):
        threading.Thread.__init__(self)

        # Init.
        self.ssh_host = ssh_conf['hostname']
        self.ssh_user = ssh_conf.get('user', "root")
        self.ssh_port = ssh_conf.get('port', 22)
        self.ssh_alias = ssh_conf.get('alias')  # SSH config 中的 Host 别名
        self.server_port = server_port
        self.callback = callback

        [self.user_ssh_private_key,
         self.user_ssh_agent] = get_emacs_vars(["lsp-bridge-user-ssh-private-key",
                                                "lsp-bridge-user-ssh-agent"])

        # 新增属性
        self.local_port = None
        self.ssh_process = None
        self.sock = None
        self.lock = threading.Lock()

        # 获取配置
        [self.remote_python_command, self.remote_python_file, self.remote_log] = \
            get_emacs_vars(["lsp-bridge-remote-python-command",
                           "lsp-bridge-remote-python-file",
                           "lsp-bridge-remote-log"])
        [self.remote_heartbeat_interval] = get_emacs_vars(["lsp-bridge-remote-heartbeat-interval"])

    def _find_free_port(self):
        """获取一个可用的本地端口"""
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.bind(('127.0.0.1', 0))
            return s.getsockname()[1]

    def _build_ssh_command(self, extra_args=None):
        """构建基础 ssh 命令"""
        cmd = ['ssh']

        # 额外参数
        if extra_args:
            cmd.extend(extra_args)

        # 如果有 alias，直接使用 alias（会自动读取 ~/.ssh/config 中的配置）
        # 否则使用 user@host 格式
        if self.ssh_alias:
            cmd.append(self.ssh_alias)
        else:
            # 端口（只有在没有 alias 时才需要手动指定）
            if self.ssh_port != 22:
                cmd.extend(['-p', str(self.ssh_port)])
            cmd.append(f'{self.ssh_user}@{self.ssh_host}')

        return cmd

    def create_channel(self):
        """启动 SSH 端口转发并连接"""
        self.local_port = self._find_free_port()

        # 构建端口转发命令
        # 使用 localhost 而不是 127.0.0.1，以便同时支持 IPv4 和 IPv6
        forward_spec = f'{self.local_port}:localhost:{self.server_port}'
        extra_args = [
            '-N',  # 不执行远程命令
            '-o', 'ExitOnForwardFailure=yes',  # 端口转发失败时退出
            '-o', 'ControlMaster=no',  # 禁用连接复用
            '-o', 'ControlPath=none',  # 完全禁用控制套接字
            '-L', forward_spec
        ]

        # 心跳设置
        if self.remote_heartbeat_interval and self.remote_heartbeat_interval != 0:
            extra_args.extend(['-o', f'ServerAliveInterval={self.remote_heartbeat_interval}'])

        cmd = self._build_ssh_command(extra_args)

        log_time(f"SSH command: {' '.join(cmd)}")

        # 启动 SSH 进程
        self.ssh_process = subprocess.Popen(
            cmd,
            stdin=subprocess.DEVNULL,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE
        )

        log_time(f"SSH process started, pid: {self.ssh_process.pid}")

        # 等待端口转发就绪并连接
        self._wait_and_connect()

    def _wait_and_connect(self, max_retries=300, delay=0.2):
        """等待端口转发就绪并建立 socket 连接"""
        log_time(f"Waiting for SSH tunnel on port {self.local_port}...")

        for i in range(max_retries):
            # 检查 SSH 进程是否已退出（可能是认证失败等）
            if self.ssh_process.poll() is not None:
                stderr = self.ssh_process.stderr.read().decode()
                log_time(f"SSH process exited with code {self.ssh_process.returncode}, stderr: {stderr}")
                raise Exception(f"SSH process exited: {stderr}")

            try:
                self.sock = socket.create_connection(
                    ('127.0.0.1', self.local_port),
                    timeout=1
                )
                # 连接成功后，验证远程服务是否可用
                # 设置短超时来检测连接是否立即被断开
                self.sock.settimeout(0.5)
                try:
                    # 尝试接收数据，如果远程服务不存在，连接会被断开
                    data = self.sock.recv(1, socket.MSG_PEEK)
                    # 如果收到空数据，说明连接被关闭
                    if not data:
                        raise Exception("Remote service not available")
                except socket.timeout:
                    # 超时是正常的，说明连接有效但没有数据
                    pass
                except (ConnectionResetError, BrokenPipeError, OSError) as e:
                    # 连接被重置，说明远程服务不存在
                    self.sock.close()
                    raise Exception(f"Remote service not available: {e}")

                # 连接验证成功，移除超时设置
                self.sock.settimeout(None)
                log_time(f"Connected to SSH tunnel on port {self.local_port}")
                return
            except (socket.error, ConnectionRefusedError) as e:
                if i % 10 == 0:
                    log_time(f"Retry {i}/{max_retries}: waiting for tunnel... ({e})")
                time.sleep(delay)

        # 超时后检查 SSH 进程状态
        log_time(f"Failed to connect to SSH tunnel after {max_retries} retries")
        if self.ssh_process.poll() is None:
            # SSH 进程还在运行，尝试读取 stderr
            log_time(f"SSH process still running (pid: {self.ssh_process.pid}), trying to get stderr...")
            self.ssh_process.terminate()
            try:
                self.ssh_process.wait(timeout=2)
            except:
                self.ssh_process.kill()
            stderr = self.ssh_process.stderr.read().decode()
            log_time(f"SSH stderr: {stderr}")
        raise Exception(f"Failed to connect to SSH tunnel after {max_retries} retries")

    def send_message(self, message):
        """发送消息"""
        try:
            data = json.dumps(message)
            with self.lock:
                self.sock.sendall(f"{data}\n".encode("utf-8"))
        except socket.error as e:
            raise SendMessageException() from e
        else:
            log_time_debug(f"Sent to server {self.ssh_host} port {self.server_port}: {message}")

    def run(self):
        """接收消息循环"""
        try:
            sock_file = self.sock.makefile("r")
            while True:
                data = sock_file.readline().strip()
                if not data:
                    break

                message = parse_json_content(data)
                # 替换 host 为实际的远程服务器地址，而不是 SSH 隧道的本地地址
                message["host"] = self.ssh_host
                log_time_debug(f"Received from server {self.ssh_host} port {self.server_port}: {message}")
                self.callback(message)
        finally:
            self.close()

    def close(self):
        """关闭连接"""
        if self.sock:
            try:
                self.sock.close()
            except:
                pass
        if self.ssh_process:
            try:
                self.ssh_process.terminate()
                self.ssh_process.wait(timeout=5)
            except:
                self.ssh_process.kill()

    def _run_ssh_command(self, remote_cmd):
        """执行远程 SSH 命令"""
        cmd = self._build_ssh_command()
        cmd.append(remote_cmd)

        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True
        )
        return result

    def start_lsp_bridge_process(self):
        """启动远程 lsp-bridge 进程"""
        remote_python_command = self.remote_python_command
        remote_python_file = self.remote_python_file
        remote_log = self.remote_log

        # use -l option to bash as a login shell, ensuring that login scripts (like ~/.bash_profile) are read and executed.
        # This is useful for lsp-bridge to use environment settings to correctly find out language server command
        remote_cmd = f"""
        /bin/bash -l -c '
        pid=$(pgrep -f "lsp_bridge.py$")
        if [ "$pid" == "" ]; then
            echo -e "Start lsp-bridge process as user $(whoami)" | tee {remote_log}
            nohup {remote_python_command} {remote_python_file} >>{remote_log} 2>&1 &
            sleep 1
            if pgrep -f "lsp_bridge.py$" > /dev/null; then
                echo -e "Start lsp-bridge successfully" | tee -a {remote_log}
            else
                echo -e "Start lsp-bridge failed" | tee -a {remote_log}
            fi
        else
            echo "lsp-bridge already running with pid $pid"
        fi'
        """

        result = self._run_ssh_command(remote_cmd)
        log_time(f"Remote process started at {self.ssh_host}, stdout: {result.stdout}, stderr: {result.stderr}")

    def kill_lsp_bridge_process(self):
        """停止远程 lsp-bridge 进程"""
        remote_log = self.remote_log

        remote_cmd = f"""
        nohup /bin/bash -l -c '
        pid=$(pgrep -f "lsp_bridge.py$")
        echo "try kill $pid" | tee >> {remote_log}
        if ! [ "$pid" == "" ]; then
            echo -e "kill lsp-bridge process as user $(whoami)" | tee >>{remote_log}
            kill $pid
            if [ "$?" = "0" ]; then
                echo -e "Kill lsp-bridge successfully" | tee >>{remote_log}
            else
                echo -e "Kill lsp-bridge failed" | tee >>{remote_log}
            fi
        fi'
        """

        try:
            self._run_ssh_command(remote_cmd)
        except:
            pass


class DockerFileClient(threading.Thread):
    def __init__(self, container_name, server_port, callback):
        threading.Thread.__init__(self)
        self.container_name = container_name
        self.server_port = server_port
        self.callback = callback
        self.sock = None
        self.lock = threading.Lock()  # For thread-safe socket access

        self.connect()

    def connect(self):
        """Connect to docker container

        :raises: :class:`ContainerConnectionException`: if failed to connect
        """
        with self.lock:
            try:
                # Use create_connection to support both IPv4 and IPv6
                self.sock = socket.create_connection(("127.0.0.1", self.server_port))
            except Exception as e:
                raise ContainerConnectionException(e)

    def send_message(self, message):
        """Send message via socket

        :raises: :class:`SendMessageException`: if socket is invalid
        """
        try:
            if self.sock.fileno == -1:
                # container might be restarted, try to reconnect
                time.sleep(1)
                self.connect()

            data = json.dumps(message)
            self.sock.sendall(f"{data}\n".encode("utf-8"))

        except Exception as e:
            logger.exception(e)
            raise e
        else:
            log_time_debug(f"Sended to server {self.container_name} port {self.server_port}: {message}")

    def run(self):
        """Continuously listen for incoming data from the server."""
        try:
            sock_file = self.sock.makefile("r")
            while True:
                message = sock_file.readline().strip()
                if not message:
                    break

                message = parse_json_content(message)
                message["host"] = self.container_name
                log_time_debug(f"Received from server {self.container_name} port {self.server_port}: {message}")
                self.callback(message)

        except socket.error as e:
            raise SendMessageException() from e
        finally:
            self.sock.close()

class RemoteFileServer:
    def __init__(self, host, port):
        import socket

        # Init.
        self.host = host
        self.port = port
        family = socket.AF_INET6 if is_ipv6(host) else socket.AF_INET
        self.server = socket.socket(family, socket.SOCK_STREAM)
        self.server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.server.bind((self.host, self.port))
        self.server.listen(5)

        # Build event loop.
        self.event_loop = threading.Thread(target=self.event_dispatcher)
        self.event_loop.start()

        self.client_socket = None
        self.client_address = None

    def event_dispatcher(self):
        try:
            while True:
                self.client_socket, self.client_address = self.server.accept()

                threading.Thread(target=self.handle_client).start()
        except Exception as e:
            logger.exception(e)

    def handle_client(self):
        try:
            client_file = self.client_socket.makefile('r')
            while True:
                data = client_file.readline().strip()
                if not data:
                    break
                elif data == "ping":
                    log_time_debug(f"Server port {self.port} received ping from client {self.client_address}")
                    continue

                message = parse_json_content(data)
                log_time_debug(f"Server port {self.port} received message from client {self.client_address}: {message}")
                resp = self.handle_message(message)
                if resp:
                    self.client_socket.send(f"{resp}\n".encode("utf-8"))

            client_file.close()
            self.client_socket.shutdown(socket.SHUT_RDWR)
            self.client_socket.close()
            log_time(f"Server port {self.port} socket close for client {self.client_address}")
            self.client_socket = None
            self.client_address = None
        except Exception as e:
            logger.exception(e)

    def handle_message(self, message):
        return

    def send_message(self, message):
        try:
            if self.client_socket:
                if self.client_address:
                    message["host"] = self.client_address[0]

                data = json.dumps(message)
                self.client_socket.send(f"{data}\n".encode("utf-8"))
        except Exception as e:
            logger.exception(e)
            raise SendMessageException() from e
        else:
            log_time_debug(f"Server port {self.port} sended to client {self.client_address}: {message}")


class FileSyncServer(RemoteFileServer):
    def __init__(self, host, port):
        super().__init__(host, port)
        self.file_dict = {}
        self.file_locks = {}

    def handle_message(self, message):
        command = message["command"]

        if command == "open_file":
            return self.handle_open_file(message)
        elif command == "save_file":
            return self.handle_save_file(message)
        elif command == "close_file":
            return self.handle_close_file(message)
        elif command == "change_file":
            return self.handle_change_file(message)
        elif command == "remote_sync":
            return self.handle_remote_sync(message)
        elif command == "update_file":
            return self.handle_update_file(message)

    @contextmanager
    def file_access_lock(self, path):
        path = os.path.abspath(os.path.expanduser(path))

        #  use lock to prevent
        #    - utils.get_buffer_content
        #    - utils.get_file_content_from_file_server
        # from reading file content before handle_open_file finishes.
        lock = self.file_locks.setdefault(path, threading.Lock())
        try:
            lock.acquire()
            yield
        finally:
            lock.release()

    def get_file_content(self, path):
        if path in self.file_dict:
            return self.file_dict[path]
        else:
            # wait for the lock if only path haven't been read
            with self.file_access_lock(path):
                return self.file_dict.get(path, "")

    def handle_update_file(self, message):
        path = message["path"]
        self.file_dict[path] = message["content"]

    def handle_remote_sync(self, message):
        remote_info = message["remote_connection_info"]
        set_remote_connection_info(remote_info)

    def handle_open_file(self, message):
        path = message["path"]
        response = {**message, "path": path}

        if os.path.exists(path):
            with self.file_access_lock(path):
                with open(path) as f:
                    content = f.read()
                    response.update({
                        "path": path,
                        "content": content
                    })
                    self.file_dict[path] = content
        else:
            response.update({
                "path": path,
                "content": "",
                "error": f"Cannot found file {path} on server.",
            })

        return json.dumps(response)

    def handle_change_file(self, message):
        path = message["path"]
        if path not in self.file_dict:
            with open(path) as f:
                self.file_dict[path] = f.read()

        self.file_dict[path] = rebuild_content_from_diff(self.file_dict[path], message["args"][0], message["args"][1], message["args"][3])

    def handle_save_file(self, message):
        path = message["path"]

        if path in self.file_dict:
            with open(path, 'w') as file:
                file.write(self.file_dict[path])

    def handle_close_file(self, message):
        path = message["path"]

        if path in self.file_dict:
            del self.file_dict[path]

    def close_all_files(self):
        self.file_dict.clear()
        self.file_locks.clear()


class FileElispServer(RemoteFileServer):
    def __init__(self, host, port, lsp_bridge):
        self.lsp_bridge = lsp_bridge
        self.rpcs = {}
        # Set file_elisp_server BEFORE super().__init__() starts event_loop.
        # This ensures get_emacs_vars() can use RPC when handle_client() runs.
        lsp_bridge.file_elisp_server = self
        super().__init__(host, port)

    def handle_client(self):
        # remote server lsp-bridge process use this cient_socket to call elisp function from local Emacs.
        log_time(f"Client connect from {self.client_address[0]}:{self.client_address[1]}")

        self.rpcs.clear()
        threading.Thread(target=super().handle_client).start()

        self.lsp_bridge.init_search_backends()
        log_time("init_search_backends finish")
        # Signal that init_search_backends is done
        self.lsp_bridge.init_search_backends_complete_event.set()

    def handle_message(self, message):
        if message == "Connect":
            log_time("Drop 'say hello' message from local Emacs.")
            return
        else:
            ts = message["timestamp"]
            self.rpcs[ts]["result"] = message["result"]
            self.rpcs[ts]["completion"].set()

    def call_remote_rpc(self, message):
        ts = time.monotonic_ns()
        cpl = threading.Event()
        try:
            message["timestamp"] = ts
            self.rpcs[ts] = { "msg": message, "completion": cpl }
            self.send_message(message)
        except Exception as e:
            logger.exception(e)
            return None
        else:
            cpl.wait()
            result = self.rpcs[ts]["result"]
            del self.rpcs[ts]
            return result


class FileCommandServer(RemoteFileServer):
    def __init__(self, host, port, lsp_bridge):
        self.lsp_bridge = lsp_bridge
        self.remote_client_host = None  # Real host from client message
        super().__init__(host, port)

    def handle_client(self):
        # Record server host when lsp-bridge running in remote server.
        # we wait for init_search_backends to finish execution
        # before start thread to handle remote request
        log_time("wait for init_search_backends to finsih execution")
        self.lsp_bridge.init_search_backends_complete_event.wait()

        super().handle_client()

        # close all files when client disconnects
        self.lsp_bridge.file_server.close_all_files()
        self.lsp_bridge.close_all_files()

    def handle_message(self, message):
        # Record the real remote client host from message
        if "server" in message and message["server"]:
            self.remote_client_host = message["server"]

        if message["command"] == "lsp_request":
            # Call LSP request.
            self.lsp_bridge.event_queue.put({
                "name": "action_func",
                "content": ("_{}".format(message["method"]), [message["path"]] + message["args"])
            })
        elif message["command"] == "func_request":
            # Call lsp-bridge normal function.
            getattr(self.lsp_bridge, message["method"])(*message["args"])


def save_ip_to_file(ip, filename):
    with open(filename, 'r') as f:
        existing_ips = f.read().splitlines()

    if ip in existing_ips:
        return

    existing_ips.append(ip)

    with open(filename, 'w') as f:
        f.write('\n'.join(existing_ips))


def get_container_local_ip(container_name):
    try:
        command = "docker inspect -f '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}' " + container_name
        result = subprocess.check_output(command, shell=True, text=True).strip()
        return result
    except subprocess.CalledProcessError:
        message_emacs(f"get_container_local_ip error: {traceback.format_exc()}")
        return None


def save_ip(ip):
    user_emacs_dir = get_emacs_func_result("get-user-emacs-directory")
    ip_file = os.path.join(user_emacs_dir, "lsp-bridge", "remote_file", "ip.txt")
    touch(ip_file)

    save_ip_to_file(ip, ip_file)
