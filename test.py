import subprocess
import re

def run_cmd(cmd):
    result = subprocess.run(cmd, shell=True, capture_output=True, text=True)

    # Get the output as a string
    status = result.returncode
    if status != 0:
        print("error")
        print(result.stderr)
    else:
        output = result.stdout

        # Print the output
        #print(output)
        return output
        
def parse_tag_line(line):
    if line == '':
        return None
    
    tag = {}
    entry = line.rsplit(';"', 1)
    
    normal_field = entry[0].split('\t', 2)
    # print(line)
    tag['tagname'] = normal_field[0]
    tag['tagfile'] = normal_field[1]
    tag['tagaddress'] = normal_field[2]
    
    if len(entry) == 2:
        for field in entry[1].split('\t')[1:]:
            field_entry = field.split(':', 1)
            # print(field_entry)
            if len(field_entry) == 2:
                tag.update(dict([field_entry]))
            else:
                tag.update({"kind": field_entry})
                
    #print(tag)
    return tag
    
def make_tag_annotation(tag):
    kind = tag.get("kind", "")
    typeref = tag.get("typeref", "")
    scope = tag.get("scope", "")
    extras = tag.get("extras", "")
    reference = "<R>" if "reference" in extras else ""
    
    typeref = typeref.removeprefix("typename:")
    typeref = re.sub(r'(^|:)(__anon[^:]+)(:|$)', r'\1__anon\3', typeref)
    scope = re.sub(r'(^|:)(__anon[^:]+)(:|$)', r'\1__anon\3', scope)
    
    annotation = (reference, 
                  kind, 
                  "/" if (kind != "" and typeref != "") else "",
                  typeref,
                  "@" if (scope != "") else "",
                  scope)

    return ''.join(annotation)
    
def make_ctags_acm_candidate(tag: dict):
    candidate = {}
    #print(tag)
    tagname = tag["tagname"]
    
    kind = tag.get("kind", "NOKIND")
    signature = tag.get("signature", "NOSIGN")
    annotation = make_tag_annotation(tag)
    
    candidate["key"] = tagname
    candidate["icon"] = kind
    candidate["label"] = tagname
    candidate["display-label"] = tagname
    candidate["annotation"] = annotation
    candidate["backend"] = "ctags"

    return candidate
    
# "(#(\"DEFUN\" 0 5 (citre-kind macro citre-signature \"(lname,fnname,sname,minargs,maxargs,intspec,doc)\" citre-annotation #(\" (macro)\" 0 2 (face citre-tag-annotation-face) 2 7 (face citre-tag-annotation-face) 7 8 (face citre-tag-annotation-face)) face font-lock-keyword-face))

def make_complete(cmd):
    output = run_cmd(cmd)
    
    lines = output.split('\n')
    lines = [l for l in lines if l != '']

    tags = map(parse_tag_line, lines)
    candidates = map(make_ctags_acm_candidate, tags)

    return list(candidates)
    
output = make_complete("readtags -t /Users/bytedance/code/emacs/tags -Q '(not (or (and $extras ((string->regexp \"(^|,) ?(anonymous|reference)(,|$)\" :case-fold false) $extras)) (or (and $extras ((string->regexp \"(^|,) ?(inputFile)(,|$)\" :case-fold false) $extras)) (and $kind ((string->regexp \"^(file|F)$\" :case-fold false) $kind))) false))' -S '(<or> (if (and $name &name) (<> (length $name) (length &name)) 0) (if (and $name &name) (<> $name &name) 0))' -Enep - stretch")

#print(output)

# readtags -t /Users/bytedance/code/emacs/tags -Q '(not (or (and $extras ((string->regexp "(^|,) ?(anonymous|reference)(,|$)" :case-fold false) $extras)) (or (and $extras ((string->regexp "(^|,) ?(inputFile)(,|$)" :case-fold false) $extras)) (and $kind ((string->regexp "^(file|F)$" :case-fold false) $kind))) false))' -S '(<or> (if (and $name &name) (<> (length $name) (length &name)) 0) (if (and $name &name) (<> $name &name) 0))' -Enep - DEFUN