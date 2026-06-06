from pathlib import Path
import re
import sys

p = Path("app_epidemiologic_v17_academic_freeze.R")
text = p.read_text()

def remove_function(text, fname):
    m = re.search(rf"(?m)^{fname}\s*<-\s*function\b", text)
    if not m:
        print("not found:", fname)
        return text
    start = m.start()
    i = text.find("{", m.end())
    depth = 0
    for j in range(i, len(text)):
        if text[j] == "{":
            depth += 1
        elif text[j] == "}":
            depth -= 1
            if depth == 0:
                end = j + 1
                while end < len(text) and text[end] in "\n\r ":
                    end += 1
                print("removed:", fname)
                return text[:start] + text[end:]
    raise RuntimeError(fname)

for f in sys.argv[1:]:
    text = remove_function(text, f)

p.write_text(text)
