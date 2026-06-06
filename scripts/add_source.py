from pathlib import Path
import sys

target = sys.argv[1]
p = Path("app_epidemiologic_v17_academic_freeze.R")
lines = p.read_text().splitlines(keepends=True)
line = f'source("{target}")\n'

if any(line.strip() == x.strip() for x in lines):
    print("already present")
else:
    insert_at = 0
    for i, x in enumerate(lines[:100]):
        if x.strip().startswith("source(") or x.strip().startswith("library("):
            insert_at = i + 1
    lines.insert(insert_at, line)
    p.write_text("".join(lines))
    print("inserted")
