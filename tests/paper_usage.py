"""List what the paper uses: every \\input, every \\includegraphics and every
macro it invokes, with the macro file that defines each macro.

    python tests/paper_usage.py [--overleaf DIR] [--paper prizes/v14.tex]
                                [--letter prizes/response_plosone_r1.tex]
                                [--out expected/paper_usage.csv]

Not part of the replication run: it reads the authors' Overleaf clone. The
output is the specification the package is held to. Columns:

    document   paper | letter
    kind       input | figure | macro
    name       the \\input path, the figure path, or the macro name
    defined_in for a macro, the .tex file (relative to the Overleaf clone)
               whose \\newcommand defines it

Comment lines (starting with %) and text after an unescaped % are ignored.
The scan covers the document and every file it \\input{}s, so a macro used in a
table's notes counts. A macro used in the letter but not in the paper appears
only under document = letter.
"""

import argparse
import csv
import os
import re

DEF_RE = re.compile(r"\\(?:re)?newcommand\*?\{\\([A-Za-z]+)\}")
INPUT_RE = re.compile(r"\\input\{([^}]*)\}")
FIG_RE = re.compile(r"\\includegraphics(?:\[[^]]*\])?\{([^}]*)\}")
MACRO_RE = re.compile(r"\\([A-Za-z]+)")


def strip_comments(text):
    out = []
    for line in text.splitlines():
        if line.lstrip().startswith("%"):
            continue
        out.append(re.sub(r"(^|[^\\])%.*$", r"\1", line))
    return "\n".join(out)


def read(path):
    with open(path, encoding="utf-8", errors="ignore") as fh:
        return strip_comments(fh.read())


def resolve(overleaf, doc_path, ref):
    """Overleaf resolves \\input relative to the project root, then to the
    document's directory."""
    ref = ref if ref.endswith(".tex") else ref + ".tex"
    for base in (overleaf, os.path.dirname(os.path.join(overleaf, doc_path))):
        cand = os.path.normpath(os.path.join(base, ref))
        if os.path.exists(cand):
            return os.path.relpath(cand, overleaf).replace("\\", "/")
    raise FileNotFoundError("%s inputs %s, not found" % (doc_path, ref))


def scan(overleaf, doc_path):
    text = read(os.path.join(overleaf, doc_path))
    inputs = [resolve(overleaf, doc_path, r) for r in INPUT_RE.findall(text)]
    figures = FIG_RE.findall(text)
    defined = {}     # macro -> file defining it
    bodies = [text]
    for inp in inputs:
        t = read(os.path.join(overleaf, inp))
        for m in DEF_RE.findall(t):
            defined.setdefault(m, inp)
        bodies.append(t)
    body = "\n".join(bodies)
    # a macro counts as used where it is invoked, not where it is defined
    body_wo_defs = DEF_RE.sub("", body)
    used = set(MACRO_RE.findall(body_wo_defs))
    macros = sorted(m for m in defined if m in used)
    return inputs, figures, macros, defined


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--overleaf", default=os.environ.get("OVERLEAF_REPO", "C:/Users/deivi/github/gtl-prizes-overleaf"))
    ap.add_argument("--paper", default="prizes/v14.tex")
    ap.add_argument("--letter", default="prizes/response_plosone_r1.tex")
    ap.add_argument("--out", default="expected/paper_usage.csv")
    a = ap.parse_args()

    rows = []
    for document, doc_path in (("paper", a.paper), ("letter", a.letter)):
        inputs, figures, macros, defined = scan(a.overleaf, doc_path)
        rows += [(document, "input", i, "") for i in inputs]
        rows += [(document, "figure", f, "") for f in figures]
        rows += [(document, "macro", m, defined[m]) for m in macros]
        print("%-6s %3d inputs, %3d figures, %3d macros" % (document, len(inputs), len(figures), len(macros)))

    with open(a.out, "w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(["document", "kind", "name", "defined_in"])
        w.writerows(rows)
    print("wrote", a.out)


if __name__ == "__main__":
    main()
