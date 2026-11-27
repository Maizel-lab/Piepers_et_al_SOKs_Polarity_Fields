import re
from pathlib import Path

def root_id(name: str) -> str:
    """Biological replicate (root) from an image/CSV file name.
    '<lif file> - <position>' : root = lif file (from its date) + line prefix + S<n>/plant<n>/Image<n>;
    primordium indices (LR1, LRP2, LR-1, ...) are ignored, so two primordia from the same
    sample position count as one root. Files without a position token are their own root."""
    stem = Path(name).name
    stem = re.sub(r"\.(csv|png|tif)$", "", stem)
    stem = re.sub(r"_(pos|mirrored)$", "", stem)
    if " - " in stem:
        lif, pos = stem.split(" - ", 1)
        m = re.search(r"\d{4}-\d{2}-\d{2}.*", lif)
        lif = m.group(0) if m else lif.strip()
        s = re.search(r"(?:^|_)(S\d+)(?=_|$|-)", pos)
        if s:
            line = re.search(r"(st[A-Za-z]*\d+-\d+-\d+|sC\d+(?:-\d+)*|sR\d+-\d+)", pos)
            return f"{lif}|{line.group(1)+'_' if line else ''}{s.group(1)}"
        for pat in (r"plant\d+", r"Image\d+"):
            t = re.search(pat, pos)
            if t:
                return f"{lif}|{t.group(0)}"
        return f"{lif}|{pos}"
    return stem
