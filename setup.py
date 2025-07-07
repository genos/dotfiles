#!/usr/bin/env python3
"""Install my dotfiles"""

import os
from pathlib import Path
import sys

DOTFILES = Path(__file__).parent
CONFIG = DOTFILES / "config.yaml"
# Split a line in the CONFIG and link the right to the left
for line in CONFIG.read_text().strip().split("\n"):
    left, right = line.split(": ")
    actual, symlink = DOTFILES / left, Path.home() / right
    if not symlink.parent.exists():
        os.makedirs(symlink.parent)
    try:
        symlink.symlink_to(actual)
    except FileExistsError:
        print(f"{symlink} already exists; did you clean up before?")
        sys.exit(1)
else:
    print("Done!")
