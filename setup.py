#!/usr/bin/env python
"""Install my dotfiles"""

import os
from pathlib import Path

DOTFILES = Path(__file__).parent.resolve()
CONFIG = DOTFILES / Path("config.yaml")


if __name__ == "__main__":
    # Split a line in the CONFIG and link the right to the left
    for line in CONFIG.read_text().strip().split("\n"):
        left, right = line.split(": ")
        actual, symlink = DOTFILES / left, Path.home() / right
        if not symlink.parent.exists():
            os.makedirs(symlink.parent)
        symlink.symlink_to(actual)
    print("Done!")
