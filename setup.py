#!/usr/bin/env python3
"""Install my dotfiles"""

import os
from pathlib import Path

DOTFILES = Path(__file__).parent
CONFIG = DOTFILES / Path("config.yaml")


def process(line: str) -> None:
    """Split a line in the CONFIG and link the `right` to the `left`"""
    left, right = line.split(": ")
    actual, symlink = DOTFILES / left, Path.home() / right
    if not symlink.parent.exists():
        os.mkdir(symlink.parent)
    symlink.symlink_to(actual)


if __name__ == "__main__":
    for LINE in CONFIG.read_text().split("\n"):
        if LINE:
            process(LINE)
    print("Done!")
