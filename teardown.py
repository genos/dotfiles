#!/usr/bin/env python
"""Uninstall my dotfiles"""

import os
from pathlib import Path

CONFIG = Path(__file__).parent.resolve() / Path("config.yaml")


if __name__ == "__main__":
    # Remove all symlinks listed in the CONFIG
    for line in CONFIG.read_text().strip().split("\n"):
        try:
            os.unlink(str(Path.home() / line.split(": ")[1]))
        except FileNotFoundError:
            pass
    print("Done!")
