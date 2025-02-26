#!/usr/bin/env python3
"""Uninstall my dotfiles"""

from pathlib import Path

CONFIG = Path(__file__).parent.resolve() / Path("config.yaml")
# Remove all symlinks listed in the CONFIG
for line in CONFIG.read_text().strip().split("\n"):
    symlink = Path.home() / line.split(": ")[1]
    try:
        symlink.unlink()
    except FileNotFoundError:
        pass
print("Done!")
