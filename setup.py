#!/usr/bin/env python3

import os
from pathlib import Path

DOTFILES = Path.home() / Path("github/dotfiles")
CONFIG = DOTFILES / Path("config.yaml")

def mkdirs(path: Path) -> None:
    if not path.parent.exists():
        os.mkdir(path.parent)

def process(line: str) -> None:
    actual, symlink = line.split(": ")
    mkdirs(Path.home() / Path(symlink))
    (Path.home() / Path(symlink)).symlink_to(DOTFILES / actual)


if __name__ == "__main__":
    for line in CONFIG.read_text().split("\n"):
        if line:
            process(line)
    print("Done!")
