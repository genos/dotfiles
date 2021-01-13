"""
Configuration example for ``ptpython``.

Copy this file to $XDG_CONFIG_HOME/ptpython/config.py
On Linux, this is: ~/.config/ptpython/config.py
"""
from prompt_toolkit.filters import ViInsertMode
from prompt_toolkit.key_binding.key_processor import KeyPress
from prompt_toolkit.keys import Keys
from prompt_toolkit.styles import Style

from ptpython.layout import CompletionVisualisation

__all__ = ["configure"]


def configure(repl):
    """
    Configuration method. This is called during the start-up of ptpython.

    :param repl: `PythonRepl` instance.
    """
    # Vi mode.
    repl.vi_mode = True

    # Enable open-in-editor. Pressing C-x C-e in emacs mode or 'v' in
    # Vi navigation mode will open the input in the current editor.
    repl.enable_open_in_editor = True

    # Use this colorscheme for the code.
    repl.use_code_colorscheme("monokai")
