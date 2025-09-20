# BEAT - Buffer Editing And Traversal

Utility functions for Buffer Editing And Traversal (BEAT)

BEAT (Buffer Editing And Traversal) is a collection of utility functions to aid in traversing and editing buffers in a more familiar way.
This includes a series of functionalities ported from VS Code and other such CUA text editors.
Tries to, when possible, reuse standard elisp functions and use standard Emacs naming schemes (e.g. kill, save, dwim, etc.) for all functions.
All functions are opt-in and you may pick and choose which commands you want and which key-sequences you would like to bind them to.

## Config
How i configure this (using [`use-package`](https://github.com/jwiegley/use-package))


```elisp
(use-package beat
  :ensure t
  :vc (:url "https://github.com/ImFstAsFckBoi/beat")
  :bind (("C-w" . beat-dwim-kill)
         ("M-w" . beat-dwim-save)
         ("C-a" . beat-dwim-move-beginning-of-line)
         ("C-d" . beat-mark-around-word-or-next-match)
         ("C-<right>" . beat-right-to-boundary)
         ("C-<left>" . beat-left-to-boundary)
         ("C-<delete>" . beat-delete-right-to-boundary)
         ("C-<backspace>" . beat-delete-left-to-boundary)
         ("<up>" . beat-dwim-previous-line)))
```

## All functions


|Function name | Description|
|--------------|------------|
|`beat-dwim-kill` | Mimic VS Code functionality [C-x]: kill whole line if nothing is marked.|
|`beat-dwim-save` | Mimic VS Code functionality [C-c]: save whole line to kill-ring if nothing is marked.|
|`beat-delete-mark-yank` | PARTIALLY OBSOLETE! FUNCTIONALITY COVERED BY 'delete-selection-mode'. Mimic VS Code functionality [C-v]: delete marked region when| yanking.
|`beat-mark-around-word` | Mimic first part of VS Code functionality [C-d]: Mark the current word, forward and backwards|
|`beat-mark-around-word-or-next-match` | Mimic VS Code functionality [C-d]: If nothing is marked, mark word, if something is marked, mark next instance of it.|
|`beat-duplicate-line-down` | Mimic VS Code functionality [M-S-\<down\>]: Duplicate line down.|
|`beat-dwim-previous-line` | Mimic VS Code behavior: Pressing up on the first line moves the cursor to column 0.|
|`beat-delete-right-to-boundary` | Mimic VS Code functionality [C-\<delete\>]: Deletes sequences forward, with more similar rules to VS Code.|
|`beat-delete-left-to-boundary` | Mimic VS Code functionality [C-\<backspace\>]: Deletes sequences backwards, with more similar rules to VS Code.|
|`beat-right-to-boundary` | Go forward for all characters in the same char category.|
|`beat-left-to-boundary` | Go backward for all characters in the same char category.|
|`beat-dwim-move-beginning-of-line` | Default behavior move to first non white-space character. If at first non white-space character, move to beginning of line|


