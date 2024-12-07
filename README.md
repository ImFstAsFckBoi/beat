# BEAT+ - Buffer Editing And Traveral Plus

Utility functions for Buffer Editing And Traveral (BEAT)

BEAT+ (Buffer Editing And Traversal Plus) is a collection of utility functions to aid in traversing and editing buffers in a more familiar way.
This includes a series of functionalities ported from VS Code and other such CUA text editors.
Tries to, when possible, reuse standard elisp functions and use standard Emacs naming schemes (e.g. kill, save, dwim, etc.) for all functions.
All functions are opt-in and you may pick and choose which commands you want and which key-sequences you would like to bind them to.

## Install
You can install it automatically by running `M-x` `package-vc-install` `RET` `https://github.com/ImFstAsFckBoi/beatp`.
You can also do it manually by adding the `beat+.el` file to your load path (e.g. `~/.config/emacs/elisp/`)

If you don't have a custom load path, add it to your config like so
```elisp
(add-to-list 'load-path "~/.config/emacs/elisp/")
```

## Config
How i configure this (using [`use-package`](https://github.com/jwiegley/use-package))


```elisp
(use-package "beat+"
  :bind ("C-w" . beatp-dwim-kill)
        ("M-w" . beatp-dwim-save)
        ("C-d" . beatp-select-around-word-or-next-match)
        ("C-<right>" . beatp-right-to-boundary)
        ("C-<left>" . beatp-left-to-boundary)
        ("C-<delete>" . beatp-delete-right-to-boundary)
        ("C-<backspace>" . beatp-delete-left-to-boundary))
```





## All functions


|Function name | Description|
|--------------|------------|
|`beatp-dwim-kill` | Mimic VS Code functionality [C-x]: kill whole line if nothing is marked.|
|`beatp-dwim-save` | Mimic VS Code functionality [C-c]: save whole line to kill-ring if nothing is marked.|
|`beatp-delete-selection-yank` | PARTIALLY OBSOLETE! FUNCTIONALITY COVERED BY 'delete-selection-mode'. Mimic VS Code functionality [C-v]: delete marked region when| yanking.
|`beatp-select-around-word` | Mimic first part of VS Code functionality [C-d]: Mark the current word, forward and backwards|
|`beatp-select-around-word-or-next-match` | Mimic VS Code functionality [C-d]: If nothing is marked, select word, if something is marked, select next instance of it.|
|`beatp-duplicate-line-down` | Mimic VS Code functionality [M-S-\<down\>]: Duplicate line down.|
|`beatp-delete-right-to-boundary` | Mimic VS Code functionality [C-\<delete\>]: Deletes sequences forward, with more similar rules to VS Code.|
|`beatp-delete-left-to-boundary` | Mimic VS Code functionality [C-\<backspace\>]: Deletes sequences backwards, with more similar rules to VS Code.|
|`beatp-right-to-boundary` | Goto forward for all characters in the same char category.|
|`beatp-left-to-boundary` | Goto backward for all characters in the same char category.|

