# rsync-mode

`rsync-mode` is a minor mode to automatically rsync entire projects to
remote hosts in Emacs. The `rsync-mode` minor mode provides a spinner
in the status bar to display project/file synchronization progress. It
also offers an option to automatically rsync the current project on
every save.

## Project configuration
To specify remote hosts for the project, set the directory-local
variables `rsync-remote-paths`, `rsync-local-path`, and
`rsync-excluded-dirs` at the project root level.

Here is an example that will enable synchronization on save for any file that is in a subdirectory of the project root:

``` emacs-lisp
;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((rsync-remote-paths . ("remote:/path/to/remote/directory"))
         (rsync-local-path . "/path/to/local/directory/")
         (rsync-excluded-dirs . (".git"
                                 "data"
                                 ".ipynb_checkpoints"
                                 ".pytest_cache"
                                 "venv"
                                 "*.egg-info"))
         (rsync-sync-on-save . t)
         (eval . (rsync-mode)))))
```

## Commands
Two functions are defined for interactive use: `rsync` and
`rsync-all`. The former synchronizes files to a single remote, which
is chosen interactively, with completions read from the
`rsync-remote-paths` directory-local variable.

The function `rsync-all` synchronizes the current project to all
remote hosts found in the directory-local variable
`rsync-remote-paths`.

## Minor mode
The minor mode `rsync-mode` adds a progress bar indicator to the
modeline. If `rsync-sync-on-save` is `t`, `rsync-mode` also adds a
hook that runs `rsync-all` on save, which is especially convenient for
remote development.
