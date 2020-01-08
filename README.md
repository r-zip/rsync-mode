# rsync-mode

`rsync-mode` is a minor mode to automatically rsync entire projects to
remote hosts in Emacs.

## Project configuration
To specify remote hosts for the project, set the directory-local
variables `rsync-remote-paths`, `rsync-local-path`, and
`rsync-excluded-dirs` at the project root level. Example:

``` emacs-lisp
;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((rsync-remote-paths . ("host1:projects/" "host2:projects/"))
         (rsync-local-path . "/path/to/local/project")
         (rsync-excluded-dirs . (".git"
                                 "data"
                                 ".ipynb_checkpoints"
                                 ".pytest_cache"
                                 "venv"
                                 "*.egg-info")))))
```

## Commands
Two functions are defined for interactive use: `rsync` and
`rsync-all`. The former synchronizes files to a single remote, which
is chosen interactively using `ivy`, with completions read from the
`rsync-remote-paths` directory-local variable. Integration with
`helm`, `ido`, and the default `completing-read` is planned.

The function `rsync-all` synchronizes the current project to all
remote hosts found in the directory-local variable
`rsync-remote-paths`.

## `rsync-mode`
