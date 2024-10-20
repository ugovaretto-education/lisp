# Configuration

Use `~/.config/emacs` as root directory on Mac an Linux.
Use `use-package` to install and configure packages.
Put `.dir-locals.el` in any directory to create per-folder/project configuarations.
`early-init.el` is run before `init.el` if present.
(electric-pair-mode): automatically add matching parentheses.

*SBLC* reads the configuration from the file `$HOME/.sblrc` to use *quicklisp* from
other LISPs invoke `(load "~/.local/quicklisp/setup.lisp")` or wherever you
installed *quicklisp* (default path is `"~/quicklisp"`).

## Install SBCL and package manager:

After installing SBCL (e.g. `brew install sbcl` on MacOS):

### 1. Install `quicklisp`
```sh
curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
       --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
       --eval '(ql:add-to-init-file)' \
       --quit
```

### 2. Install *ultralisp* disttribution
```sh
sbcl --eval '(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)' --eval '(ql:update-all-dists)' --quit

```

### 3. (Optional) Install CLOG (GUI/Web)
```sh
sbcl --eval '(ql:quickload :clog/tools)' --eval '(clog-tools:clog-builder)'
```

### 4. Install *SLY* or *SLIME* on Emacs


### Updated packages

(ql:update-dist "ultralisp")
(ql:update-dist "quicklisp")


### Best themes:

* zenburn (low contrast)
* doom-toyo-night (mid contrast)
* doom-zenburn (low contras, higher contrast than zenburn)
* doom-rouge (high contrast)
* misterioso (mid contrast)
* modus-vivendi (high contrast)

Use `consult-theme` to change theme.

## Select lisp implementation

### SLIME

```lisp
;; CCL (ClosureCL)
(setq u:*ccl-home*  (concat u:*home* "Library/CCL/"))
(setq u:*ccl-init*  (concat u:*shared* "ccl-init.lisp"))
(setq u:*ccl-exec*  (concat u:*ccl-home* "ccl/dx86cl64"))
(setenv "CCL_DEFAULT_DIRECTORY" (concat u:*ccl-home* "ccl"))

;; SBCL
(setq u:*sbcl-home* (concat u:*home* "Library/sbcl/"))
(setq u:*sbcl-init* (concat u:*shared* "sbcl-init.lisp"))

(setq-default slime-lisp-implementations
  `((sbcl  ("/usr/local/bin/sbcl" "--userinit" ,u:*sbcl-init*) :coding-system utf-8-unix)
     (ccl   (,u:*ccl-exec* "-n" "-l" ,u:*ccl-init*) :coding-system utf-8-unix)))

(defmacro define-slime-lisp (name)
  `(defun ,name ()  (interactive)  (let ((slime-default-lisp ',name))  (slime))))

(define-slime-lisp sbcl)
(define-slime-lisp ccl)
```
OR

```lisp
(setq slime-lisp-implementations
      '((sbcl ("/opt/sbcl/bin/sbcl" "--core" "/opt/sbcl/lib/sbcl/sbcl.core")
              :coding-system utf-8-unix
              :env ("SBCL_HOME=/opt/sbcl/lib/sbcl"))
        (ccl ("/opt/ccl/lx86cl64")
             :coding-system utf-8-unix)))
(require 'slime-autoloads)
```


### SLY

```lisp
(setq-default sly-lisp-implementations
    `((sbcl  ("sbcl") :coding-system utf-8-unix)
        (ccl   ("~/Downloads/2018-06/ccl/lx86cl64")
            :coding-system utf-8-unix)))

(defmacro define-sly-lisp (name)
  `(defun ,name ()  (interactive)  (let ((sly-default-lisp ',name))  (sly))))

(define-sly-lisp sbcl)
(define-sly-lisp ccl)
```

# Key bindings

C-g: reset status
C-x {: reduce horizonatal window size (C-u <N> to reduce by N characters)
C-x }: increase horizonal window size (C-u <N> to increase by N characters)
C-X ^: change vertical window size (use negative numbers to shrink + C-u)
C-/: undo
C-S-/: redo
M-y: browse kill ring
C-S: select
C-x S: select columns
C-w: kill
M-w: copy
M-f: move one word forward
M-b: move one word backward
C-v: scroll down
M-v: scroll up
M-<: move to beginning of buffer
M->: move to end of buffer
C-s: search
M-s w: fuzzy find word, typing C-s will repeat the search
M-g g: go to line
C-x-k: kill buffer
C-x 0: kill current window
C-x 1: make current window fullscreen
C-x 2: split horizontally
C-x 3: split vertically
S-<arrow>: move to other window
C-x o: cycle through windows
C-x-<left|right>: move to other buffer
C-x b: select buffer
M-x untabify: replace tabs with spaces
C-u <number>: repeat following command <number> times

M-g M-n: next compile error
M-g M-p: prev compile error

*Mark*
C-S-<left|right>: mark from current position to beginning of work at left or right
C-S-<up>: mark from current position to beginning of line above or beginning
of current line if no line above exists
C-S-<down>: mark from current position to end of line below or end of current line
if no line below exists

*Tabs*
C-x t 2: add tab
C-x t 0: close current tab
C-x t 1: keep only current tab
C-x t o: next tab

*Query replace*
1. C-s-w: at beginning of word select current word for replacement
2. Hit <Enter> and enter replacement word
3. Press '!' for replacing all occurrences or keep presin 'y' or space to replace next occurence

Elisp: C-j at end of elisp expression in scratch buffer


## Dired
C-x d
C-x d *.cpp only list file ending with ".cpp"
d: flag for deletion
x: delete flagged files
c: compress
C: copy
C-x-q: toggle buffer writable (to e.g. rename multiple files)

# From external packages

### xref
1. Install Universal CTags
2. Run `etags -e -R .` to in roor directory to generate `TAGS` file.

M-.: go to reference or file under cursor. Works with LSP as well without explicit TAG generation.

### Project
Add `.project.el` or `.projectile` file in root directory to turn
directory into a project folder.
C-x p f: fuzzy find file
C-x p p: list projects

### Mark Multiple
C-c q: mark-next-like-this

### Expand Region
C-q: expand-region

### Minions
C-.: show all minor modes in menu

### Embark
C-.: on target will show all the relevant actions
that can be performed on that target e.g. on  C++
class variables it shows the keys to use to "go to definition" or "show references" etc.

### Which key
No keybindings just press on any key combination and wait for a second to
display the list of possible keybindings e.g. 'C-x-' will show 'p'
for project/projectile, 'f' for search file etc.

### Consult
consult-imenu: show list of symbols e.g. in c++ all functions or classes.
consult-flymake: go to errors/warnings.
consult-theme: change theme.

### Yasnippet
yas/new-snippet: new snippet
yas/visit-snippet-file: edit snippet file.
C-S-e: expand snippet.

### SLY
C-r: history, keep pressing C-r to go back, '<space> expands last item added.
C-c: compile file or region from other window
C-c-k: compile and run buffer or region from other window

### Which-key
Enable by invoking `which-key-mode`.

### Magit
's': stage
'c': commit
'P': push

### Recentf
C-c-r: recent file list

### Dired
C-o: split window
+: new directory
d: mark for deletion
x: commit delete
R: move
m: mark
c: copy

### Vterm
C-c-t: copy mode, read-only text buffer
