;;; init.el -*- lexical-binding: t; -*-

(let ((host (shell-command-to-string "hostname -f")))
  (defconst IS-GLAPTOP (and IS-LINUX (string-match-p "roam" host)))
  (defconst IS-CROSTINI    (string-match-p "penguin" host))
  (defconst IS-WORKSTATION (string-match-p "google.*\.com" host))
  (defconst WORK (or IS-GLAPTOP IS-WORKSTATION)))

(when WORK (load (concat doom-private-dir "specific-init.el")))

(when (bound-and-true-p doom-debug-p)
  (require 'benchmark-init)
  (add-hook 'doom-first-input-hook #'benchmark-init/deactivate))

(doom! :input

       :completion
       company             ; the ultimate code completion backend
       (ivy                ; a search engine for love and life
        (:if IS-MAC +icons))

       :ui
       doom                ; what makes DOOM look the way it does
       fill-column         ; a `fill-column' indicator
       hl-todo             ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       modeline            ; snazzy, Atom-inspired modeline, plus API
       nav-flash           ; blink the current line after jumping
       ophints             ; highlight the region an operation acts on
       popup               ; tame sudden yet inevitable temporary windows
       vc-gutter           ; vcs diff in the fringe
       vi-tilde-fringe     ; fringe tildes to mark beyond EOB
       window-select       ; visually switch windows

       :editor
       (:if (not WORK)
           file-templates) ; auto-snippets for empty files
       fold                ; (nigh) universal code folding
       (format             ; automated prettiness
        (:if (not WORK) +onsave)) ; breaks work setup

       :emacs
       (dired +icons)      ; making dired pretty [functional]
       electric            ; smarter, keyword-based electric-indent
       (undo +tree)        ; persistent, smarter undo for your inevitable mistakes
       vc                  ; version-control and Emacs, sitting in a tree

       :term
       vterm               ; another terminals in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       spell               ; tasing you for misspelling mispelling

       :tools
       (eval +overlay)     ; run code, run (also, repls)
       lookup              ; navigate your code and its documentation
       (:if (not WORK)
        lsp +eglot)
       magit               ; a git porcelain for Emacs
       rgb                 ; creating color strings

       :os
       tty                 ; improve the terminal Emacs experience

       :lang
       (:if (not WORK)
           cc)             ; C/C++/Obj-C madness
       data                ; config/data formats
       json                ; At least it ain't XML
       yaml                ; JSON, but readable
       emacs-lisp          ; drown in parentheses
       markdown            ; writing docs for people to ignore
       org                 ; organize your plain life in plain text
       (rust +lsp)         ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       sh                  ; she sells {ba,z,fi}sh shells on the C xor

       :email
       notmuch

       :config
       (default +bindings +smartparens))
