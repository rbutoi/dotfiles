;;; init.el -*- lexical-binding: t; -*-

(setq WORK ; has custom coding setup
      ;; double-negate to extract truthy value
      (not (not (string-match-p "google" (system-name)))))

(doom! :input

       :completion
       company             ; the ultimate code completion backend
       ivy                 ; a search engine for love and life

       :ui
       doom                ; what makes DOOM look the way it does
       doom-dashboard      ; a nifty splash screen for Emacs
       fill-column         ; a `fill-column' indicator
       hl-todo             ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       modeline            ; snazzy, Atom-inspired modeline, plus API
       nav-flash           ; blink the current line after jumping
       ophints             ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       vc-gutter           ; vcs diff in the fringe
       vi-tilde-fringe     ; fringe tildes to mark beyond EOB
       window-select       ; visually switch windows

       :editor
       (:if (not WORK)
           file-templates) ; auto-snippets for empty files
       fold                ; (nigh) universal code folding
       (format +onsave)    ; automated prettiness
       word-wrap           ; soft wrapping with language-aware indent

       :emacs
       (dired +icons)      ; making dired pretty [functional]
       electric            ; smarter, keyword-based electric-indent
       (undo +tree)        ; persistent, smarter undo for your inevitable mistakes
       vc                  ; version-control and Emacs, sitting in a tree

       :term
       vterm               ; another terminals in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       (spell +everywhere) ; tasing you for misspelling mispelling

       :tools
       (eval +overlay)     ; run code, run (also, repls)
       (:if (not WORK)
        lookup)            ; navigate your code and its documentation
       (:if IS-MAC macos)  ; MacOS-specific commands
       magit               ; a git porcelain for Emacs
       rgb                 ; creating color strings

       :lang
       (:if (not WORK)
           cc)             ; C/C++/Obj-C madness
       data                ; config/data formats
       yaml                ; JSON, but readable
       emacs-lisp          ; drown in parentheses
       markdown            ; writing docs for people to ignore
       org                 ; organize your plain life in plain text
       rust                ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       sh                  ; she sells {ba,z,fi}sh shells on the C xor

       :email
       notmuch

       :app
       irc                 ; how neckbeards socialize

       :config
       (default +bindings +smartparens))
