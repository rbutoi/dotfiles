((ace-window :source "elpaca-menu-lock-file" :recipe
             (:package "ace-window" :repo "abo-abo/ace-window" :fetcher github
                       :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :id ace-window :type git :protocol https
                       :inherit t :depth treeless :ref
                       "77115afc1b0b9f633084cf7479c767988106c196"))
 (acp :source "elpaca-menu-lock-file" :recipe
      (:package "acp" :fetcher github :repo "xenodium/acp.el" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :source "MELPA" :id acp :type git :protocol https :inherit t
                :depth treeless :ref "c32fbf8df34ed0095853a8cf55dc783e68b67d90"))
 (agent-shell :source "elpaca-menu-lock-file" :recipe
              (:package "agent-shell" :fetcher github :repo
                        "xenodium/agent-shell" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :id agent-shell :type git :protocol
                        https :inherit t :depth treeless :ref
                        "73718e228c2011b0f645ed4c6b3f2377965b1940"))
 (apheleia :source "elpaca-menu-lock-file" :recipe
           (:package "apheleia" :fetcher github :repo "radian-software/apheleia"
                     :files (:defaults ("scripts" "scripts/formatters")) :source
                     "MELPA" :id apheleia :type git :protocol https :inherit t
                     :depth treeless :ref
                     "e6e5d5523d229735ab5f8ec83e10beefcfd00d76"))
 (atomic-chrome :source "elpaca-menu-lock-file" :recipe
                (:package "atomic-chrome" :repo "alpha22jp/atomic-chrome"
                          :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :id atomic-chrome :type git :protocol
                          https :inherit t :depth treeless :ref
                          "072a137a19d7e6a300ca3e87c0e142a7f4ccb5fb"))
 (auto-compile :source "elpaca-menu-lock-file" :recipe
               (:package "auto-compile" :repo "emacscollective/auto-compile"
                         :fetcher github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                                    "*-test.el" "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :id auto-compile :type git :protocol
                         https :inherit t :depth treeless :ref
                         "cc51aea86617cab6cb1ef76672a343f2bf3206c1"))
 (auto-dark :source "elpaca-menu-lock-file" :recipe
            (:package "auto-dark" :repo "LionyxML/auto-dark-emacs" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :id auto-dark :type git :protocol https
                      :inherit t :depth treeless :ref
                      "6d1e8d2fc493dccbf05c9191611805c7e7881c70"))
 (auto-highlight-symbol :source "elpaca-menu-lock-file" :recipe
                        (:package "auto-highlight-symbol" :repo
                                  "elp-revive/auto-highlight-symbol" :fetcher
                                  github :files
                                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                   "*.texinfo" "doc/dir" "doc/*.info"
                                   "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                   "docs/dir" "docs/*.info" "docs/*.texi"
                                   "docs/*.texinfo"
                                   (:exclude ".dir-locals.el" "test.el"
                                             "tests.el" "*-test.el" "*-tests.el"
                                             "LICENSE" "README*" "*-pkg.el"))
                                  :source "MELPA" :id auto-highlight-symbol
                                  :type git :protocol https :inherit t :depth
                                  treeless :ref
                                  "e84da32e7cf1baefb0a9eef42a2fc842cf18f8b3"))
 (avy :source "elpaca-menu-lock-file" :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :source "MELPA" :id avy :type git :protocol https :inherit t
                :depth treeless :ref "933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (batppuccin :source "elpaca-menu-lock-file" :recipe
             (:source nil :package "batppuccin" :id batppuccin :repo
                      "https://github.com/bbatsov/batppuccin-emacs" :type git
                      :protocol https :inherit t :depth treeless :ref
                      "68e39af5ee2362a60fae31774a62b5c20b1ed74d"))
 (bufler :source "elpaca-menu-lock-file" :recipe
         (:package "bufler" :fetcher github :repo "alphapapa/bufler.el" :files
                   (:defaults (:exclude "helm-bufler.el")) :source "MELPA" :id
                   bufler :type git :protocol https :inherit t :depth treeless
                   :ref "b96822d2132fda6bd1dd86f017d7e76e3b990c82"))
 (burly :source "elpaca-menu-lock-file" :recipe
        (:package "burly" :fetcher github :repo "alphapapa/burly.el" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                   "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                   "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                   "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "MELPA" :id burly :type git :protocol https :inherit t
                  :depth treeless :ref
                  "d5b7133b5b629dd6bca29bb16660a9e472e82e25"))
 (cape :source "elpaca-menu-lock-file" :recipe
       (:package "cape" :repo "minad/cape" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :id cape :type git :protocol https :inherit t
                 :depth treeless :ref "7a6a752bc694e81853d915281a73a9c3acc69757"))
 (cfrs :source "elpaca-menu-lock-file" :recipe
       (:package "cfrs" :repo "Alexander-Miller/cfrs" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :id cfrs :type git :protocol https :inherit t
                 :depth treeless :ref "981bddb3fb9fd9c58aed182e352975bd10ad74c8"))
 (closql :source "elpaca-menu-lock-file" :recipe
         (:package "closql" :fetcher github :repo "magit/closql" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :id closql :type git :protocol https :inherit
                   t :depth treeless :ref
                   "947426d0c93e5ad5374c464b2f121c36cdaf2132"))
 (cmake-mode :source "elpaca-menu-lock-file" :recipe
             (:package "cmake-mode" :fetcher git :url
                       "https://gitlab.kitware.com/cmake/cmake.git" :files
                       ("Auxiliary/*.el") :source "MELPA" :id cmake-mode :type
                       git :protocol https :inherit t :depth treeless :ref
                       "3e3db36520f72424ba9f17a06264876fe3f90049"))
 (compiler-explorer :source "elpaca-menu-lock-file" :recipe
                    (:package "compiler-explorer" :fetcher github :repo
                              "mkcms/compiler-explorer.el" :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                               "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el" "tests.el"
                                         "*-test.el" "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "MELPA" :id compiler-explorer :type git
                              :protocol https :inherit t :depth treeless :ref
                              "e26a4dac90ca2ceed26b77470bdbea097e877aa4"))
 (cond-let
   :source "elpaca-menu-lock-file" :recipe
   (:package "cond-let" :fetcher github :repo "tarsius/cond-let" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :source "MELPA" :id cond-let :type git :protocol https :inherit t
             :depth treeless :ref "8bf87d45e169ebc091103b2aae325aece3aa804d"))
 (consult :source "elpaca-menu-lock-file" :recipe
          (:package "consult" :repo "minad/consult" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :id consult :type git :protocol https
                    :inherit t :depth treeless :ref
                    "20476c690ce3ecd45460011ce6b03fd58a642181"))
 (consult-dir :source "elpaca-menu-lock-file" :recipe
              (:package "consult-dir" :fetcher github :repo
                        "karthink/consult-dir" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :id consult-dir :type git :protocol
                        https :inherit t :depth treeless :ref
                        "1497b46d6f48da2d884296a1297e5ace1e050eb5"))
 (copilot :source "elpaca-menu-lock-file" :recipe
          (:package "copilot" :fetcher github :repo "copilot-emacs/copilot.el"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :id copilot :type git :protocol https
                    :inherit t :depth treeless :ref
                    "ab5c58bc969f52f6d75e972658f2c3381c70b4fa"))
 (corfu :source "elpaca-menu-lock-file" :recipe
        (:package "corfu" :repo "minad/corfu" :files
                  (:defaults "extensions/corfu-*.el") :fetcher github :source
                  "MELPA" :id corfu :type git :protocol https :inherit t :depth
                  treeless :ref "20009d4fcc31770200b63a1440f15320ee009def"))
 (crux :source "elpaca-menu-lock-file" :recipe
       (:package "crux" :fetcher github :repo "bbatsov/crux" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :id crux :type git :protocol https :inherit t
                 :depth treeless :ref "69e03917f6fd35e25b9a9dfd02df8ff3643f9227"))
 (csv-mode :source "elpaca-menu-lock-file" :recipe
           (:package "csv-mode" :repo
                     ("https://github.com/emacsmirror/gnu_elpa" . "csv-mode")
                     :tar "1.27" :host gnu :branch "externals/csv-mode" :files
                     ("*" (:exclude ".git")) :source "GNU ELPA" :id csv-mode
                     :type git :protocol https :inherit t :depth treeless :ref
                     "ba5dc934b9dbdc2b57ab1917a669cdfd7d1838d3"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi") :source "MELPA" :id dash :type git
                 :protocol https :inherit t :depth treeless :ref
                 "d3a84021dbe48dba63b52ef7665651e0cf02e915"))
 (deadgrep :source "elpaca-menu-lock-file" :recipe
           (:package "deadgrep" :repo "Wilfred/deadgrep" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                      "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :id deadgrep :type git :protocol https
                     :inherit t :depth treeless :ref
                     "edb1957d0d6033698c6784686b27508034003fa0"))
 (diff-hl :source "elpaca-menu-lock-file" :recipe
          (:package "diff-hl" :fetcher github :repo "dgutov/diff-hl" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :id diff-hl :type git :protocol https
                    :inherit t :depth treeless :ref
                    "b965e19e6e7f9933199e421849a49229207c1c9f"))
 (dired-gitignore :source "elpaca-menu-lock-file" :recipe
                  (:package "dired-gitignore" :fetcher github :repo
                            "johannes-mueller/dired-gitignore.el" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                             "doc/*.texinfo" "lisp/*.el" "docs/dir"
                             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el"
                                       "*-test.el" "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :id dired-gitignore :type git
                            :protocol https :inherit t :depth treeless :ref
                            "634b87d7551ec2ab403f371f239a938d4da6b7d2"))
 (dired-hide-dotfiles :source "elpaca-menu-lock-file" :recipe
                      (:package "dired-hide-dotfiles" :fetcher github :repo
                                "mattiasb/dired-hide-dotfiles" :files
                                ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                 "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                                 "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                                           "*-test.el" "*-tests.el" "LICENSE"
                                           "README*" "*-pkg.el"))
                                :source "MELPA" :id dired-hide-dotfiles :type
                                git :protocol https :inherit t :depth treeless
                                :ref "0d035ba8c5decc5957d50f3c64ef860b5c2093a1"))
 (dockerfile-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "dockerfile-mode" :fetcher github :repo
                            "spotify/dockerfile-mode" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                             "doc/*.texinfo" "lisp/*.el" "docs/dir"
                             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el"
                                       "*-test.el" "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :id dockerfile-mode :type git
                            :protocol https :inherit t :depth treeless :ref
                            "97733ce074b1252c1270fd5e8a53d178b66668ed"))
 (eglot-python-preset :source "elpaca-menu-lock-file" :recipe
                      (:package "eglot-python-preset" :fetcher github :repo
                                "mwolson/eglot-python-preset" :files
                                (:defaults "templates/*.tpl.py") :source "MELPA"
                                :id eglot-python-preset :type git :protocol
                                https :inherit t :depth treeless :ref
                                "39474289e20b366afdf000856c9dc784e12d751b"))
 (elisp-refs :source "elpaca-menu-lock-file" :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github
                       :files (:defaults (:exclude "elisp-refs-bench.el"))
                       :source "MELPA" :id elisp-refs :type git :protocol https
                       :inherit t :depth treeless :ref
                       "541a064c3ce27867872cf708354a65d83baf2a6d"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :package "elpaca" :id elpaca :repo
            "https://github.com/progfolio/elpaca.git" :ref
            "e9cb7eef2d8539e362d87f0489ab9eed8e8732c4" :depth 1 :inherit ignore
            :files (:defaults "elpaca-test.el" (:exclude "extensions")) :build
            (:not elpaca-activate) :type git :protocol https))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git" :files
                               ("extensions/elpaca-use-package.el") :main
                               "extensions/elpaca-use-package.el" :build
                               (:not elpaca-build-docs) :source
                               "Elpaca extensions" :id elpaca-use-package :type
                               git :protocol https :inherit t :depth treeless
                               :ref "e9cb7eef2d8539e362d87f0489ab9eed8e8732c4"))
 (emacsql :source "elpaca-menu-lock-file" :recipe
          (:package "emacsql" :fetcher github :repo "magit/emacsql" :files
                    (:defaults "README.md" "sqlite") :source "MELPA" :id emacsql
                    :type git :protocol https :inherit t :depth treeless :ref
                    "2fe6d4562b32a170a750d5e80514fbb6b6694803"))
 (embark :source "elpaca-menu-lock-file" :recipe
         (:package "embark" :repo "oantolin/embark" :fetcher github :files
                   ("embark.el" "embark-org.el" "embark.texi") :source "MELPA"
                   :id embark :type git :protocol https :inherit t :depth
                   treeless :ref "27de48004242e98586b9c9661fdb6912f26fe70f"))
 (embark-consult :source "elpaca-menu-lock-file" :recipe
                 (:package "embark-consult" :repo "oantolin/embark" :fetcher
                           github :files ("embark-consult.el") :source "MELPA"
                           :id embark-consult :type git :protocol https :inherit
                           t :depth treeless :ref
                           "27de48004242e98586b9c9661fdb6912f26fe70f"))
 (expand-region :source "elpaca-menu-lock-file" :recipe
                (:package "expand-region" :repo "magnars/expand-region.el"
                          :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :id expand-region :type git :protocol
                          https :inherit t :depth treeless :ref
                          "351279272330cae6cecea941b0033a8dd8bcc4e8"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                         "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :id f :type git :protocol https :inherit t :depth
              treeless :ref "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (fancy-fill-paragraph :source "elpaca-menu-lock-file" :recipe
                       (:package "fancy-fill-paragraph" :fetcher codeberg :repo
                                 "ideasman42/emacs-fancy-fill-paragraph" :files
                                 ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                  "*.texinfo" "doc/dir" "doc/*.info"
                                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                  "docs/dir" "docs/*.info" "docs/*.texi"
                                  "docs/*.texinfo"
                                  (:exclude ".dir-locals.el" "test.el"
                                            "tests.el" "*-test.el" "*-tests.el"
                                            "LICENSE" "README*" "*-pkg.el"))
                                 :source "MELPA" :id fancy-fill-paragraph :type
                                 git :protocol https :inherit t :depth treeless
                                 :ref "9f409bf7838d5aa06fc7c5ba7d2aef1fb32cabc2"))
 (fish-mode :source "elpaca-menu-lock-file" :recipe
            (:package "fish-mode" :fetcher github :repo "wwwjfy/emacs-fish"
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :id fish-mode :type git :protocol https
                      :inherit t :depth treeless :ref
                      "2526b1803b58cf145bc70ff6ce2adb3f6c246f89"))
 (flyover :source "elpaca-menu-lock-file" :recipe
          (:package "flyover" :fetcher github :repo "konrad1977/flyover" :files
                    ("flyover.el") :source "MELPA" :id flyover :type git
                    :protocol https :inherit t :depth treeless :ref
                    "8815cb067ca0d3d32607c9ae5093673cb29663a0"))
 (flyspell-correct :source "elpaca-menu-lock-file" :recipe
                   (:package "flyspell-correct" :repo
                             "d12frosted/flyspell-correct" :fetcher github
                             :files
                             ("flyspell-correct.el" "flyspell-correct-ido.el")
                             :source "MELPA" :id flyspell-correct :type git
                             :protocol https :inherit t :depth treeless :ref
                             "c6dfb9bebb90ecbcaa3fbd4d747a677e17698e02"))
 (forge :source "elpaca-menu-lock-file" :recipe
        (:package "forge" :fetcher github :repo "magit/forge" :files
                  ("lisp/*.el" "docs/*.texi" ".dir-locals.el") :source "MELPA"
                  :id forge :type git :protocol https :inherit t :depth treeless
                  :ref "f46e2be47ae89b18a0d6bb4496e60e72aa7a0df1"))
 (general :source "elpaca-menu-lock-file" :recipe
          (:package "general" :fetcher github :repo "noctuid/general.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :id general :wait t :type git :protocol
                    https :inherit t :depth treeless :ref
                    "a48768f85a655fe77b5f45c2880b420da1b1b9c3"))
 (ghub :source "elpaca-menu-lock-file" :recipe
       (:package "ghub" :fetcher github :repo "magit/ghub" :files
                 ("lisp/*.el" "docs/*.texi" ".dir-locals.el") :source "MELPA"
                 :id ghub :type git :protocol https :inherit t :depth treeless
                 :ref "1fb0fba075cb8b80f9819c874be584dffce50b51"))
 (git-link :source "elpaca-menu-lock-file" :recipe
           (:package "git-link" :fetcher github :repo "sshaw/git-link" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                      "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :id git-link :type git :protocol https
                     :inherit t :depth treeless :ref
                     "d9b375f79e6071a9926bf73bba64111adfc93bf5"))
 (git-modes :source "elpaca-menu-lock-file" :recipe
            (:package "git-modes" :fetcher github :repo "magit/git-modes"
                      :old-names
                      (gitattributes-mode gitconfig-mode gitignore-mode) :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :id git-modes :type git :protocol https
                      :inherit t :depth treeless :ref
                      "c3faeeea1982786f78d8c38397dec0f078eaec84"))
 (go-mode :source "elpaca-menu-lock-file" :recipe
          (:package "go-mode" :repo "dominikh/go-mode.el" :fetcher github :files
                    ("go-mode.el") :source "MELPA" :id go-mode :type git
                    :protocol https :inherit t :depth treeless :ref
                    "0ed3c5227e7f622589f1411b4939c3ee34711ebd"))
 (google-this :source "elpaca-menu-lock-file" :recipe
              (:package "google-this" :repo "Malabarba/emacs-google-this"
                        :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :id google-this :type git :protocol
                        https :inherit t :depth treeless :ref
                        "abdcb565503844e2146de42ab5ba898e90a2bb09"))
 (goto-chg :source "elpaca-menu-lock-file" :recipe
           (:package "goto-chg" :repo "emacs-evil/goto-chg" :fetcher github
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                      "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :id goto-chg :type git :protocol https
                     :inherit t :depth treeless :ref
                     "72f556524b88e9d30dc7fc5b0dc32078c166fda7"))
 (grip-mode :source "elpaca-menu-lock-file" :recipe
            (:package "grip-mode" :repo "seagle0128/grip-mode" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :id grip-mode :type git :protocol https
                      :inherit t :depth treeless :ref
                      "d2d27240d0150c00f0b9a5d7d840357e84d4728d"))
 (hcl-mode :source "elpaca-menu-lock-file" :recipe
           (:package "hcl-mode" :repo "hcl-emacs/hcl-mode" :fetcher github
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                      "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :id hcl-mode :type git :protocol https
                     :inherit t :depth treeless :ref
                     "1da895ed75d28d9f87cbf9b74f075d90ba31c0ed"))
 (helpful :source "elpaca-menu-lock-file" :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :id helpful :type git :protocol https
                    :inherit t :depth treeless :ref
                    "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39"))
 (ht :source "elpaca-menu-lock-file" :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :source "MELPA" :id ht :type git :protocol https :inherit t
               :depth treeless :ref "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (hungry-delete :source "elpaca-menu-lock-file" :recipe
                (:package "hungry-delete" :fetcher github :repo
                          "nflath/hungry-delete" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :id hungry-delete :type git :protocol
                          https :inherit t :depth treeless :ref
                          "d919e555e5c13a2edf4570f3ceec84f0ade71657"))
 (hydra :source "elpaca-menu-lock-file" :recipe
        (:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
                  (:defaults (:exclude "lv.el")) :source "MELPA" :id hydra :type
                  git :protocol https :inherit t :depth treeless :ref
                  "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (i3wm-config-mode :source "elpaca-menu-lock-file" :recipe
                   (:package "i3wm-config-mode" :repo
                             "Alexander-Miller/i3wm-Config-Mode" :fetcher github
                             :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                              "doc/*.texinfo" "lisp/*.el" "docs/dir"
                              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el"
                                        "*-test.el" "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :id i3wm-config-mode :type git
                             :protocol https :inherit t :depth treeless :ref
                             "188e3978807ec39eba3cb69d973c0062af324215"))
 (ialign :source "elpaca-menu-lock-file" :recipe
         (:package "ialign" :fetcher github :repo "mkcms/ialign" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :id ialign :type git :protocol https :inherit
                   t :depth treeless :ref
                   "61820880cf61d102a406e4e9be501a9227749c45"))
 (jq-mode :source "elpaca-menu-lock-file" :recipe
          (:package "jq-mode" :repo "ljos/jq-mode" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :id jq-mode :type git :protocol https
                    :inherit t :depth treeless :ref
                    "39acc77a63555b8556b8163be3d9b142d173c795"))
 (json-mode :source "elpaca-menu-lock-file" :recipe
            (:package "json-mode" :fetcher github :repo "json-emacs/json-mode"
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :id json-mode :type git :protocol https
                      :inherit t :depth treeless :ref
                      "466d5b563721bbeffac3f610aefaac15a39d90a9"))
 (json-snatcher :source "elpaca-menu-lock-file" :recipe
                (:package "json-snatcher" :fetcher github :repo
                          "Sterlingg/json-snatcher" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :id json-snatcher :type git :protocol
                          https :inherit t :depth treeless :ref
                          "b28d1c0670636da6db508d03872d96ffddbc10f2"))
 (just-mode :source "elpaca-menu-lock-file" :recipe
            (:package "just-mode" :repo "leon-barrett/just-mode.el" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :id just-mode :type git :protocol https
                      :inherit t :depth treeless :ref
                      "b6173c7bf4d8d28e0dbd80fa41b9c75626885b4e"))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files
                  ("llama.el" ".dir-locals.el") :source "MELPA" :id llama :type
                  git :protocol https :inherit t :depth treeless :ref
                  "d430d48e0b5afd2a34b5531f103dcb110c3539c4"))
 (lua-mode :source "elpaca-menu-lock-file" :recipe
           (:package "lua-mode" :repo "immerrr/lua-mode" :fetcher github :files
                     (:defaults (:exclude "init-tryout.el")) :source "MELPA" :id
                     lua-mode :type git :protocol https :inherit t :depth
                     treeless :ref "2f6b8d7a6317e42c953c5119b0119ddb337e0a5f"))
 (lv :source "elpaca-menu-lock-file" :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files ("lv.el")
               :source "MELPA" :id lv :type git :protocol https :inherit t
               :depth treeless :ref "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
                   "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
                   ("git-hooks" "git-hooks/*")
                   (:exclude "lisp/magit-section.el"))
                  :source "MELPA" :id magit :type git :protocol https :inherit t
                  :depth treeless :ref
                  "61950fca5e31c9a33d839b3994d93be6d370fb4c"))
 (magit-delta :source "elpaca-menu-lock-file" :recipe
              (:package "magit-delta" :fetcher github :repo
                        "dandavison/magit-delta" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :id magit-delta :type git :protocol
                        https :inherit t :depth treeless :ref
                        "5fc7dbddcfacfe46d3fd876172ad02a9ab6ac616"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo "magit/magit"
                          :files
                          ("lisp/magit-section.el" "docs/magit-section.texi"
                           "magit-section-pkg.el")
                          :source "MELPA" :id magit-section :type git :protocol
                          https :inherit t :depth treeless :ref
                          "61950fca5e31c9a33d839b3994d93be6d370fb4c"))
 (majutsu :source "elpaca-menu-lock-file" :recipe
          (:source nil :package "majutsu" :id majutsu :repo
                   "https://github.com/0WD0/majutsu" :type git :protocol https
                   :inherit t :depth treeless :ref
                   "c329beb4a959efe2ad07007dc9c983a0dfbf34a3"))
 (marginalia :source "elpaca-menu-lock-file" :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher github
                       :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :id marginalia :type git :protocol https
                       :inherit t :depth treeless :ref
                       "51a79bb82355d0ce0ee677151f041a3aba8cbfca"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo
                          "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :id markdown-mode :type git :protocol
                          https :inherit t :depth treeless :ref
                          "182640f79c3ed66f82f0419f130dffc173ee9464"))
 (minions :source "elpaca-menu-lock-file" :recipe
          (:package "minions" :fetcher github :repo "tarsius/minions" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :id minions :type git :protocol https
                    :inherit t :depth treeless :ref
                    "5b73cd443c28a6e9c8e5ddd60ada38afdf40dfb9"))
 (multiple-cursors :source "elpaca-menu-lock-file" :recipe
                   (:package "multiple-cursors" :fetcher github :repo
                             "magnars/multiple-cursors.el" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                              "doc/*.texinfo" "lisp/*.el" "docs/dir"
                              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el"
                                        "*-test.el" "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :id multiple-cursors :type git
                             :protocol https :inherit t :depth treeless :ref
                             "ddd677091afc7d65ce56d11866e18aeded110ada"))
 (mwim :source "elpaca-menu-lock-file" :recipe
       (:package "mwim" :repo "alezost/mwim.el" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :id mwim :type git :protocol https :inherit t
                 :depth treeless :ref "b41885b3e14653d17eabb2db0bd336ac972d5315"))
 (nix-mode :source "elpaca-menu-lock-file" :recipe
           (:package "nix-mode" :fetcher github :repo "NixOS/nix-mode" :files
                     (:defaults (:exclude "nix-company.el" "nix-mode-mmm.el"))
                     :source "MELPA" :id nix-mode :type git :protocol https
                     :inherit t :depth treeless :ref
                     "719feb7868fb567ecfe5578f6119892c771ac5e5"))
 (no-littering :source "elpaca-menu-lock-file" :recipe
               (:package "no-littering" :fetcher github :repo
                         "emacscollective/no-littering" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                                    "*-test.el" "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :id no-littering :type git :protocol
                         https :inherit t :depth treeless :ref
                         "7bab26a5912074669aa6b4246f79bbdcfc0f65ba"))
 (orderless :source "elpaca-menu-lock-file" :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :id orderless :type git :protocol https
                      :inherit t :depth treeless :ref
                      "3a2a32181f7a5bd7b633e40d89de771a5dd88cc7"))
 (pcre2el :source "elpaca-menu-lock-file" :recipe
          (:package "pcre2el" :fetcher github :repo "joddie/pcre2el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :id pcre2el :type git :protocol https
                    :inherit t :depth treeless :ref
                    "b4d846d80dddb313042131cf2b8fbf647567e000"))
 (pfuture :source "elpaca-menu-lock-file" :recipe
          (:package "pfuture" :repo "Alexander-Miller/pfuture" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                     "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                     "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :id pfuture :type git :protocol https
                    :inherit t :depth treeless :ref
                    "19b53aebbc0f2da31de6326c495038901bffb73c"))
 (plz :source "elpaca-menu-lock-file" :recipe
      (:package "plz" :repo ("https://github.com/alphapapa/plz.el.git" . "plz")
                :tar "0.9.1" :host gnu :files ("*" (:exclude ".git" "LICENSE"))
                :source "GNU ELPA" :id plz :type git :protocol https :inherit t
                :depth treeless :ref "e2d07838e3b64ee5ebe59d4c3c9011adefb7b58e"))
 (popper :source "elpaca-menu-lock-file" :recipe
         (:package "popper" :fetcher github :repo "karthink/popper" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :id popper :type git :protocol https :inherit
                   t :depth treeless :ref
                   "d83b894ee7a9daf7c8e9b864c23d08f1b23d78f6"))
 (posframe :source "elpaca-menu-lock-file" :recipe
           (:package "posframe" :fetcher github :repo "tumashu/posframe" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                      "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :id posframe :type git :protocol https
                     :inherit t :depth treeless :ref
                     "3a80911b2f45ce6926196930bb7d5cc662c7b3c8"))
 (pretty-hydra :source "elpaca-menu-lock-file" :recipe
               (:package "pretty-hydra" :repo "jerrypnz/major-mode-hydra.el"
                         :fetcher github :files ("pretty-hydra.el") :source
                         "MELPA" :id pretty-hydra :type git :protocol https
                         :inherit t :depth treeless :ref
                         "2494d71e24b61c1f5ef2dc17885e2f65bf98b3b2"))
 (rainbow-mode :source "elpaca-menu-lock-file" :recipe
               (:package "rainbow-mode" :repo
                         ("https://github.com/emacsmirror/gnu_elpa"
                          . "rainbow-mode")
                         :tar "1.0.6" :host gnu :branch "externals/rainbow-mode"
                         :files ("*" (:exclude ".git")) :source "GNU ELPA" :id
                         rainbow-mode :type git :protocol https :inherit t
                         :depth treeless :ref
                         "f7db3b5919f70420a91eb199f8663468de3033f3"))
 (reformatter :source "elpaca-menu-lock-file" :recipe
              (:package "reformatter" :repo "purcell/emacs-reformatter" :fetcher
                        github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :id reformatter :type git :protocol
                        https :inherit t :depth treeless :ref
                        "c0ddac04b7b937ed56d6bf97e4bfcc4eccfa501a"))
 (rust-mode :source "elpaca-menu-lock-file" :recipe
            (:package "rust-mode" :repo "rust-lang/rust-mode" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :id rust-mode :type git :protocol https
                      :inherit t :depth treeless :ref
                      "668069ad8b6ca20bd0d2334db1c0d046809affd6"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                         "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :id s :type git :protocol https :inherit t :depth
              treeless :ref "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (shell-maker :source "elpaca-menu-lock-file" :recipe
              (:package "shell-maker" :fetcher github :repo
                        "xenodium/shell-maker" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :id shell-maker :type git :protocol
                        https :inherit t :depth treeless :ref
                        "6377cbdb49248d670170f1c8dbe045648063583e"))
 (smartparens :source "elpaca-menu-lock-file" :recipe
              (:package "smartparens" :fetcher github :repo "Fuco1/smartparens"
                        :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :id smartparens :type git :protocol
                        https :inherit t :depth treeless :ref
                        "82d2cf084a19b0c2c3812e0550721f8a61996056"))
 (solaire-mode :source "elpaca-menu-lock-file" :recipe
               (:package "solaire-mode" :repo "hlissner/emacs-solaire-mode"
                         :fetcher github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                                    "*-test.el" "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :id solaire-mode :type git :protocol
                         https :inherit t :depth treeless :ref
                         "1bd0134194e48c8fe4089e9d505517935b2b15e3"))
 (spinner :source "elpaca-menu-lock-file" :recipe
          (:package "spinner" :repo
                    ("https://github.com/Malabarba/spinner.el" . "spinner") :tar
                    "1.7.4" :host gnu :files ("*" (:exclude ".git")) :source
                    "GNU ELPA" :id spinner :type git :protocol https :inherit t
                    :depth treeless :ref
                    "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (string-inflection :source "elpaca-menu-lock-file" :recipe
                    (:package "string-inflection" :fetcher github :repo
                              "akicho8/string-inflection" :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                               "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el" "tests.el"
                                         "*-test.el" "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "MELPA" :id string-inflection :type git
                              :protocol https :inherit t :depth treeless :ref
                              "4a2f87d7b47f5efe702a78f8a40a98df36eeba13"))
 (svelte-mode :source "elpaca-menu-lock-file" :recipe
              (:package "svelte-mode" :fetcher github :repo
                        "leafOfTree/svelte-mode" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :id svelte-mode :type git :protocol
                        https :inherit t :depth treeless :ref
                        "ac8fba901dc790976f9893e338c8ad1241b897c6"))
 (swift-mode :source "elpaca-menu-lock-file" :recipe
             (:package "swift-mode" :repo "swift-emacs/swift-mode" :fetcher
                       github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                        "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE" "README*"
                                  "*-pkg.el"))
                       :source "MELPA" :id swift-mode :type git :protocol https
                       :inherit t :depth treeless :ref
                       "cfae3b85ad09bd293df941261afbc21e41bbb5f8"))
 (system-packages :source "elpaca-menu-lock-file" :recipe
                  (:package "system-packages" :repo
                            ("https://gitlab.com/jabranham/system-packages"
                             . "system-packages")
                            :tar "1.1.2" :host gnu :files
                            ("*" (:exclude ".git")) :source "GNU ELPA" :id
                            system-packages :type git :protocol https :inherit t
                            :depth treeless :ref
                            "de2a98caad223ded3b58512d8f44a8307a228a93"))
 (terraform-mode :source "elpaca-menu-lock-file" :recipe
                 (:package "terraform-mode" :repo "hcl-emacs/terraform-mode"
                           :fetcher github :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                            "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                            "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el" "tests.el"
                                      "*-test.el" "*-tests.el" "LICENSE"
                                      "README*" "*-pkg.el"))
                           :source "MELPA" :id terraform-mode :type git
                           :protocol https :inherit t :depth treeless :ref
                           "01635df3625c0cec2bb4613a6f920b8569d41009"))
 (track-changes :source "elpaca-menu-lock-file" :recipe
                (:package "track-changes" :repo
                          ("https://github.com/emacs-mirror/emacs"
                           . "track-changes")
                          :tar "1.5" :host gnu :branch "master" :files
                          ("lisp/emacs-lisp/track-changes.el" (:exclude ".git"))
                          :source "GNU ELPA" :id track-changes :type git
                          :protocol https :inherit t :depth treeless :ref
                          "36354c8ab4556c64082a87c3ad8e1705abbb04cf"))
 (transient :source "elpaca-menu-lock-file" :recipe
            (:package "transient" :fetcher github :repo "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :id transient :type git :protocol https
                      :inherit t :depth treeless :ref
                      "8b14203107950d6eba0e17d14867e05547725219"))
 (transient-posframe :source "elpaca-menu-lock-file" :recipe
                     (:package "transient-posframe" :fetcher github :repo
                               "emacsorphanage/transient-posframe" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el"
                                          "*-test.el" "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :id transient-posframe :type git
                               :protocol https :inherit t :depth treeless :ref
                               "06e6a4dea56c6333b7be17e16e6e94b1cdd1d2e1"))
 (treemacs :source "elpaca-menu-lock-file" :recipe
           (:package "treemacs" :fetcher github :repo
                     "Alexander-Miller/treemacs" :files
                     (:defaults "Changelog.org" "icons" "src/elisp/treemacs*.el"
                                "src/scripts/treemacs*.py"
                                (:exclude "src/extra/*"))
                     :source "MELPA" :id treemacs :type git :protocol https
                     :inherit t :depth treeless :ref
                     "2ab5a3c89fa01bbbd99de9b8986908b2bc5a7b49"))
 (treemacs-magit :source "elpaca-menu-lock-file" :recipe
                 (:package "treemacs-magit" :fetcher github :repo
                           "Alexander-Miller/treemacs" :files
                           ("src/extra/treemacs-magit.el") :source "MELPA" :id
                           treemacs-magit :type git :protocol https :inherit t
                           :depth treeless :ref
                           "2ab5a3c89fa01bbbd99de9b8986908b2bc5a7b49"))
 (treepy :source "elpaca-menu-lock-file" :recipe
         (:package "treepy" :repo "volrath/treepy.el" :fetcher github :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :id treepy :type git :protocol https :inherit
                   t :depth treeless :ref
                   "28f0e2c2c75ea186e8beb570a4a70087926ff80b"))
 (typescript-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "typescript-mode" :fetcher github :repo
                            "emacs-typescript/typescript.el" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                             "doc/*.texinfo" "lisp/*.el" "docs/dir"
                             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el"
                                       "*-test.el" "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :id typescript-mode :type git
                            :protocol https :inherit t :depth treeless :ref
                            "2535780bdb318d86761b9bd21b0347ca6a89628f"))
 (ultra-scroll :source "elpaca-menu-lock-file" :recipe
               (:package "ultra-scroll" :fetcher github :repo
                         "jdtsmith/ultra-scroll" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                                    "*-test.el" "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :id ultra-scroll :type git :protocol
                         https :inherit t :depth treeless :ref
                         "0a9a26071ec33305cbc7a0f1dc7202702319d51f"))
 (vc-jj :source "elpaca-menu-lock-file" :recipe
        (:package "vc-jj" :repo
                  ("https://codeberg.org/emacs-jj-vc/vc-jj.el" . "vc-jj") :tar
                  "0.5" :host gnu :files ("*" (:exclude ".git")) :source
                  "GNU ELPA" :id vc-jj :type git :protocol https :inherit t
                  :depth treeless :ref
                  "7cdb661e2dee7ab37a9467f3aafc48065bb08b1b"))
 (vertico :source "elpaca-menu-lock-file" :recipe
          (:package "vertico" :repo "minad/vertico" :files
                    (:defaults "extensions/vertico-*.el") :fetcher github
                    :source "MELPA" :id vertico :type git :protocol https
                    :inherit t :depth treeless :ref
                    "f3c2033ba63880d6265cf1e1eb9e987792042fc4"))
 (vertico-posframe :source "elpaca-menu-lock-file" :recipe
                   (:package "vertico-posframe" :repo
                             ("https://github.com/tumashu/vertico-posframe"
                              . "vertico-posframe")
                             :tar "0.9.2" :host gnu :files
                             ("*" (:exclude ".git")) :source "GNU ELPA" :id
                             vertico-posframe :type git :protocol https :inherit
                             t :depth treeless :ref
                             "d6e06a4f1b34d24cc0ca6ec69d2d6c965191b23e"))
 (visual-fill-column :source "elpaca-menu-lock-file" :recipe
                     (:package "visual-fill-column" :fetcher codeberg :repo
                               "joostkremers/visual-fill-column" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el"
                                          "*-test.el" "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :id visual-fill-column :type git
                               :protocol https :inherit t :depth treeless :ref
                               "e1be9a1545157d24454d950c0ac79553c540edb7"))
 (visual-regexp :source "elpaca-menu-lock-file" :recipe
                (:package "visual-regexp" :repo "benma/visual-regexp.el"
                          :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el"
                                     "*-test.el" "*-tests.el" "LICENSE"
                                     "README*" "*-pkg.el"))
                          :source "MELPA" :id visual-regexp :type git :protocol
                          https :inherit t :depth treeless :ref
                          "48457d42a5e0fe10fa3a9c15854f1f127ade09b5"))
 (vterm :source "elpaca-menu-lock-file" :recipe
        (:package "vterm" :fetcher github :repo "akermu/emacs-libvterm" :files
                  ("CMakeLists.txt" "elisp.c" "elisp.h" "emacs-module.h" "etc"
                   "utf8.c" "utf8.h" "vterm.el" "vterm-module.c"
                   "vterm-module.h")
                  :source "MELPA" :id vterm :type git :protocol https :inherit t
                  :depth treeless :ref
                  "54c29d14bca05bdd8ae60cda01715d727831e3f9"))
 (vterm-toggle :source "elpaca-menu-lock-file" :recipe
               (:package "vterm-toggle" :fetcher github :repo
                         "jixiuf/vterm-toggle" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                                    "*-test.el" "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :id vterm-toggle :type git :protocol
                         https :inherit t :depth treeless :ref
                         "81d031f153c5fa656c744cd518b7d54c54506706"))
 (vundo :source "elpaca-menu-lock-file" :recipe
        (:package "vundo" :repo ("https://github.com/casouri/vundo" . "vundo")
                  :tar "2.4.0" :host gnu :files ("*" (:exclude ".git" "test"))
                  :source "GNU ELPA" :id vundo :type git :protocol https
                  :inherit t :depth treeless :ref
                  "e0af8c5845abf884a644215a9cac37f39c13cd5a"))
 (websocket :source "elpaca-menu-lock-file" :recipe
            (:package "websocket" :repo "ahyatt/emacs-websocket" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :id websocket :type git :protocol https
                      :inherit t :depth treeless :ref
                      "2195e1247ecb04c30321702aa5f5618a51c329c5"))
 (wgrep :source "elpaca-menu-lock-file" :recipe
        (:package "wgrep" :fetcher github :repo "mhayashi1120/Emacs-wgrep"
                  :files ("wgrep.el") :source "MELPA" :id wgrep :type git
                  :protocol https :inherit t :depth treeless :ref
                  "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f"))
 (which-key :source "elpaca-menu-lock-file" :recipe
            (:package "which-key" :repo "justbur/emacs-which-key" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :id which-key :type git :protocol https
                      :inherit t :depth treeless :ref
                      "38d4308d1143b61e4004b6e7a940686784e51500"))
 (which-key-posframe :source "elpaca-menu-lock-file" :recipe
                     (:package "which-key-posframe" :fetcher github :repo
                               "emacsorphanage/which-key-posframe" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                                "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el"
                                          "*-test.el" "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :id which-key-posframe :type git
                               :protocol https :inherit t :depth treeless :ref
                               "e4a9ce9a1b20de550fca51f14d055821980d534a"))
 (with-editor :source "elpaca-menu-lock-file"
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :source "MELPA" :id with-editor :type git :protocol https :inherit
             t :depth treeless :ref "64211dcb815f2533ac3d2a7e56ff36ae804d8338"))
 (ws-butler :source "elpaca-menu-lock-file" :recipe
            (:package "ws-butler" :fetcher git :url
                      "https://https.git.savannah.gnu.org/git/elpa/nongnu.git"
                      :branch "elpa/ws-butler" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :id ws-butler :type git :protocol https
                      :inherit t :depth treeless :ref
                      "67c49cfdf5a5a9f28792c500c8eb0017cfe74a3a"))
 (xclip :source "elpaca-menu-lock-file" :recipe
        (:package "xclip" :repo
                  ("https://github.com/emacsmirror/gnu_elpa" . "xclip") :tar
                  "1.11.1" :host gnu :branch "externals/xclip" :files
                  ("*" (:exclude ".git")) :source "GNU ELPA" :id xclip :type git
                  :protocol https :inherit t :depth treeless :ref
                  "7febe164de2a881b83b9d604d3c7cf20b69f422d"))
 (xterm-color :source "elpaca-menu-lock-file" :recipe
              (:package "xterm-color" :repo "atomontage/xterm-color" :fetcher
                        github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :id xterm-color :type git :protocol
                        https :inherit t :depth treeless :ref
                        "86fab1d247eb5ebe6b40fa5073a70dfa487cd465"))
 (yaml :source "elpaca-menu-lock-file" :recipe
       (:package "yaml" :repo "zkry/yaml.el" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :id yaml :type git :protocol https :inherit t
                 :depth treeless :ref "f2369fb4985ed054be47ae111760ff2075dff72a"))
 (yaml-mode :source "elpaca-menu-lock-file" :recipe
            (:package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher github
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                       "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                       "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                       "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE" "README*"
                                 "*-pkg.el"))
                      :source "MELPA" :id yaml-mode :type git :protocol https
                      :inherit t :depth treeless :ref
                      "d91f878729312a6beed77e6637c60497c5786efa"))
 (zig-mode :source "elpaca-menu-lock-file" :recipe
           (:package "zig-mode" :repo "ziglang/zig-mode" :fetcher codeberg
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                      "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                      "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                      "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE" "README*"
                                "*-pkg.el"))
                     :source "MELPA" :id zig-mode :type git :protocol https
                     :inherit t :depth treeless :ref
                     "20e395f940afe1e19e965050b0284ec418d6a9d5"))
 (zoom-window :source "elpaca-menu-lock-file" :recipe
              (:package "zoom-window" :fetcher github :repo
                        "emacsorphanage/zoom-window" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                         "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE" "README*"
                                   "*-pkg.el"))
                        :source "MELPA" :id zoom-window :type git :protocol
                        https :inherit t :depth treeless :ref
                        "8a0ae04de53583949a58e0aa8e7f64f03be7c9f8")))
