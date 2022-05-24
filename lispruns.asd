(asdf:defsystem "lispruns"
    :description "A speedrun timer using n-curses written in lisp"
    :version "0.1"
    :author "Simponic"
    :depends-on (:unix-opts
                 :mito
                 :sxql
                 :cl-ppcre
                 :croatoan
                 :local-time)
    :components ((:file "util") ;; Miscellaneous helpers
                 (:file "config") ;; For importing category configuration files
                 (:file "digits") ;; Lisp file with cool ascii digits
                 (:file "text" :depends-on ("digits")) ;; Helper functions for performing figlet-like actions and such
                 (:file "time") ;; Custom time forms
                 (:file "ui" :depends-on ("util" "text" "time")) ;; Functions to draw the UI
                 (:file "speedrun" :depends-on ("util")) ;; The actual timer logic
                 (:file "database/category") ;; Category DAO
                 (:file "database/run") ;; Run DAO
                 (:file "main" :depends-on ("util"
                                            "config"
                                            "ui"
                                            "speedrun"
                                            "database/category"
                                            "database/run"))))
