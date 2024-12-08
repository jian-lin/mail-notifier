;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((haskell-mode
  . ((eglot-workspace-configuration
      . (:haskell
         ;; our hlint config must come after the relude one to override it
         (:plugin (:hlint (:config (:flags ["--hint=.hlint.relude.yaml"
                                            "--hint=.hlint.yaml"])))))))))
