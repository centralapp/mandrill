(
 ;; for each `<mode> . (<variable> . <value>)`, set the given variables
 ;; `eval` is a pseudovariable that evaluates its value
 (nil .
      (
       (eval .
             (progn
               ;; load the cosmos utilities
               (load (concat (locate-dominating-file buffer-file-name "cosmos.el") "cosmos"))
               ;; load cabal package with target
               (load-cosmos-target "mandrill" "mandrill-tests")
               )
             )
       )
      )
 )
