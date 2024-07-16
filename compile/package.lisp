(defpackage #:maclina.compile
  (:use #:cl)
  (:local-nicknames (#:m #:maclina.machine)
                    (#:arg #:maclina.argparse))
  (:shadow #:compile #:eval #:constantp #:macroexpand-1 #:macroexpand
           #:with-compilation-unit)
  (:export #:compile-into #:compile #:eval #:eval-progn)
  (:export #:with-compilation-unit #:with-compilation-results)
  ;; Compiler guts - used in cmpltv
  (:export #:add-declarations #:lexenv-for-macrolet
	   #:make-null-lexical-environment
	   #:make-local-macro #:make-symbol-macro
	   #:add-macros #:add-symbol-macros
           #:compute-macroexpander
           #:macroexpand-1 #:macroexpand #:constantp)
  (:export #:run-time-environment)
  (:export #:ltv-info #:ltv-info-form #:ltv-info-read-only-p)
  (:export #:fdefinition-info #:fdefinition-info-name)
  (:export #:value-cell-info #:value-cell-info-name)
  (:export #:constant-info #:constant-info-value)
  (:export #:env-info)
  (:export #:cmodule #:make-cmodule #:cmodule-literals #:link)
  (:export #:cfunction #:cfunction-cmodule #:cfunction-nlocals
           #:cfunction-closed #:cfunction-final-entry-point
           #:cfunction-final-size
	   #:cfunction-name #:cfunction-doc
           #:cfunction-lambda-list #:cfunction-lambda-list-p)
  ;; Conditions and compilation unit handling
  (:export #:with-compilation-unit #:with-compilation-results)
  (:export #:compiler-condition #:source)
  (:export #:unknown-reference #:unknown-variable #:unknown-function
           #:name
           #:unknown-reference-resolution #:resolve-reference
           #:resolve-function #:resolve-macro
           #:assumed-function-now-macro)
  ;; PC map info related stuff
  (:export #:*source-locations*))
