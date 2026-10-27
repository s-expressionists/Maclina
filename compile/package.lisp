(defpackage #:maclina.compile
  (:use #:cl)
  (:local-nicknames (#:m #:maclina.machine)
                    (#:arg #:maclina.argparse))
  (:shadow #:compile #:eval #:constantp #:macroexpand-1 #:macroexpand
           #:with-compilation-unit)
  (:export #:compile-into #:compile #:eval #:eval-progn)
  (:export #:with-compilation-unit #:with-compilation-results)
  ;; Declaration handling
  (:export #:process-declaration #:process-declarations
           #:process-binding-declaration
           #:process-variable-declaration #:process-function-declaration)
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
  (:export #:named-constant-info #:named-constant-info-name)
  (:export #:env-info)
  (:export #:cmodule #:make-cmodule #:cmodule-literals #:link)
  (:export #:cfunction #:cfunction-cmodule #:cfunction-nlocals
           #:cfunction-closed #:cfunction-final-entry-point
           #:cfunction-final-size
	   #:cfunction-name #:cfunction-doc #:cfunction-source
           #:cfunction-lambda-list #:cfunction-lambda-list-p)
  ;; Conditions and compilation unit handling
  (:export #:with-compilation-unit #:with-compilation-results)
  (:export #:compiler-condition #:source)
  (:export #:unknown-reference
           #:unknown-variable #:unknown-function #:unknown-type
           #:name
           #:unknown-reference-resolution #:resolve-reference
           #:resolve-function #:resolve-macro #:resolve-type
           #:assumed-function-now-macro)
  ;; Saved functions, for use in inlining
  (:export #:saved-function #:save-function
           #:saved-function-module
           #:saved-function-nlocals #:saved-function-environment-size
           #:saved-function-entry-pc #:saved-function-size
           ;; FIXME: merge with cfunction-name etc, use methods
           #:saved-function-name #:saved-function-doc #:saved-function-source
           #:saved-function-lambda-list #:saved-function-lambda-list-p
           #:saved-function-lookup-name
           #:saved-module
           #:saved-module-bytecode #:saved-module-literals #:saved-module-pc-map)
  ;; Miscellaneous hooks
  (:export #:debug-lambda-name #:debug-lambda-list)
  (:export #:load-map-info)
  ;; PC map info related stuff
  (:export #:*source-locations* #:*default-source-location*
           #:form-source-location #:default-source-location))
