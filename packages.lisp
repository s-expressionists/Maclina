(defpackage #:maclina.machine
  (:use #:cl)
  (:shadow #:return #:throw #:symbol-value #:progv #:fdefinition #:nil #:eq
           #:set #:push #:pop)
  (:shadow #:boundp #:makunbound #:fboundp #:fmakunbound)
  (:shadow #:lambda-parameters-limit #:call-arguments-limit
           #:lambda-list-keywords #:multiple-values-limit)
  ;; Additional opname exports are done in machine.lisp.
  (:export #:*client*)
  (:export #:bytecode-module #:make-bytecode-module
           #:bytecode-module-bytecode #:bytecode-module-literals)
  (:export #:bytecode-function #:make-bytecode-function
           #:bytecode-function-module #:bytecode-function-entry-pc
           #:bytecode-function-size
           #:bytecode-function-environment-size
           #:bytecode-function-locals-frame-size
           #:bytecode-function-name #:bytecode-function-lambda-list)
  (:export #:bytecode-closure #:make-bytecode-closure
           #:bytecode-closure-template #:bytecode-closure-env)
  (:export #:compute-instance-function)
  (:export #:link-function #:link-variable #:link-environment)
  (:export #:boundp #:makunbound #:symbol-value #:call-with-progv #:progv
	   #:fdefinition #:fmakunbound #:fboundp)
  (:export #:lambda-parameters-limit #:call-arguments-limit
           #:lambda-list-keywords #:multiple-values-limit)
  ;; PC map stuff
  (:export #:bytecode-module-pc-map
           #:map-info #:start #:end
           #:source-info #:source)
  (:export #:program-structure-info
           #:declarations-info #:the-info #:if-info
           #:tagbody-info #:block-info
           #:receiving #:declarations #:the-type #:tags #:name)
  (:export #:vars-info #:bindings #:var-info #:index #:cellp)
  ;; A few shared conditions
  (:export #:unknown-opcode #:unknown-long-opcode))

(defpackage #:maclina.introspect
  (:use #:cl)
  (:shadow #:disassemble)
  (:local-nicknames (#:m #:maclina.machine))
  (:export #:disassemble #:display-instruction)
  (:export #:info-at #:most-specific-info-at #:source-at #:function-at))
