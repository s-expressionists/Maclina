(defpackage #:maclina.machine
  (:use #:cl)
  (:shadow #:return #:throw #:symbol-value #:progv #:fdefinition #:nil #:eq
           #:set #:push #:pop)
  (:shadow #:boundp #:makunbound #:fboundp #:fmakunbound)
  (:shadow #:lambda-parameters-limit #:call-arguments-limit
           #:lambda-list-keywords #:multiple-values-limit)
  (:shadow #:function)
  (:shadow #:find-class #:find-package)
  ;; Additional opname exports are done in machine.lisp.
  (:export #:*client*)
  (:export #:module #:make-module #:bytecode #:literals #:pc-map)
  (:export #:function #:make-function
           #:entry-pc #:size #:environment-size #:locals-frame-size
           #:name #:lambda-list)
  (:export #:closure #:make-closure #:template #:environment)
  (:export #:compute-instance-function)
  (:export #:link-function #:link-variable #:link-environment
           #:find-class #:find-package)
  (:export #:boundp #:makunbound #:symbol-value #:call-with-progv #:progv
	   #:fdefinition #:fmakunbound #:fboundp)
  (:export #:lambda-parameters-limit #:call-arguments-limit
           #:lambda-list-keywords #:multiple-values-limit)
  ;; PC map stuff
  (:export #:map-info #:start #:end
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
  (:export #:info-at #:most-specific-info-at #:source-at #:function-at)
  (:export #:map-instructions #:do-instructions
           #:map-annotated-instructions #:map-annotated-instructions-literals
           #:delay))
