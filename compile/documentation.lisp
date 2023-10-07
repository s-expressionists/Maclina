(in-package #:maclina.compile)

(setf (documentation (find-package "MACLINA.COMPILE") t)
      "Package for Lisp compiler and evaluator. Main entry points are COMPILE and EVAL."
      (documentation 'eval 'function)
      "As CL:EVAL. Evaluate FORM in the compilation ENVIRONMENT and the current dynamic environment."
      (documentation 'eval-progn 'function)
      "Evaluate FORMS as a progn in the compilation ENVIRONMENT. This is provided as a convenience function, because it works regardless of how CL:PROGN is bound in ENVIRONMENT, if at all."
      (documentation 'compile 'function)
      "Compile LAMBDA-EXPRESSION into a function in the compilation ENVIRONMENT.
Returns three values: FUNCTION, WARNINGSP, FAILUREP, which are the same as those of CL:COMPILE. FUNCTION may or may not be considered a COMPILED-FUNCTION by the host - this is out of Maclina's control.
Unlike CL:COMPILE, COMPILE does not accept a function name, and does not alter bindings to install the newly compiled function."
      (documentation 'macroexpand-1 'function)
      "As CL:MACROEXPAND-1. Uses Trucler to interrogate for macro definitions, so it is (probably) more flexible than the host function."
      (documentation 'macroexpand-1 'function)
      "As CL:MACROEXPAND. Uses Trucler to interrogate for macro definitions, so it is (probably) more flexible than the host function."
      (documentation 'make-null-lexical-environment 'function)
      "Create a compiler lexenv given a global compilation environment. This can be used for local augmentations and passed as an argument to COMPILE, etc.

See ADD-DECLARATIONS
See ADD-MACROS
See ADD-SYMBOL-MACROS"
      (documentation 'lexenv-for-macrolet 'function)
      "Given a compiler lexenv, return a lexenv stripped of runtime information. This is suitable for passing to COMPUTE-MACROEXPANDER and is used for compiling MACROLET forms correctly.

See COMPUTE-MACROEXPANDER"
      (documentation 'compute-macroexpander 'function)
      "Given the definition of a local macro - its name, lambda list, body, and the environment it's being compiled in - return an actual function to use as a macroexpander. The environment should be one returned by LEXENV-FOR-MACROLET.

See LEXENV-FOR-MACROLET
See ADD-MACROS"
      (documentation 'make-local-macro 'function)
      "Given a macro name and expander function, return a compiler information structure it can use to understand the MACROLET binding. This structure is usable in concert with ADD-MACROS.

See COMPUTE-MACROEXPANDER
See ADD-MACROS"
      (documentation 'make-symbol-macro 'function)
      "Given a symbol and its macroexpansion, return a compiler information structure which it can use to understand the SYMBOL-MACROLET binding. This structure is usable in concert with ADD-SYMBOL-MACROS.

See ADD-SYMBOL-MACROS"
      (documentation 'add-macros 'function)
      "Augment a compiler lexenv with new macro definitions. The new definitions should be an alist, with names for cars and macro information objects (returned by MAKE-LOCAL-MACRO) for cdrs.

See MAKE-LOCAL-MACRO"
      (documentation 'add-symbol-macros 'function)
      "Augment a compiler lexenv with new symbol macro definitions. The new definitions should be an alist, with names for cars and symbol macro information objects (returned by MAKE-SYMBOL-MACRO) for cdrs.

See MAKE-SYMBOL-MACRO"
      (documentation 'compile-into 'function)
      "Compile LAMBDA-EXPRESSION in the compilation ENVIRONMENT into a new compiler function within MODULE. The compiler function information is returned, but is not loaded into being an actual callable function. COMPILE-INTO underlies all other compilation operations and can be used for lower level purposes, such as getting a compiler function to serialize during COMPILE-FILE.
If DECLARATIONS is provided, the body of the lambda expression is treated as if it was in a PROGN, so declaration expressions are not permitted (and will be treated as forms). The provided DECLARATIONS are used instead of any in the body.
If BLOCK-NAME is provided, a block of that name will be established around the body of the lambda expression.
 These options are available so that functions can be compiled with implicit blocks or progns regardless of the compilation environment's bindings of CL:PROGN or CL:BLOCK.

See CMODULE"
      (documentation 'link 'function)
      "Finish compilation of a module by resolving all temporary labels into actual positions. Return the final bytecode for the module."
      (documentation 'with-compilation-results 'function)
      "Evaluate BODY as a progn, and then return values analogous to those returned by COMPILE. Specifically, three values are returned:
* the primary value of BODY
* warningsp, which is true iff evaluation of BODY signaled any unhandled warnings
* failurep, which is true iff evaluation of BODY signaled any unhandled warnings or errors, other than style warnings
COnditions may be handled from outside the body of WITH-COMPILATION-RESULTS. Handling a warning with MUFFLE-WARNING is not sufficient to prevent WITH-COMPILATION-RESULTS from noting it: you would need a different restart."
      (documentation 'with-compilation-unit 'function)
      "As CL:WITH-COMPILATION-UNIT. Evaluate BODY as a progn and return its values. Within the dynamic environment of WITH-COMPILATION-UNIT, actions deferred by the compiler until the end of compilation will be deferred to the end of the outermost WITH-COMPILATION-FORM (or the innermost with :override true).
On exit, this outermost WITH-COMPILATION-FORM will print to *ERROR-OUTPUT* a summary of conditions signaled during evaluation, if there were any.
For now in Maclina, these actions are only signaling certain kinds of warnings relating to unknown names."
      (documentation 'run-time-environment 'function)
      "The compiler calls this function to get the run time environment for a given compilation environment. Clients should customize it in accordance with their environments' structures."
      (documentation 'resolve-reference 'function)
      "This function can be called by WITH-COMPILATION-UNIT's expansion when it receives an UNKNOWN-REFERENCE-RESOLUTION signal. The function is called with the resolution condition and any unresolved condition. If the resolution resolves the given condition, T should be returned. Other effects are permissible, such as signaling warnings."
      (documentation 'cfunction 'type)
      "A compiler function, i.e. compiler information about an eventual function. After being produced by COMPILE-INTO, contains enough information to make an actual function out of. Compiler functions are used by the file compiler.

See COMPILE-INTO"
      (documentation 'cfunction-cmodule 'function)
      "Return a compiler function's module.

See CFUNCTION
See CMODULE"
      (documentation 'cfunction-nlocals 'function)
      "Return the number of local variables bound within a compiler function.

See CFUNCTION"
      (documentation 'cfunction-final-entry-point 'function)
      "Return the computed entry point for a compiler function. This is an index into the bytecode indicating the first instruction to be executed when the eventual function is called.

See CFUNCTION
See LINK"
      (documentation 'cfunction-final-size 'function)
      "Return the size in bytes of the bytecode corresponding to a compiler function.

See CFUNCTION"
      (documentation 'cmodule 'type)
      "A compiler module. Represents a collection of functions being compiled together; for example local functions defined by FLET or LAMBDA will share a module with their parent function.

See MAKE-CMODULE
See CMODULE-LITERALS
See CFUNCTION
See COMPILE-INTO"
      (documentation 'make-cmodule 'function)
      "Create a new, empty compiler module. Functions can be compiled into the module with COMPILE-INTO.

See CMODULE"
      (documentation 'cmodule-literals 'function)
      "Retrieve the literals vector of a CMODULE. This is a vector containing information about constants, LOAD-TIME-VALUE forms, etc. present in compiled code.

See CMODULE"
      (documentation 'link 'function)
      "Finish compilation of the MODULE, computing the final, linked bytecode. Does not load constants.

See CMODULE")
