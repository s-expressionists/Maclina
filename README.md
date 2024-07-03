Maclina is an implementation of a Common Lisp evaluator, compiler, file compiler, and FASL loader, all written in portable Common Lisp. It compiles Lisp into a bytecode representation, which is interpreted by a simple virtual machine. The compiler is complete but simple, performing only easy optimizations; this makes it fast, and suited for code that does not necessarily need to run quickly, such as that evaluated just once.

Compilation and bytecode interpretation take place relative to specified environments. [Clostrum](https://github.com/s-expressionists/Clostrum) can be used to build a first-class environment to execute code in, meaning Maclina can be used to execute code in an isolated environment, like a sandbox.

Bytecode functions exist as real functions, so they can be called the same as any other functions. Bytecode functions and native functions can coexist without difficulty and call each other.

The FASL format is simple and portable. Lisp source files can be compiled in one implementation and then loaded it into another, as long as the compilation and loading environments agree.

# Quick start

Load the `maclina` ASDF system. There is a dependency on Clostrum, which is not available on Quicklisp, so you'll need to set that up yourself.

Before compiling or evaluating code, you need to set the client in order to inform Trucler how to get global definitions. On SBCL you can use the host environment as follows:

```lisp
(setf maclina.machine:*client* (make-instance 'trucler-native-sbcl:client))
```

The procedure on CCL is analogous. Or, you can use some other trucler client and environment, such as Trucler's reference implementation.

Now you can compile code with `maclina.compile:compile` and disassemble it with `maclina.machine:disassemble`:

```lisp
(defvar *f* (maclina.compile:compile '(lambda (x) (let ((y 5)) (print y) #'(lambda () (+ y x))))))
(maclina.machine:disassemble *f*) ; =>
---module---
  check-arg-count-= 1
  bind-required-args 1
  const '5
  set 1
  fdefinition 'PRINT
  ref 1
  call 1
  ref 1
  ref 0
  make-closure '#<MACLINA.MACHINE:BYTECODE-FUNCTION {100C2D803B}>
  pop
  return
; No value
```

To actually run code, first set up a stack for the VM with `(maclina.vm-native:initialize-vm N)`, where N is how many objects the stack will be able to hold, say 20000. Then you can simply call the functions returned by `compile`:

```lisp
(funcall *f* 5) ; =>
5
#<MACLINA.MACHINE:BYTECODE-CLOSURE>
```

You can get a running trace of the machine state by binding `maclina.vm-native:*trace*` to true around a call:

```lisp
(let ((maclina.vm-native:*trace* t)) (funcall *f* 3)) ; =>

  check-arg-count-= 1 ; bp 1 sp 3 locals #(0 0) stack #()
  bind-required-args 1 ; bp 1 sp 3 locals #(0 0) stack #()
  const '5 ; bp 1 sp 3 locals #(3 0) stack #()
  set 1 ; bp 1 sp 4 locals #(3 0) stack #(5)
  fdefinition 'PRINT ; bp 1 sp 3 locals #(3 5) stack #()
  ref 1 ; bp 1 sp 4 locals #(3 5) stack #(#<FUNCTION PRINT>)
  call 1 ; bp 1 sp 5 locals #(3 5) stack #(#<FUNCTION PRINT> 5)

5
  ref 1 ; bp 1 sp 3 locals #(3 5) stack #()
  ref 0 ; bp 1 sp 4 locals #(3 5) stack #(5)
  make-closure '#<MACLINA.MACHINE:BYTECODE-FUNCTION NIL> ; bp 1 sp 5 locals #(3 5) stack #(5 3)
  pop ; bp 1 sp 4 locals #(3 5) stack #(#<MACLINA.MACHINE:BYTECODE-CLOSURE NIL>)
  return ; bp 1 sp 3 locals #(3 5) stack #()

#<MACLINA.MACHINE:BYTECODE-CLOSURE {100C2D80CB}>
```

# First-class environments

The `maclina/vm-cross` subsystem allows Maclina to be used for compiling and running Lisp code in arbitrary first-class environments, in concert with Clostrum. Here is an example:

```lisp
;;; maclina-cross does not itself load a global environment implementation,
;;; since it can be used with any. Here we use clostrum-basic for that.
;;; We also need clostrum-trucler to be able to compile relative to
;;; a Clostrum environment.
(ql:quickload '(:clostrum-basic :clostrum-trucler))

;;; Set up the client to use maclina-cross, and initialize the VM.
(setf maclina.machine:*client* (make-instance 'maclina.vm-cross:client))
(maclina.vm-cross:initialize-vm 20000)

;;; Construct our environments.
(defvar *rte* (make-instance 'clostrum-basic:run-time-environment))
(defvar *env* (make-instance 'clostrum-basic:compilation-environment
                 :parent *rte*))

;;; These new environments are totally devoid of bindings.
;;; To do anything useful, we have to define at least a few.
;;; We'll define + and *readtable* weirdly to emphasize that we are
;;; not operating in the host environment.
(setf (clostrum:fdefinition maclina.machine:*client* *rte* '+) #'-)
(clostrum:make-variable maclina.machine:*client* *rte* '*readtable* 17)

;;; Now behold:
(maclina.compile:eval '(+ *readtable* 3) *env*) ; => 14
;;; And of course, the host *READTABLE* and + are unaffected.
```

## Sandboxing

A more complete CL experience requires a richer environment. The [Extrinsicl](https://github.com/s-expressionists/Extrinsicl) project can be used to construct such an environment, but more generally you can just fill a Clostrum environment. Extrinsicl can additionally be configured to provide functions like `eval` through Maclina.

For real sandboxing of untrusted code, you will need a "safe" environment lacking any undesirable operators. What is undesirable depends on your application, but might include, for instance, file I/O. If your environment is well constructed, you don't need to worry about functions that carry out evaluation or introspection in themselves, because they will only operate with respect to your safe environment.

Another danger of untrusted code is it not halting, which can be a denial of service attack. The cross VM has a `with-timeout` macro that can be used to abort evaluation after executing some number of VM instructions. This covers all evaluations within the cross VM, including indirectly as from calls to VM functions. Note that computations outside of the VM are not tracked, so for example there will be no abort if the untrusted code calls a non-VM function that does not halt.

Here is an example of a basic sandbox:

```lisp
(ql:quickload '(:clostrum-basic :extrinsicl :extrinsicl/maclina :maclina))

;;; Set up Maclina.
(setf maclina.machine:*client* (make-instance 'maclina.vm-cross:client))
(maclina.vm-cross:initialize-vm 20000)

;;; Create the (empty) environment.
(defvar *rte* (make-instance 'clostrum-basic:run-time-environment))
(defvar *env* (make-instance 'clostrum-basic:compilation-environment
                :parent *rte*))

;;; Install most of CL.
(extrinsicl:install-cl maclina.machine:*client* *rte*)
(extrinsicl.maclina:install-eval maclina.machine:*client* *rte*)

;;; Uninstall filesystem access.
(loop for f in '(open directory probe-file ensure-directories-exist truename
                 file-author file-write-date rename-file delete-file)
      do (clostrum:fmakunbound maclina.machine:*client* *rte* f))

;;; Also add a trap.
(setf (clostrum:fdefinition maclina.machine:*client* *rte* 'o)
      (lambda (&rest args) (apply #'open args)))

;;; Try it out.
(maclina.compile:eval '(+ 2 7) *env*) ;=> 9
(defparameter *fib*
  (maclina.compile:compile
   '(lambda (n)
      (loop for a = 0 for b = 1
            repeat n
            do (psetf a b b (+ a b))
            finally (return a)))
   *rte*))
(funcall *fib* 37) ;=> big number

;;; But we can't access the filesystem.
(maclina.compile:eval '(open "/tmp/hello.txt") *env*)
;=> error: UNDEFINED-FUNCTION OPEN

;;; Tricky stuff is available but doesn't help escape.
(maclina.compile:eval '(eval 'pi) *env*) ;=> pi
(maclina.compile:eval `(funcall ,*fib* 37) *env*) ;=> big number
(maclina.compile:eval '(find-symbol "OPEN") *env*) ;=> OPEN
(maclina.compile:eval '(eval (list (find-symbol "OPEN") "/tmp/hello.txt")) *env*)
;=> error: UNDEFINED-FUNCTION OPEN

;;; Whoops, we forgot WITH-OPEN-FILE. But that's okay.
(maclina.compile:eval '(with-open-file (s "/tmp/hello.txt")) *env*)
;=> error: UNDEFINED-FUNCTION OPEN

;;; But the VM can't intercept a function call within a host function.
(maclina.compile:eval '(o "/tmp/hello.txt") *env*) ;=> actually opens

;;; DoS denied.
(maclina.vm-cross:with-timeout (1000000)
  (maclina.compile:eval '(loop) *env*))
;=> error: TIMEOUT

;;; Watch out for more exotic DoS outside of the VM, though.
(maclina.vm-cross:with-timeout (100000)
  (maclina.compile:compile '(lambda () (progn . #1=(nil . #1#))) *env*))
; => compiler hangs
(maclina.vm-cross:with-timeout (100000)
  (maclina.compile:eval '(typep 17 '#1=(not #1#)) *env*))
; => hang or stack overflow
```

# Subsystems

Maclina defines a variety of subsystems that can be loaded independently. It's set up this way so that you can, for example, load one of the VM definitions and run bytecode compiled elsewhere, without needing to load any of the compiler's multitudinous dependencies.

* `maclina/base` is the base system. Everything depends on `maclina/base`. `maclina/base` defines various shared conditions, the MOP magic that lets bytecode functions be run in a host Lisp,the names of instructions, and the disassembler.
* `maclina/compile` turns Lisp forms into bytecode. You need it in order to compile or evaluate forms. But this alone won't let you run bytecode; you'll need one of the VM systems for that. And Lisp compilation frequently involves evaluation, so you'll probably need to load a VM before you can compile much of anything.
* `maclina/compile-file` implements the file compiler. It depends on the compiler in `maclina/compile` to do that.
* `maclina/vm-shared` is an internal system containing some code shared by the VM implementations.
* `maclina/vm-native` is the "native" implementation of the VM, which is to say that it operates entirely in the host Lisp's normal global environment. This is simple but a bit inflexible.
* `maclina/vm-cross` is an implementation of the VM that operates relative to a Clostrum environment. This is what you want to do anything first-class-environment-related.
* `maclina/load` loads FASL files created by `maclina/compile-file`. `maclina/load` and one of the VMs is sufficient to load and run FASLs.
* For general use and convenience, the `maclina` oversystem just loads everything.

Assuming the compilation and loader environments match (e.g. any function appearing in a macroexpansion is actually available in the load-time environment), there is no problem with compiling code with one VM and loading it with another. Using multiple VMs in the same image also works.

# Implementation status

Works. Except:

* A bespoke environment structure is used rather than host environments. As such, imported host macros that actually access their environment (e.g. `setf`) will not work.

## More TODO

* VM optimizations
  * Profile
  * Values
  * Arguments
  * Maybe elide some array bounds checks when safe, but keep a guard on overflowing the stack
* Compiler optimizations
  * Entirely elide unused blocks (i.e. don't even do `save-sp`)
  * Use `restore-sp` + `jump` for local exits to blocks even if they are also nonlocally exited to
  * Optimize some degenerate forms, like `(block nil)` => `nil`
  * Inline lambda forms
  * Inline functions more generally
  * Contify?
  * Use multiple-value contexts for `multiple-value-call` with a lambda
* Instructions for inline operations like `car`, possibly
