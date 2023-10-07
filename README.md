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

# Subsystems

Maclina defines a variety of subsystems that can be loaded independently. It's set up this way so that you can, for example, load one of the VM definitions and run bytecode compiled elsewhere, without needing to load any of the compiler's multitudinous dependencies.

* `maclina/base` is the base system. Everything depends on `maclina/base`. `maclina/base` defines various shared conditions, the MOP magic that lets bytecode functions be run in a host Lisp,the names of instructions, and the disassembler.
* `maclina/compile` turns Lisp forms into bytecode. You need it in order to compile or evaluate forms. But this alone won't let you run bytecode; you'll need one of the VM systems for that. And Lisp compilation frequently involves evaluation, so you'll probably need to load a VM before you can compile much of anything.
* `maclina/compile-file` implements the file compiler. It depends on the compiler in `maclina/compile` to do that.
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
