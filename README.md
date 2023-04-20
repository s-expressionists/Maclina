Lisp implementation of Common Lisp VM used in Clasp and possibly for other purposes.

Specification is at https://github.com/clasp-developers/clasp/wiki/Virtual-machine-design

# Design goals

* Quick compilation of CL code (in more-or-less one pass)
* Reasonably quick execution of the bytecode
* Compatibility with native code, i.e. VM functions can call native functions and vice versa, with practical performance

# Use

Load the `cvm` ASDF system.

Before compiling or evaluating code, you should probably set the client in order to inform Trucler how to get global definitions. On SBCL you can use the host environment as follows:

```lisp
(setf cvm.compile:*client* (make-instance 'trucler-native-sbcl:client))
```

The procedure on CCL is analogous. Or, you can use some other trucler client and environment, such as Trucler's reference implementation.

Now you can compile code with `cvm.compile:compile` and disassemble it with `cvm.machine:disassemble`:

```lisp
(defvar *f* (cvm.compile:compile '(lambda (x) (let ((y 5)) (print y) #'(lambda () (+ y x))))))
(cvm.machine:disassemble *) ; =>
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
  make-closure '#<CVM.MACHINE:BYTECODE-FUNCTION {100C2D803B}>
  pop
  return
  check-arg-count-<= 0
  fdefinition '+
  closure 0
  closure 1
  call 2
  return
; No value
```

To actually run code, first set up a stack for the vm with `(cvm.vm:initialize-vm N)`, where N is how many objects the stack will be able to hold, say 20000. Then you can simply call the functions returned by `compile`:

```lisp
(funcall *f* 5) ; =>
5
#<CVM.MACHINE:BYTECODE-CLOSURE>
```

You can get a running trace of the machine state by binding `cvm.vm:*trace*` to true around a call:

```lisp
(let ((cvm.machine:*trace* t)) (funcall *f* 3)) ; =>
((CVM.MACHINE:CHECK-ARG-COUNT-= NIL (:OPERAND 1)) 10 12 #(3 5) #())
((CVM.MACHINE:BIND-REQUIRED-ARGS NIL (:OPERAND 1)) 10 12 #(3 5) #())
((CVM.MACHINE:CONST NIL (:CONSTANT 0)) 10 12 #(3 5) #())
((CVM.MACHINE:SET NIL (:OPERAND 1)) 10 13 #(3 5) #(5))
((CVM.MACHINE:FDEFINITION NIL (:CONSTANT 1)) 10 12 #(3 5) #())
((CVM.MACHINE:REF NIL (:OPERAND 1)) 10 13 #(3 5) #(#<FUNCTION PRINT>))
((CVM.MACHINE:CALL NIL (:OPERAND 1)) 10 14 #(3 5) #(#<FUNCTION PRINT> 5))
5
((CVM.MACHINE:REF NIL (:OPERAND 1)) 10 12 #(3 5) #())
((CVM.MACHINE:REF NIL (:OPERAND 0)) 10 13 #(3 5) #(5))
((CVM.MACHINE:MAKE-CLOSURE NIL (:CONSTANT 3)) 10 14 #(3 5) #(5 3))
((CVM.MACHINE:POP NIL) 10 13 #(3 5) #(#<CVM.MACHINE:BYTECODE-CLOSURE {100C2D80CB}>))
((CVM.MACHINE:RETURN NIL) 10 12 #(3 5) #())
#<CVM.MACHINE:BYTECODE-CLOSURE {100C2D80CB}>
```

# Implementation status

Works. Except:

* `unwind-protect` is not implemented.
* A bespoke environment structure is used rather than host environments. As such, macros that actually access their environment (e.g. `setf`) will not work.

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
* Better syntax errors (required for serious use as a frontend)
