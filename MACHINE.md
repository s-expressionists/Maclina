This file defines the virtual machine used by Maclina, as implemented in vm-native.lisp and vm-cross.lisp.

The virtual machine executes a bytecode. Each instruction is encoded as a one byte opcode followed by a fixed number of operands. All operands are one byte unless the `long` opcode described below is used.

The virtual machine is abstract. It is not necessary to implement with the mechanisms given in this description, as long as the implementation behaves _as if_ it operated like the machine described here. For example, it would be acceptable to implement the "virtual machine" by compiling bytecode into native code with the same effects, but with none of the stacks or the rest of the machine state actually existent at runtime.

# Structures

Bytecode is organized into modules. A module contains the bytecode for one or more functions. It also contains a vector of _literals_ accessed throughout its bytecode. Literals include constants appearing in the code, load time values, _function cells_, and _variable cells_.

Function cells and variable cells are implementation-defined objects that represent global bindings in some environment. A function or variable cell has an associated name; when the value bound to that environment's function or variable binding (respectively) of that name changes, or when the binding is made unbound, the cell reflects the change. When a bytecode module is loaded, the loader defines what environment it is loading into, and all function cells and variable cells are for bindings in this one environment.

Bytecode functions can be either closures or "templates". A closure is made up of a "template" and a closure vector. Maclina uses flat closures, so closures do not need to maintain a chain of environments. Each element of the closure vector is either a value or a _cell_ (distinct from function and variable cells). A cell is an object that holds a value and may have that value changed; cells are used when a function can mutate lexical variables within another closure.

All other information about a function is part of the template. Bytecode function templates contain the following information:

* The module it is a member of.
* A count of simultaneously bound local variables.
* A count of how many closure values and cells a function with this template has.
* An entry point: the index, into the module's bytecode, of the function's first instruction.

If a template has a closure count of zero, it is itself callable as a function. This allows the machine to skip allocation in the common case of non-closure functions. Templates that do need closures are never directly accessible in normal operation; they are only used for making closures.

# Operation

The virtual machine has the following state:

* The _instruction pointer_, `ip` indicating the current instruction.
* A _literals vector_, `literals`, as described above for modules.
* A _values vector_, `values`, containing values returned by a function call.
* The function's _closure vector_, `closure`.
* A _locals vector_, `locals`, containing values bound by local lexical variables.
* An _arguments vector_, `arguments`, containing the arguments the function was called with.
* A _stack_, `stack`, of values.
* A _variadic argument stack_, `varargs`, used for variadic calls (multiple value calls).
* A _dynamic environment stack_, `destack`, used to track nonlocal exit points, dynamic variable bindings, and `unwind-protect` cleanups.

All of these except the instruction pointer and dynamic environment stack are (at least conceptually) local to a given function call. Execution of one function for a call never requires interrogating the stack of another call, for example. `values` are sometimes transferred from one call to another, but only when the transferring call is exiting. When one bytecode function calls another, this state must be saved for the duration of the call, and restored when the callee returns.

Each thread has its own dynamic environment stack, which is initialized to empty before any bytecode is executed in that thread.

`literals`, `closure`, `locals`, and the `stack`, have a fixed size for any given function. A valid bytecode program never reads beyond these static bounds, and so bounds checks are unnecessary at runtime. `arguments` must hold at most `lambda-parameters-limit` elements, and `values` at most `multiple-values-limit` elements. `arguments` is unchanged during any given function call, and no operation can read beyond its limits. `values` have fluctuating sizes, but no operation can read beyond their limits. `destack` does not have any defined size limit. `varargs` is complicated; see below.

Virtual machine operation begins as soon as a bytecode function is called. To call a function, the virtual machine is initialized with the function's closure vector in `closure`, the arguments to the call in `arguments`, the function's module's literals in `literals`, and an empty `stack` and `varargs`. `values` does not have to be initialized; any bytecode function that reads from `values` before writing to it is ill formed. Once this initialization is done, `ip` is set to the function's entry point, and the bytecode begins executing instructions from there.

## varargs

`varargs` is used to accumulate arguments for a variadic call, from e.g. `cl:multiple-value-call` or `cl:apply`. It is possible (if rare) for execution to need to accumulate multiple lists of arguments simultaneously, due to code like `(multiple-value-call f (foo) (multiple-value-call g ...) ...)`; therefore `varargs` is presented here as a stack of sequences. Each sequence must be able to hold at most `lambda-parameters-limit` arguments. For any given function, the depth of the stack (i.e. the number of sequences live at any given time) is finite, statically apparent, and probably very small. It is never necessary to reach back into the `varargs` stack to access earlier sequences while accumulating another.

The `stack` and `varargs` are never used simultaneously. That is to say that in valid bytecode, `stack` is not popped when the last push was to `varargs`, and vice versa. It is therefore possible for an implementation to combine both stacks into one. They are presented separately here to emphasize that operations on one cannot be validly interleaved with operations on the other. The simple implementation of this is pushing the values to the (shared) stack followed by a number indicating how many values have been accumulated so far.

## Dynamic environments

The virtual machine tracks a current dynamic environment stack in each thread. This is the Lisp dynamic environment described in CLHS 3.1.1.2, containing dynamic variable bindings, catch tags, and exit points. (The virtual machine does not implement handlers and restarts directly.)

The stack contains objects representing parts of the dynamic environment. There are five kinds of dynamic environment objects: entries (representing exit points), catches (for `cl:catch`), single dynamic variable bindings, multiple dynamic variable bindings (for `cl:progv`), and protection dynenvs (`cl:unwind-protect` cleanups).

The dynamic environment stack can be added to during calls, and within a call entries may be popped off, but a call will never pop the dynamic environment stack back farther than where it was when the call began. The exception is during nonlocal exits, in which the dynamic environment stack will be unwound.

The CLHS specifies in 5.2 that during unwinding, it is illegal to transfer to an exit point within the extent of the ultimate destination. For example, `(block a (block b (unwind-protect (return-from a) (return-from b))))` is invalid: it tries to return to `a`, but the `unwind-protect` intercepts with a return to `b`, which is within `a`. It does _not_ specify that doing so is an error. 5.2 also specifies that the effects are undefined if an attempt is made to transfer to an exit point whose dynamic extent has ended. The virtual machine, similarly, does not specify what happens in either situation, so that it may be used in concert with existing Lisp implementations. Any safe implementation of the virtual machine should detect these situations and signal a control error, but this is not required.

It is worth emphasizing again that the presentation in this document is abstract, and that the virtual machine does not need to be implemented as described here, as long as it behaves the same. The presentation here uses actual objects as dynamic environment markers, and has an actual dynamic environment stack that is a normal list. It uses shallow binding of dynamic variables. None of these are necessary for an implementation.

## Instructions

Each instruction is presented as a mnemonic name and corresponding opcode in hexadecimal. Following this are zero or more parameters, indicated as `(name kind)`. The number of parameters for any given instruction is fixed, i.e. any instruction of a given opcode always has the same number of parameters. Each parameter is one byte, unless the `long` prefix (explained more below) is used, in which case each parameter is two bytes, representing a little-endian number. Parameters other than labels are interpreted as unsigned; labels are interpreted as signed (two's complement).

Kind can be `misc`, `literal`, `label`, or `keys`.

* `misc` means the interpretation is peculiar to the instruction.
* `literal` means that the parameter is an index into the `literals` vector the instruction loads this literal and uses it for its operation.
* `label` represents a relative instruction address. The label parameter is added to the current `ip` (that is, the index of the instruction's opcode) to get a new instruction pointer used by the instruction.
* `keys` is unique to `parse-key-args` and explained in the description of that instruction.

Each instruction is accompanied by Lisp pseudocode describing its effect. In this pseudocode, allcaps symbols are used to refer to the VM state components described above.

The following operations are used in this pseudocode:
* `(mv-vector form)` = `(coerce (multiple-value-list form) 'vector)`
* `(mv-list)` = `(coerce VALUES 'list)`
* `(gather n)` = `(nreverse (loop repeat n collect (pop STACK)))`, i.e. gather the previous N elements of the stack into a list.
* `(make-cell X)` makes a cell containing the value X.
* `(cell-ref cell)` is a place. Accessing it accesses the value of the cell.
* `(vcell-value vcell)` accesses a variable cell's value.
* `(fcell-function fcell)` accesses a function cell's function. `fcell-boundp` returns true iff the cell has a value.
* `make-closure` constructs a new function from the given template and sequence of closure values.
* `closure-size` returns the size of a closure vector for the given template.
* `closure-vector` accesses a closure's closure vector.
* `template` returns a closure's template.
* `(all-known-keywords-p plist keywords)` returns true if the plist has only those indicators in `keywords`, also handling `:allow-other-keys`.
* `(make-exit-point CLOSURE ARGUMENTS STACK VARARGS)` creates a new dynamic environment entry representing an `entry` instruction. `exit-closure`, `exit-arguments`, `exit-stack`, and `exit-varargs` read elements in the entry.
* `(make-catch-point tag destination LITERALS CLOSURE ARGUMENTS STACK VARARGS)` creates a new dynamic environment entry representing a `catch` instruction, i.e. a `cl:catch`. `catch-point-p` is a predicate for this type of entry. `catch-closure`, `catch-point-tag`, `catch-arguments`, `catch-stack`, `catch-varargs`, `catch-dest`, and `catch-literals` read elements in the entry.
* `(make-special-binding varible-cell value)` creates a new dynamic environment entry representing a single dynamic variable binding.
* `(make-progv-dynenv vcells values)` creates a new dynamic environment entry representing a `progv` binding of the vcells to the values.
* `(vcell-value vcell DESTACK)` accesses the binding of the variable cell in the given dynamic environment stack. When reading the value, if the vcell is unbound, an `unbound-variable` error is signaled. With the shallow binding used in this description, `vcell-value` would look through the `DESTACK` for any special binding or progv entries binding the variable, and if it didn't find any, would use the global binding.
* `(make-protection-dynenv thunk)` creates a new dynamic environment entry representing a cleanup, from the `protect` instruction. `protection-dynenv-thunk` reads the thunk.
* `(cleanup entry)` executes any cleanup actions required when unwinding a given dynamic environment entry. With the presentation here, the only required action is that `cleanup` of a protection dynenv will call its thunk. Around calling this thunk, `VALUES` is saved.

After any instruction that does not alter `ip`, `ip` is advanced to the next instruction (after the opcode and all of the parameters).

### ref #x00 (index misc)

Push the value of the `index`th local to the stack.

```lisp
(push (aref LOCALS index) STACK)
```

### const #x01 (index literal)

Push the value of the `index`th literal to the stack.

```lisp
(push (aref LITERALS index) STACK)
```

### closure #x02 (index misc)

Push the `index`th closure value or cell to the stack.

```lisp
(push (aref CLOSURE index) STACK)
```

### call #x03 (nargs misc)

Execute a function call and store its returned values in `values`. The last `nargs` values on the stack are arguments to the call, pushed left to right (i.e. the most recent stack value is the final argument, the next most recent is the penultimate argument, etc.). The function being called is on the stack before the arguments.

```lisp
(let ((arguments (gather nargs)))
  (setf VALUES (mv-vector (apply (pop STACK) arguments))))
```

### call-receive-one #x04 (nargs misc)

As `call`, but rather than receiving all values, just push the primary value to the stack. As per usual Lisp semantics, if the call returns zero values, `cl:nil` is pushed.

```lisp
(let ((arguments (gather nargs)))
  (push (apply (pop STACK) arguments) STACK))
```

### call-receive-fixed #x05 (nargs misc) (nvals misc)

As `call`, but receive `nvals` values and push them to the stack, left to right. As per usual Lisp semantics, missing values are treated as `cl:nil`.

```lisp
(let ((arguments (gather nargs)))
  (multiple-value-call (lambda (&rest vals)
  		         (loop repeat nvals
			       do (push (pop vals) STACK)))
    (apply (pop STACK) arguments)))
```

### bind #x06 (nvars misc) (base misc)

Pop `nvars` values off the stack, and assign them into `locals` starting at `base`. The first popped value goes to the last local.

```lisp
(loop for i from (+ base nvars -1) downto base
      do (setf (aref LOCALS i) (pop STACK)))
```

### set #x07 (base misc)

Pop one value from the stack and assign it into the `base`th `local`. `set base` is equivalent to `bind 1 base`.


```lisp
(setf (aref LOCALS base) (pop STACK))
```

### make-cell #0x08

Make a fresh cell, and assign its value to be a value popped from the stack. Push the cell to the stack.

```lisp
(push (make-cell (pop STACK)) STACK)
```

### cell-ref #x09

Pop a value from the stack; it is a cell. Push the value in the cell.

```lisp
(push (cell-ref (pop STACK)) STACK)
```

### cell-set #x0a

Pop a value from the stack; it is a cell. Pop another value from the stack, and set the cell's value to be that value.

```lisp
(setf (cell-ref (pop STACK)) (pop STACK))
```

### make-closure #x0b (template literal)

The `template`th literal is a function template with a closure size of `n`. Pop `n` values from the stack, and make a new closure with that template and the popped values as its closure vector. The first popped element is the last element in the vector, etc. Push the closure.

```lisp
(push (make-closure (aref LITERALS template) (gather (closure-size (aref LITERALS template)))) STACK)
```

### make-uninitialized-closure #x0c (template literal)

The `template`th literal is a function template. Allocate and push a new closure with that template and an appropriate closure size.

```lisp
(push (make-closure (aref LITERALS template) (make-array (closure-size (aref LITERALS template)))) STACK)
```

### initialize-closure #x0d (base misc)

The `base`th local is an uninitialized closure produced by `make-uninitialized-closure`. Mutate its closure vector to contain `n` values popped from the stack, where `n` is the closure's template's closure size.

```lisp
(setf (closure-vector (aref LOCALS base)) (gather (closure-size (template (aref LOCALS base)))))
```

### return #x0e

Return `values` from this function. Ends execution of this function.

```lisp
(return (mv-list))
```

### bind-required-args #x0f (nreq misc)

Set the first `nreq` locals to be the first `nreq` arguments.

```lisp
(replace LOCALS ARGUMENTS :end2 nreq)
```

### bind-optional-args #x10 (nreq misc) (nopt misc)

Push the `nopt` arguments beginning at `nreq` to the stack. If there are not enough arguments, an "unsupplied" value with no meaning except to `jump-if-supplied` is pushed instead.

```lisp
(loop for i from nreq below (+ nreq nopt)
      do (push (if (< (length ARGUMENTS) i)
	       	   (aref ARGUMENTS i)
		   +UNSUPPLIED+)
               STACK))
```

### listify-rest-args #x11 (nfixed misc)

Construct a list out of all the arguments beginning at `nfixed`, and push it to the stack.

```lisp
(push (nthcdr nfixed ARGUMENTS) STACK)
```


## parse-key-args #x13 (nfixed misc) (key-count-info misc) (keys keys)

The low 7 (or 15, for `long parse-key-args`) bits of `key-count-info` are a count of keywords, call it `nkeys`. There are `nkeys` literals beginning at `keys` that are keys. Interpret the arguments beginning with `nfixed` as a keyword plist, and push them to the stack in the order of the corresponding keywords. If any of these do not have an entry in the arguments plist, an "unsupplied" value with no meaning except to `jump-if-supplied` is pushed instead.

If the length of the argument plist is odd, signal a program error. If the high bit of `key-count-info` is unset, and there are keywords in the argument plist that are not part of `keys`, signal a program error.

```lisp
(let* ((plist (nthcdr NFIXED arguments))
       (nkeys (ldb (byte 7 #|or 15|# 0) key-count-info))
       (aokp (logbitp 7 #|or 15|# key-count-info))
       (keywords (subseq LITERALS keys (+ keys nkeys))))
  (unless (evenp (length plist)) (error 'program-error ...))
  (loop for i from base for kw in keywords
  	do (push (getf plist kw +UNSUPPLIED+) STACK))
  (unless (or aokp (all-known-keywords-p plist keywords))
    (error 'program-error ...)))
```

### jump-{8,16,24} #x14,#x15,#x16 (dest label)

Read the next 1, 2, or 3 bytes as a label. Unconditionally branch there.

```lisp
(incf IP label)
```

### jump-if-{8,16,24} #x17,#x18,#x19 (dest label)

Pop a value from the stack. If it is not `cl:nil`, jump to the label.

```lisp
(when (pop STACK) (incf IP label))
```

### jump-if-supplied-{8,16} #x1a #x1b (dest label)

Pop a value. If it is anything but the distinguished unsupplied value, push it back, then jump to the label.

```lisp
(let ((value (pop STACK)))
  (unless (eq value +UNSUPPLIED+) (push value STACK) (incf IP label)))
```

### check-arg-count-<= #x1c (nargs misc)

If the number of arguments is not less than or equal to `nargs`, signal a program error.

```lisp
(unless (<= (length ARGUMENTS) nargs) (error 'program-error ...))
```

### check-arg-count->= #x1d (nargs misc)

If the number of arguments is not greater than or equal to `nargs`, signal a program error.

```lisp
(unless (>= (length ARGUMENTS) nargs) (error 'program-error ...))
```

### check-arg-count-= #x1e (nargs misc)

If the number of arguments is not equal to `nargs`, signal a program error.

```lisp
(unless (= (length ARGUMENTS) nargs) (error 'program-error ...))
```

### push-values #x1f

Start a new `varargs` entry with `values`.

```lisp
(push VALUES VARARGS)
```

### append-values #x20

Continue an existing `varargs` entry with `values`.

```lisp
(setf (first VARARGS) (append (first VARARGS) (mv-list)))
```

### pop-values #x21

Pop a `varargs` entry into `values`.

```lisp
(setf VALUES (pop VARARGS))
```

### mv-call #x22

Pop a `varargs` entry. Pop a value from the stack; it is a function. Call the function with the popped varargs. Store the resulting values in `values`.

```lisp
(let ((args (pop VARARGS)))
  (setf VALUES (mv-vector (apply (pop STACK) args))))
```

### mv-call-receive-one #x23

Like `mv-call`, but push the primary value to the stack instead.

```lisp
(let ((args (pop VARARGS)))
  (push (apply (pop STACK) args) STACK))
```

### mv-call-receive-fixed #x24 (nvals misc)

Like `mv-call`, but push the first `nvals` values to the stack instead.

```lisp
(let ((arguments (pop VARARGS)))
  (multiple-value-call (lambda (&rest vals)
  		         (loop repeat nvals
			       do (push (pop vals) STACK)))
    (apply (pop STACK) arguments)))
```

### save-sp #x25 (base misc)

Save the current `stack` into the `base`th local. This is usable (only) by `restore-sp` during local unwind operations.

```lisp
(setf (aref LOCALS base) STACK)
```

### restore-sp #x26 (base misc)

Set stack from a saved stack in the `base`th local.

```lisp
(setf STACK (aref LOCALS base))
```

### entry #x27 (base misc)

Create a new exit point (as for `cl:block` or `cl:tagbody`). Push it to the dynenv stack, and also store it into the `base`th local.

```lisp
(let ((dynenv (make-exit-point CLOSURE ARGUMENTS STACK VARARGS)))
  (push dynenv DESTACK)
  (setf (aref LOCALS base) dynenv))
```

### exit-{8,16,24} #x28,29,2a (dest label)

Pop a value from the stack: this is an exit point (created by `entry`). Unwind to the exit point's frame, i.e. restore the VM state to where it was just after the `entry`, except that the `IP` is at `dest`, and `VALUES` are transmitted in their current state. Note that this means that after the exit, the exit point will still be on `destack` as the most recent element: this allows `entry` and `exit` to implement `cl:tagbody`.

If the exit point is not on the DESTACK, the effects are undefined. Safe implementations should signal a control error.

[FIXME: This should maybe have a closure parameter directly now rather than popping from the stack.]

```lisp
(let ((exit (pop STACK)))
  ;;(unless (member exit DESTACK) (error 'control-error ...)) ; optional!
  ;; Unwind
  (loop until (eq (first DESTACK) exit) do (cleanup (pop DESTACK)))
  ;; Reset state
  (setf CLOSURE (exit-closure exit) ARGUMENTS (exit-arguments exit)
  	STACK (exit-stack exit) VARARGS (exit-varargs exit))
  ;; Jump
  (incf IP label))
```

### entry-close #x2b

Discard an entry point from `destack`.

```lisp
(pop DESTACK)
```

### catch-{8,16} #x2c, #x2d (dest label)

Pop a value from the stack: this will be a catch tag. Create a new catch exit point, with destination `dest`. Push it to the dynenv stack.

```lisp
(push (make-catch-point (pop STACK) (+ IP dest) LITERALS CLOSURE ARGUMENTS STACK VARARGS) DESTACK)
```

### throw #x2e

Pop a value from the stack: this is a catch tag. If there is no catch exit point on `destack` with that tag, signal a control error. Otherwise, unwind to that catch. Note that unlike `entry` and `exit`, after `throw` the catch will no longer be on `destack`, as its extent has ended.

```lisp
(let* ((tag (pop STACK))
       (catch (loop for de in DESTACK
       	      	    when (and (catch-point-p de)
		    	      (eq tag (catch-point-tag de)))
		    return de)))
  (when (null catch) (error 'control-error ...))
  ;; Unwind
  (loop for entry = (pop DESTACK) do (cleanup entry) until (eq entry exit))
  ;; Reset state
  (setf CLOSURE (catch-closure catch) ARGUMENTS (catch-arguments catch)
  	STACK (catch-stack catch) VARARGS (catch-varargs catch)
	LITERALS (catch-literals catch))
  ;; Jump
  (setf ip (catch-dest catch)))
```

### catch-close #x2f

Discard a catch exit from `destack`.

```lisp
(pop DESTACK)
```

### special-bind #x30 (vcell literal)

Pop a value from the stack. Create a local special binding of `vcell` to that value.

```lisp
(push (make-special-binding (aref LITERALS vcell) (pop STACK)) DESTACK)
```

### symbol-value #x31 (vcell literal)

Push the value of the `vcell`'s (special) value to `stack`. If `vcell` is unbound, signal an `unbound-variable` error.

```lisp
(push (vcell-value (aref LITERALS vcell) DESTACK) STACK)
```

### symbol-value-set #x32 (vcell literal)

Pop a value from `stack`, and set `vcell`'s value to that.

```lisp
(setf (vcell-value (aref LITERALS vcell) DESTACK) STACK)
```

### unbind #x33

Discard a special binding (single, or `progv`) from `destack`.

```lisp
(pop DESTACK)
```

### progv #x34 (env literal)

Pop a value from the stack: this is a list of values. Pop another value from the stack: this is a list of symbols. Look up the symbols in `env` to get corresponding variable cells. Create local bindings of all of the variable cells according to `cl:progv` semantics.

The effects are undefined if any of the following are the case:

* The list of values is not a proper list.
* The list of symbols is not a proper list.
* The list of symbols has an element that isn't a symbol.
* Any of the symbols name a constant variable in `env`.

In a safe implementation, any of the first three points should result in a type error, and the fourth in an error. But this is not required, in order to allow existing Lisp implementations to implement the virtual machine.

```lisp
(let ((values (pop STACK)) (vars (pop STACK)) (env (aref LITERALS env)))
  (unless (proper-list-p values) (error 'type-error ...))
  (unless (and (proper-list-p vars) (every #'symbolp vars))
    (error 'type-error))
  (let ((vcells (loop for var in vars
       		      collect (variable-cell var env))))
    (when (some #'constant-vcell-p vcells) (error ...))
    (push (make-progv-dynenv vcells values) DESTACK)))
```

### fdefinition #x35 (fcell literal)

Grab a function cell from the literals. If the cell is unbound, signal an undefined function error. Otherwise, extract the cell's function and push it to `stack`.

```lisp
(let ((cell (aref LITERALS fcell)))
  (if (fcell-boundp fcell)
      (push (fcell-function cell) STACK)
      (error 'undefined-function ...)))
```

### nil #x36

Push `cl:nil` to `stack`. This is equivalent to a `const` operation but common enough to get its own opcode.

```lisp
(push nil STACK)
```

### push #x38

Push the primary value in `values` to `stack`.

```lisp
(push (if (zerop (length VALUES)) nil (aref VALUES 0)) STACK)
```

### pop #x39

Set `values` to have one value, popped from the stack.

```lisp
(setf VALUES (vector (pop STACK)))
```

### dup #x3a

Pop a value from the stack and push it twice.

```lisp
(let ((val (pop STACK))) (push val STACK) (push val STACK))
```

### fdesignator #x3b (env literal)

Pop a value from the stack. If it's a function, push it back to the stack. If it's a function name, look it up in `env`; if it's bound, push the function to `stack`, otherwise signal an undefined function error. If the value is anything else, signal a type error.

What constitutes a function name is implementation defined, but to implement CL must include at least the CL definition of `(or symbol (cons (eql setf) (cons symbol null)))`. Since the function designator is popped from the stack, in some circumstances even portable bytecode may refer to non-portable function designators. [FIXME: I think]

```lisp
(let ((desig (pop STACK)))
  (etypecase desig
    (function (push desig STACK))
    (function-name
      (let ((fcell (function-cell desig (aref LITERALS env))))
        (if (fcell-boundp fcell)
	    (push (fcell-function fcell) STACK)
	    (error 'undefined-function ...))))))
```

### called-fdefinition #x3c (fcell literal)

Grab a function cell from the literals. If the cell is unbound, signal an undefined function error. Otherwise, extract the cell's function and push it to `stack`.

This is identical to `fdefinition`, except that it is guaranteed that the function will only be used as a callee, and immediately. In some implementations this may allow an optimization: if unbound function cells contain a function that signals an undefined function error, there is no need for the virtual machine to check boundedness.

```lisp
(let ((cell (aref LITERALS fcell)))
  (if (fcell-boundp fcell)
      (push (fcell-function cell) STACK)
      (error 'undefined-function ...)))
;;; with optimization
(push (fcell-function (aref LITERALS fcell)) STACK)
```

### protect #x3d (template literal)

`template` is a bytecode function template from this module for a function that accepts zero arguments. Pop as many values from the stack as it needs and make a closure from the template. Create a new protection dynenv with the resulting function and push it to `destack`. Any exits through this dynenv will call the cleanup function, so this is used to implement `cl:unwind-protect`.

As the closure is only used for cleanups, it has dynamic extent. Implementations may choose to allocate it more efficiently, or to use the template as a "closure" when it doesn't close over anything.

```lisp
(let* ((template (aref LITERALS template))
       (closure (make-closure template (gather (closure-size template)))))
  (push (make-protection-dynenv closure DESTACK)))
```

### cleanup #x3e

Pop a dynenv from `destack`: it is a protection dynenv. Call its thunk with no arguments. This ends a body protected by `cl:unwind-protect` when not performing a nonlocal exit.

```lisp
(cleanup (pop DESTACK))
```

### encell #x3f (index misc)

Grab the `index`th local value. Put it in a fresh cell. Put it back.

This is equivalent to `ref index; make-cell; set index;` but is common enough to get its own instruction. And it makes analysis of bytecode a little simpler.

```lisp
(setf (aref LOCALS index) (make-cell (aref LOCALS index)))
```

### long #xff

This is a prefix rather than an actual instruction. It indicates that the next instruction should be parsed in long mode, i.e. with two-byte instead of one-byte operands. 

# Constraints

Not all sequences of instructions are valid bytecode programs. In order for the virtual machine can operate without doing runtime checks, and so that
bytecode can be analyzed coherently, there are many constraints on valid programs. A bytecode module can be checked before it is run, and rejected as erroneous if it does not meet these conditions. Some have already been mentioned.

* The `locals`, `literals`, and `closure` vectors are never accessed out of bounds.
* The stack never underflows.
* At any given instruction index, the number of values on the stack is fixed and statically determinable. This rules out, for example, loops that push or pop unevenly before returning to their starting point, or branches that merge with different stack states.
* Similarly, `values` and `varargs` are always either valid or invalid at any given instruction, so for example a merge point can't come from one merge with `values` and one without.
* `locals` positions are never referred to without being previously defined. The set of locals defined at any instruction index is, again, fixed and static.
* `values` is in a valid state when `return`, `push-values`, `append-values`, `throw`, or `push` is executed. Additionally it is valid before `exit` instructions if the target of that exit needs valid values.
* `values` is in an invalid state when `mv-call[-etc]`, `call[-etc]`, or `pop` is executed. Additionally it invalid before `exit` instructions if the target of the exit needs an invalid state.
* Dynamic environments are properly nested; so for example `entry-close` is never executed when the most recently pushed dynamic environment was not an `entry`. The nature of the dynamic environment stack at least back up to the call at any position is knowable statically.
* Dynamic environments are properly closed before any `return`.
* `make-cell` never pops a cell (i.e. cells are not wrapped in cells). `encell` similarly never reads a local that already holds a cell.
* Cells on the stack are only ever popped by the following instructions: `cell-ref`, `cell-set`, `make-closure`, `initialize-closure`.
* `cell-ref` and `cell-set` only pop cells.
* The literal referred to by `const` is not a function cell, variable cell, the environment, or template that needs a closure. The literals referred to by `make-closure` and `make-uninitialized-closure` are function templates. The literals referred to by `parse-key-args` are symbols. The literals referred to by `special-bind`, `symbol-value`, and `symbol-value-set` are variable cells. The literal referred to by `fdefinition` is a function cell. The literals referred to by `progv` and `fdesignator` are the environment.
* The object constructed by `make-uninitialized-closure` is not popped by any instructions besides `set`, `bind`, and `initialize-closure`. In particular, it is not called.
* The object popped by `initialize-closure` was pushed by `make-uninitialized-closure`.
* The argument parsing instructions are not used until the argument count has been checked.
* The "unsupplied" values put in by `bind-optional-args` and `parse-key-args` are not used by any instructions except `jump-if-supplied`.
* `append-values`, `pop-values`, and the `mv-call` instructions are only used when `varargs` is valid.
* The value put in `locals` by `save-sp` is not used by anything but `restore-sp`.
* The dynamic environment created by `entry` is not accessed after the corresponding `entry-close`.
* The value read by `restore-sp` was created by `save-sp`.
* The literal referred to by `protect` is a bytecode function template in the same module, that accepts zero arguments.

## Safety constraints

A safe implementation may impose the following additional constraints. If they are violated, the implementation may reject the bytecode, or fix it for safety.

* `call` and `mv-call` instruction callees are only ever the result of `fdefinition` or `fdesignator`, or a `const` instruction pointing to a literal that is a bytecode function template in the same module that does not need a closure. (To fix, an `fdesignator` instruction can be imposed before any call.)

# Versioning

Serialized bytecode in FASLs has a version identifier. This file describes version 0.13 of the virtual machine. See FASL.md for a changelog.
