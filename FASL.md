The Maclina FASL format is an implementation-independent representation of compiled Lisp files. Maclina FASLs record a sequence of instructions which can be executed to perform the side effects of loading a file. The FASL "VM" is distinct from and much simpler than the Maclina VM; for example there are no local variables or control flow.

While the format is implementation-independent, the contents of files are not without careful work. If a macro form expands to an implementation-defined internal function, for example, a FASL containing that form couldn't be loaded in another implementation.

# Specification

A full specification is pending the version 1.0 of the format, which is planned for when it can be really used for cross-compilation, a kind of smoke test for implementation independence. So for now the format is, unfortunately, what cmpltv.lisp generates. But here are the broad outlines.

A FASL consists of a header followed by a sequence of instructions. All multi-byte integers are in big-endian order. The header is made up of:

1. The four byte magic number: `8d 74 98 b1` (randomly chosen).
2. A two byte major version followed by a two byte minor version. This version is for the FASL format and not the compiled code.
3. An eight byte count of instructions.

The instructions follow. Each instruction consists of a one-byte opcode and successive bytes with meaning particular to that opcode.

Instructions can either be purely for effect, or create objects. Created objects are stored in an array where they can be later referenced by other instructions; this is how complex possibly-circular objects are built up, and how they can be referred to by side effects. These arrays are set up by the `init-object-array` instruction, and only one array is live at a time. FASLs usually have only one array; the instruction exists so that multiple FASLs can be concatenated easily.

After the last instruction is executed, the FASL has been fully loaded. Any remaining object array can be garbage collected, as any objects in them that are still live must have been made accessible elsewhere from a FASL side effect.

# Changelog

## 0.14 (pending)

* `listify-rest-args` now assigns directly to a local rather than pushing to the stack.
* New `encell` instruction for a common lexical variable cell making operation.
* `protect` now gets its function from the literals vector.

## 0.13

* `fdesignator` and `progv` changed to have an environment parameter for first-class environment purposes.
* `environment` fasl op to get the loader environment for `fdesignator`.
* New instructions `protect` and `cleanup` for implementing `cl:unwind-protect`.
* Redundant indexing in FASLs is eliminated, reducing complexity and saving space.
* Character encoding in FASLs changed from UTF-32 to UTF-8.
* FASL function names, docstrings, and lambda lists are changed from mandatory fields to optional standard attributes: "name", "docstring", and "lambda list" respectively.
* Arrays in FASLs may have any element type (provided the elements within can be dumped).

## 0.12

* `mv-call[-receive-{one,fixed}] semantics changed so that arguments are on stack rather than values vector.
* `called-fdefinition` instruction added for microoptimized function lookup.
* `fdesignator` instruction added so that `multiple-value-call` can be compiled without other environment support. (NOTE: The 0.12 version of this instruction does not have an environment parameter. The 0.12 version was only used in Clasp and is not represented in this repository's history.)

## 0.11

* New `vcell` and `fcell` instructions can be used to look up variable and function cells at load time to speed execution, if the implementation supports it. They are also useful for first-class environments.

## 0.10

* Module debug info was refactored; this is a Clasp-specific change as the implementation-independent debug info is not available yet.

## 0.9

* New `init-object-array` instruction records how many objects, rather than this being global to the FASL. This allows FASLs to be concatenated easily.

## 0.8

* Bytecode size field is added to cfunctions.

## 0.7

* `setf-literals` records all the literals inline rather than referring to a separate vector.

## 0.6

* Attributes are interleaved with instructions by creating an `attribute` instruction. This allows debug info to be set for functions before they are called.

## 0.5

* `funcall-initialize` now takes arguments. This reduces required compilation of simple function calls at top level.
* New `find-class` instruction to simplify a very common load form.

## 0.4

* New "attributes" allow accessory, possibly implementation defined information.

## 0.3

* Array instructions have been refactored to allow packed arrays.
* Header now counts instructions rather than bytes.

## 0.2

* Have multiple bytecode modules per FASL.
