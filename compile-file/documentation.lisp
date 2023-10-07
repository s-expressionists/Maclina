(in-package #:maclina.compile-file)

(setf (documentation 'compile-file 'function)
      "As CL:COMPILE-FILE. Compile the given file using Maclina. All normal COMPILE-FILE semantics should apply, e.g. top level form processing. Outputs a bytecode FASL.
Returns CL:COMPILE-FILE's usual three values of the output file, warningsp, and failurep.
The four standard variables bound by CL:COMPILE-FILE - *READTABLE*, *PACKAGE*, *COMPILE-FILE-TRUENAME*, and *COMPILE-FILE-PATHNAME*, are bound by use of MACLINA.MACHINE:PROGV in the Maclina environment. These are considered part of the protocol of COMPILE-FILE, in that they have to be at least available as dynamic variables in any environment that code can be COMPILE-FILE'd in.
Besides the standard arguments, there are
CLIENT: The Maclina client used for compilation, etc. The default is the value of MACLINA.MACHINE:*CLIENT*.
ENVIRONMENT: The compilation environment that should be used for compilation, as well as compile-time evaluation through EVAL-WHEN, #., etc. If not provided, defaults to NIL, meaning the host's global environment.
READER-CLIENT: The client passed to Eclector to perform read operations."
      (documentation 'compile-stream 'function)
      "Like COMPILE-FILE, but operates on streams. INPUT must be a character stream and OUTPUT an (unsigned-byte 8) stream. The FASL will be written out to the OUTPUT stream. Returns the output stream as its primary value.
Since it doesn't have an input pathname, this function does not bind *COMPILE-FILE-TRUENAME* or *COMPILE-FILE-PATHNAME*.

See COMPILE-FILE")
