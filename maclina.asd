(asdf:defsystem #:maclina
  :description "Reference implementation of the Maclina bytecode system."
  :author ("Charles Zhang"
           "Christian Schafmeister <chris.schaf@verizon.net>"
           "Tarn W. Burton <twburton@gmail.com>"
           "Bike <aeshtaer@gmail.com>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :version "0.8.0"
  :depends-on (:maclina/base :maclina/compile :maclina/compile-file :maclina/load
               :maclina/vm-native :maclina/vm-cross))

(asdf:defsystem #:maclina/base
  :description "Basic components of the Maclina bytecode system."
  :author ("Charles Zhang"
           "Christian Schafmeister <chris.schaf@verizon.net>"
           "Bike <aeshtaer@gmail.com>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :version "0.8.0"
  :depends-on (:closer-mop)
  :components ((:file "machine")
               (:file "arg-conditions")
               (:file "structures" :depends-on ("machine"))
               (:file "link" :depends-on ("machine"))
               (:file "access" :depends-on ("machine"))
               (:file "disassemble" :depends-on ("structures" "machine"))))

(asdf:defsystem #:maclina/compile
  :description "Reference implementation compiler for Maclina."
  :author ("Charles Zhang"
           "Bike <aeshtaer@gmail.com>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :depends-on (:maclina/base :alexandria :trucler :ecclesia)
  :components
  ((:module "compile"
    :components ((:file "package")
                 (:file "misc-program-conditions" :depends-on ("package"))
                 (:file "parse-macro" :depends-on ("misc-program-conditions"
                                                   "package"))
                 (:file "unknown-reference-conditions" :depends-on ("package"))
                 (:file "compilation-unit"
                  :depends-on ("unknown-reference-conditions" "package"))
                 (:file "compile" :depends-on ("unknown-reference-conditions"
                                               "misc-program-conditions"
                                               "compilation-unit" "parse-macro"
                                               "package"))
                 (:file "documentation" :depends-on ("compile"))))))

(asdf:defsystem #:maclina/compile-file
  :description "Reference implementation file compiler for Maclina."
  :author ("Tarn W. Burton <twburton@gmail.com>"
           "Bike <aeshtaer@gmail.com>")
  :depends-on (:maclina/compile :eclector :ieee-floats)
  :components
  ((:module "compile-file"
    :components ((:file "package")
                 (:file "preliminaries" :depends-on ("package"))
                 (:file "read" :depends-on ("preliminaries" "package"))
                 (:file "cmpltv" :depends-on ("preliminaries" "package"))
                 (:file "encode" :depends-on ("cmpltv" "preliminaries" "package"))
                 (:file "top-level-forms" :depends-on ("preliminaries" "package"))
                 (:file "compile-file"
                  :depends-on ("read" "top-level-forms" "cmpltv"
                                      "encode" "package"))
                 (:file "documentation" :depends-on ("compile-file"))))))

(asdf:defsystem #:maclina/load
  :description "Reference implementation FASL loader for Maclina."
  :author ("Tarn W. Burton <twburton@gmail.com>"
           "Bike <aeshtaer@gmail.com>")
  :depends-on (:maclina/base :ieee-floats)
  :components ((:file "loadltv")))

(asdf:defsystem #:maclina/vm-native
  :description "Maclina VM implementation using host environment."
  :author ("Charles Zhang"
           "Bike <aeshtaer@gmail.com>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :depends-on (:maclina/base :trucler) ; trucler only needed for client class - remove?
  :components ((:file "vm-native")))

(asdf:defsystem #:maclina/vm-cross
  :description "Maclina VM implementation using Clostrum environment."
  :author ("Charles Zhang"
           "Bike <aeshtaer@gmail.com>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :depends-on (:maclina/base :clostrum :clostrum-trucler)
  :components ((:file "vm-cross")))

(asdf:defsystem #:maclina/test
  :author ("Bike <aeshtaer@gmail.com>")
  :maintainer "Bike <aeshtaer@gmail.com>"
  :depends-on (:maclina :clostrum-basic :fiveam)
  :components
  ((:module "test"
    :components ((:file "packages")
                 (:file "suites" :depends-on ("packages"))
                 (:file "rt" :depends-on ("packages"))
                 (:file "native-sham" :depends-on ("rt" "packages"))
                 (:module "cross"
                  :depends-on ("rt" "native-sham")
                  :components ((:file "packages")
                               (:file "sham" :depends-on ("packages"))
                               (:file "rt" :depends-on ("sham"
                                                        "packages"))))
                 (:module "fasl"
                  :depends-on ("suites" "rt" "packages")
                  :components ((:file "similarity")
                               (:file "externalize")))
                 (:file "cleanliness" :depends-on ("suites" "rt" "packages"))
                 (:file "cooperation" :depends-on ("suites" "rt" "packages"))
                 (:module "compiler-conditions"
                  :depends-on ("suites" "rt" "packages")
                  :components ((:file "reference")
                               (:file "syntax")
                               (:file "macroexpansion")))
                 (:module "ansi"
                  :depends-on ("suites" "rt" "packages")
                  ;; These can be loaded in any order.
                  :components (;; eval-and-compile
                               (:file "compile")
                               (:file "dynamic-extent")
                               (:file "eval")
                               (:file "eval-when")
                               (:file "ignorable")
                               (:file "ignore")
                               (:file "lambda")
                               (:file "locally")
                               (:file "optimize")
                               (:file "special")
                               (:file "symbol-macrolet")
                               (:file "the")
                               (:file "type")
                               ;; data-and-control-flow
                               (:file "block")
                               (:file "catch")
                               (:file "flet")
                               (:file "if")
                               (:file "labels")
                               (:file "let")
                               (:file "letstar")
                               (:file "macrolet")
                               (:file "multiple-value-call")
                               (:file "multiple-value-prog1")
                               (:file "progn")
                               (:file "progv")
                               (:file "return-from")
                               (:file "tagbody")
                               (:file "unwind-protect")
			       ;; system-construction
			       (:file "compile-file")))
                 (:file "run-all"
                  :depends-on ("ansi" "cross" "rt" "packages"))))))
