@make(slides)
@majorheading(Agenda)
@blankspace(.5 in)
@begin(enumerate)
What is BUILD?

What is the motivation for BUILD?

What is the key idea?

How to model a system with BUILD.

Making requests.

Defining kinds of systems.
@end(enumerate)

@newpage
@majorheading(What is BUILD?)
@blankspace(.5 in)
BUILD is a tool for @p<describing> and @p<constructing modular>
systems.

@newpage
@majorheading<What is the motivation for BUILD?>
@blankspace(.5 in)
@begin(enumerate)
We need to manage and understand the modules that comprise our
systems.

Existing tools (@c<MAKE, DEFSYSTEM>) are not adequate.
@end(enumerate)

@newpage
@majorheading<What is the key idea?>
@blankspace(.5 in)

Derivation of construction dependencies from module referencing patterns.

@newpage
@majorheading<How to model a system with BUILD.>
@blankspace(.5 in)
Module Definitions + Reference Assertions

@begin(format)
(MODULE DRIVER 'LISP-SOURCE "DRIVER")
(MODULE OPTIONS 'LISP-SOURCE "OPT-1" "OPT-2" "OPT-3")
(MODULE UTILITIES 'LISP-SOURCE "TREES" "I-O")
(MODULE DOC 'SCRIBE-SOURCE "REFERENCE" "INTRO")
(CALLS DRIVER OPTIONS)
(CALLS OPTIONS UTILITIES)
@end(format)

Module definitions map abstract modules onto the things that the
computer traditionally manipulates (files, functions etc.).

Reference assertions specify how the modules interact.

@newpage
@majorheading<Making requests.>
@blankspace(.5 in)
@begin(format)
(REQUEST 'COMPILE-LOAD DRIVER)
(REQUEST 'FORMAT DOC)
@end(format)

Requests cause the construction dependency graph to be derived from
the set of relevant reference assertions.  The request is then honored
by ensuring that the modules corresponding to some set of nodes in the
resulting graph exist.

@newpage
@majorheading<Defining kinds of systems.>
@blankspace(.5 in)
@begin(enumerate)
Define appropriate module types.

Define appropriate process types.

Define appropriate reference types.
@end(enumerate)
