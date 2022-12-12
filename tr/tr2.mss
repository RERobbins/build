@part(Chapter2, root = "tr.mss")
@Chapter (The BUILD Reference Level)
@Label (Reference)

This chapter introduces @c<build>'s reference based system modeling
scheme.  @c<build> system models are very easy to interpret because
they contain nothing more than declarations of how grains are grouped
to form modules and how these modules refer to each other.  Although
they do not present any construction dependencies explicitly, they can
be used to derive all of the construction information found in
construction based models (see Chapter @ref<Deriving>).  Construction
models cannot be used to derive the reference information found in
reference models.  Reference models are far less confusing than the
construction based models because they are written in a language that
replaces low level grain construction information with higher level
inter-module reference patterns.

@Section (Modules)

It is often the case that groups of grains are conceived as one
logical entity but are split up (e.g. into files) for other reasons.
Modeling schemes that represent systems only at the level of the
individual grain do not have the ability to express this kind of
grouping.  The module construct used by @c<build> (and @c<defsystem>)
allows these groupings to be made explicitly in system descriptions.

@c<build> module declarations have the form:
@begin(Definition)
(:MODULE @F1<MODULE-NAME GRAIN-TYPE> &REST @f1<GRAINS>)
@end(Definition)
@begin(Description)
@f1<MODULE-NAME>@\The name of a module.  The name must be unique
within the system model.

@F1<GRAIN-TYPE>@\The name of a grain type recognized by @c<build>.
Each grain is assumed to be an instance of this type.

@f1<GRAINS>@\The names of the grains that comprise the module.
@end(Description)

The following form declares that @f0<MAIN> is a Lisp source
module composed of the single grain @f0<MAIN.LISP>,
@begin(Example, above 0, below 0)
(:MODULE MAIN :LISP-SOURCE "MAIN.LISP")
@end(Example)
and the form:
@begin(example, above 0, below 0)
(:MODULE DEFS :C-SOURCE "DEFINITIONS-1.C" "DEFINITIONS-2.C")
@end(Example)
declares that @F0<DEFS> is a C source module with two grains named
@f0<DEFINITIONS-1.C> and @f0<DEFINITIONS-2.C>.

@c<build> can use grain type information without considering module
references to determine a great deal about the construction of grains.
For instance, @c<build> knows how to invoke the correct compiler on C
or Lisp source files or how to construct @c<lint> library files from
library sources by utilizing grain type information alone.

@Section (References)

@c<build> infers construction dependencies from reference assertions
by taking advantage of the fact that construction dependencies are
caused by references between modules.  If two modules do not refer to
each other, then it is impossible for there to be a construction
dependency that involves them.  When the assertion is made that
@i<module@-(1)> refers to @i<module@-(2)>, @c<build> pessimistically
assumes that each grain in @i<module@-(1)> refers to each grain in
@i<module@-(2)>.

References with the same name may be handled differently depending
upon the grain types of the modules involved in the reference.  For
instance, the @i<calls> reference between two Lisp source modules is
handled differently than the @i<calls> reference between two C source
modules.

@newpage
@c<build> reference declarations provide for the specification of
references between modules. No meaning is attached to the ordering of
reference declarations.  Reference declarations have the form:
@begin(Definition)
@F1<(REFERENCE LEFT-ELEMENT RIGHT-ELEMENT)>
@end(Definition)
@begin(description)
@f1<REFERENCE>@\The name of a reference recognized by @c<build>.

@f1<LEFT-ELEMENT>@\A module name or list of module names.  All module
names used in a reference declaration must have been declared in a
module declaration.

@f1<RIGHT-ELEMENT@ >@\A module name or list of module names.  All module
names used in a reference declaration must have been declared in a
module declaration.
@end(description)

The use of module name lists as either of the elements of a reference
declaration is syntactic sugar that is equivalent to the set of
reference declarations composed by enumerating @f1<REFERENCE-NAME> with
each pair in the cross product of the right and left element lists.
For example:
@begin(example, above 0, below 0)
(:CALLS (A B) (D E))
@end(example)
is equivalent to:
@begin(example, above 0)
(:CALLS A D)
(:CALLS A E)
(:CALLS B D)
(:CALLS B E)
@end(example)

Here are some reference triples and the construction dependencies
that they imply:
@begin(Description2)

@f0<(:CALLS LISP-SOURCE-1 LISP-SOURCE-2)>@\Asserts that
@f0<LISP-SOURCE-1> contains functions that call @f0<LISP-SOURCE-2> and
implies that @f0<LISP-SOURCE-2> will need to be loaded in order for
@f0<LISP-SOURCE-1> to execute.

@f0<(:MACRO-CALLS LISP-SOURCE-1 LISP-SOURCE-2)>@\ Asserts that
@f0<LISP-SOURCE-1> uses macros defined in @f0<LISP-SOURCE-2> and
therefore @f0<LISP-SOURCE-2> must be loaded in order for
@f0<LISP-SOURCE-1> to compile properly.  This reference implies that
if @f0<LISP-SOURCE-2> changes, then @f0<LISP-SOURCE-1> will need to be
re-compiled.

@f0<(:CALLS C-SOURCE-1 C-SOURCE-2)>@\Implies that the object
grains compiled from @f0<C-SOURCE-2> (as well as the object grains
from any module that @f0<C-SOURCE-2> calls) need to be linked into any
executable image that is to include the object grains from
@f0<C-SOURCE-1>.

@f0<(:INCLUDES C-SOURCE-1 C-SOURCE-2)>@\Asserts that @f0<C-SOURCE-1>
contains the contents of @f0<C-SOURCE-2>.  This reference implies that
whenever the included module, @f0<C-SOURCE-2>, changes, the including
module, @f0<C-SOURCE-1>, needs to be rebuilt.
@end(description2)

@c<build> uses triples (called reference signatures) of the form
@begin(example, above 0,below 0)
@F1[<REFERENCE-NAME LEFT-GRAIN-TYPE-NAME RIGHT-GRAIN-TYPE-NAME>]
@end(example)
to identify references.  @c<build> uses grain type information to
distinguish between references that have the same name but apply to
different grain types.  A given implementation of @c<build> will
define the reference signatures that are commonly used in the
environment that @c<build> is working with.  Chapter @ref<deriving>
describes how new reference signatures may be added to @c<build>.

@Section (Models)

The general form of a @c<build> system description is:
@begin(Example)
(DEFMODEL @f1<MODEL-NAME> &REST @F1<DECLARATIONS>) 
@end(Example)

There are four kinds of declarations that may be included in a
@f0<DEFMODEL> form: module, reference, default pathname, and
default module.  Module and reference declarations were described
earlier in this chapter.  The default pathname declaration allows for
the declaration of a pathname to be used as a template for completing
filenames.  It has the form:
@begin(Example)
(:DEFAULT-PATHNAME @f1<PATHNAME>)
@end(Example)
The default module declaration is used to declare a module as the
default module for @c<build> to operate on when construction requests
for the system are made.  It has the form:
@begin(Example)
(:DEFAULT-MODULE @f1<MODULE-NAME)>
@end(Example)

Figure @ref<TinyBuildModel> contains the @f0<DEFMODEL> form for
@c<tinycomp>.  The first four declarations are module declarations
that specify the grains and grain types of the system modules.  The
last three declarations specify the references between the modules in
the system.  Figure @ref<LintBuildModel> contains the @f0<DEFMODEL>
form for @C<lint>.  The model is longer than the @c<tinycomp> model
but no more complicated.

@begin[figure]
@begin(example)
(DEFMODEL TINYCOMP
  (:MODULE DEFS :C-SOURCE "DEFINITIONS")
  (:MODULE PARSER :YACC-GRAMMAR "PARSER")
  (:MODULE CODE-GENERATOR :C-SOURCE "CODEGEN")
  (:MODULE LIBRARY :C-OBJECT "LIBRARY")

  (:INCLUDES (PARSER CODE-GENERATOR) DEFS)
  (:CALLS PARSER (LIBRARY CODE-GENERATOR))
  (:CALLS CODE-GENERATOR LIBRARY))
@end(example)
@caption(@c<BUILD> Model For @C<tinycomp>)
@tag(tinybuildmodel)
@end[figure]
@begin[figure]
@begin(example)
(DEFMODEL LINT
  (:DEFAULT-PATHNAME "/USR/SRC/LIB/MIP")
  (:MODULE DEFINITIONS-1 :C-SOURCE
   "MACDEFS" "MANIFEST" "MFILE1" "LMANIFEST")
  (:MODULE DEFINITIONS-2 :C-SOURCE "MANIFEST" "LMANIFEST")
  (:MODULE PARSER :GRAMMAR "CGRAM")
  (:MODULE PASS-1 :C-SOURCE "LINT")
  (:MODULE PASS-2 :C-SOURCE "LPASS2")
  (:MODULE SUPPORT-1 :C-SOURCE
   "XDEFS" "SCAN" "COMM1" "PFTN" "TREES" "OPTIM" "HASH")
  (:MODULE SUPPORT-2 :C-SOURCE "HASH")
  (:MODULE DRIVER :SHELL-SCRIPT "SHELL")
  (:MODULE LIBRARIES :LINT-LIBRARY-SOURCE
   "LLIB-PORT" "LLIB-LC" "LLIB-LM" "LLIB-LMP" "LLIB-LCURSES")

  (:INCLUDES PASS-1 DEFINITIONS-1)
  (:INCLUDES PASS-2 DEFINITIONS-2)
  (:CALLS DRIVER (PASS-1 PASS-2 LIBRARIES))
  (:CALLS PASS-1 (PARSER SUPPORT-1))
  (:CALLS PASS-2 SUPPORT-2))
@end(example)
@caption(@c<BUILD> Description For @c<Lint>)
@tag(lintbuildmodel)
@end[figure]

    
