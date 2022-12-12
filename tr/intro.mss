@part(Introduction, root = "tr.mss")
@Chapter (Introduction)
@Label (Introduction)

Many programming languages encourage the development of modular
systems by allowing the independent compilation of modules (ADA
@cite<ADA>, C @cite<C>, CLU @cite<CLU>, Common-Lisp
@cite<common-LISP>, Mesa @cite<Mesa>).  This feature can be exploited
to minimize the amount of compilation that needs to be done when some
part of a system is changed.  However, as systems become larger it
becomes difficult to know exactly which modules need to be recompiled
when one changes.  It is important that the correct modules be
recompiled and relinked -- a bug caused by ignoring a module that
should be rebuilt can be very difficult to find.  This problem is
called the consistent construction problem.

This report describes @c<build>, a tool that reconstructs system
modules in order to ensure that they are kept in a consistent state.
@c<build> does not modify source modules and will not rid systems of
problems that require source code revision.  However, @c<build> can
handle the many instances where some portion of a system needs to be
recompiled, relinked, or somehow reprocessed in order to eliminate
inconsistency.

There are many tools that manipulate systems by reconstructing
inconsistent parts.  Chapter @ref<SystemConstructionTools> presents
@c<make>@cite<make> and @c<defsystem>@cite<LispMachine>, two
representative tools, and discusses some of their weaknesses.  The
fundamental problem with @c<make>, @c<defsystem>, and all similar
construction directive based tools is that they operate on systems by
using user supplied lists of construction directives.  These lists are
difficult to understand.  @c<build> provides the same functionality as
existing tools but does so without requiring users to list
construction steps.

@c<build> derives the construction steps needed to produce a module
from user supplied @i<system models>.  These models specify how
modules reference each other instead of how they are constructed.
@c<build> uses the reference information to determine how modules
depend on each other and how a change to one module will effect
another. For instance, if a system model specifies that
@i<module@-(1)> refers to macros defined in @i<module@-(2)>, then
@c<build> can infer that a change to @i<module@-(2)> implies that
@i<module@-(1)> should be recompiled.  Chapter @ref<Reference>
discusses system models and chapters @ref<task>, and @ref<deriving>
explain how @c<build> uses system models to perform construction.

The major strength of @c<build>'s reference based modeling system
over a construction directive based system is that it provides a
higher level language for describing system structure.  Because it
eliminates low level construction detail and allows explicit
declaration of high level system relationships, a reference based
model is easier to understand and provides more information than its
construction directive based counterpart.

@c<build> separates knowledge about systems from knowledge about how
systems are manipulated.  The term @i<task> is used to refer to a
construction process such as compilation or linking that @c<build> may
be called upon to perform.  @c<build> uses @i<task descriptions> to
specify how to perform construction tasks and how the various kinds of
references that appear in system models may effect the construction
required to perform the task.  Using the example from the previous
paragraph, @c<build>'s task description for compilation allows it to
realize that while a change to @i<module@-(2)> implies that
@i<module@-(1)> should be recompiled, a change to @i<module@-(1)> does
not imply that @i<module@-(2)> should be recompiled.

@newpage
@subsection(TINYCOMP)
@c<tinycomp> is an example of a modular system, it will be used
throughout this report to present different aspects of system
construction tools (this example was adapted from one used by
Feldman@cite<make>).  @c<tinycomp> has two major modules, a parser and a
code generator.  The parser is built by @c<YACC>, a parser generating
tool@cite<YACC>.  The code generator is implemented in C@cite<C>.  The
parser and code generator use a common set of definitions for shared
data structures.  These definitions are combined with the source
programs during compilation.  The compiled programs are linked with a
library that is also subject to change.  Figure @ref<Tiny-Ref-Graph>
depicts @c<tinycomp>'s inter-module reference pattern and figure
@ref<Intro-Tiny-Construction-Graph> depicts @c<tinycomp>'s construction
process.

@begin[figure]
@begin(verbatim, Font SmallBodyFont, FaceCode F)
@tabclear
@tabdivide (5)
@blankspace (.5 in)
@\@=CALLS@\@=CODE-GENERATOR@\
@blankspace (.75 in)
@=PARSER@\@=INCLUDES@\@=DEFS@\   INCLUDES
@blankspace (.75 in)
@\@=CALLS@\@=LIBRARY@\@=CALLS
@blankspace (.5 in)
@TABCLEAR
@END(VERBATIM)
@CAPTION(@c<TINYCOMP> Inter-Module Reference Graph)
@TAG(TINY-REF-GRAPH)
@end[figure]
@begin[figure]
@blankspace (1 lines)
@BEGIN(VERBATIM, FONT SMALLBODYFONT, FACECODE F)
@TABCLEAR
@TABDIVIDE(7)
@blankspace (.25 in)
PARSER.GRAMMAR@=YACC@\PARSER.C@\COMPILER@\PARSER.O
@BLANKSPACE(.75 IN)
@\@\DEFINITIONS.C
@BLANKSPACE(.75 IN)
@\@\CODEGEN.C@\COMPILER@\CODEGEN.O@\  LINKER@\TINYCOMP
@BLANKSPACE(.75 IN)
@\@\@\@\LIBRARY.O
@TABCLEAR

@END(VERBATIM)
@CAPTION(Construction Graph For @c<Tinycomp>)
@TAG(INTRO-TINY-CONSTRUCTION-GRAPH)
@end[figure]
@newpage
@subsection<Reference Based System Models>

Compare figure @ref<IntroTinyMakeFile> which contains the @c<make>
directives for @c<tinycomp>, and figure @ref<IntroTinyBuildModel>
which contains the @c<build> system model for @c<tinycomp>.  While the
@c<make> directives encode @c<tinycomp>'s construction graph,
@c<build>'s system model encodes @c<tinycomp>'s reference graph.

A reference model can be used in place of a construction directive
list because all of the information about construction present in such
a list can be derived from a reference model.  Consider the third
@c<make> directive for @c<tinycomp>:
@begin(example, above 0, below 0)
CODEGEN.O: CODEGEN.C DEFINITIONS.C
	CC -C CODEGEN.C		# -C COMPILES
@end(example)
This expresses that @f0<CODEGEN.O> is produced by compiling
@f0<CODEGEN.C>, and that if either @f0<CODEGEN.C> or
@f0<DEFINITIONS.C> changes, then @f0<CODEGEN.C> needs to be
recompiled.  This construction dependency exists because
@f0<CODEGEN.C> is combined with @f0<DEFINITIONS.C> when it is compiled
to produce @f0<CODEGEN.O>.

In contrast, the reference based model specifies that @f0<CODE-GENERATOR>
@i<includes> @f0<DEFS>:
@begin(example, above 0, below 0)
(:INCLUDES CODE-GENERATOR DEFS)
@end(example)
@c<build>'s description for compilation contains the knowledge that
the @f0<:INCLUDES> reference implies a compilation construction
dependency between @i<including> and @i<included> files.

@begin[figure]
@begin(Example, LeftMargin +0)

PARSER.C: PARSER.GRAMMAR
	YACC PARSER.GRAMMAR	#YACC MAKES Y.TAB.C
	MV Y.TAB.C PARSER.C	#RENAME Y.TAB.C

PARSER.O: PARSER.C DEFINITIONS.C
	CC -C PARSER.C		# -C COMPILES

CODEGEN.O: CODEGEN.C DEFINITIONS.C
	CC -C CODEGEN.C		# -C COMPILES

TINYCOMP: CODEGEN.O PARSER.O LIBRARY.O
	CC CODEGEN.O PARSER.O LIBRARY.O -O TINYCOMP # -O LINKS
@end(Example)
@caption(MakeFile For @c<TinyComp>)
@tag(IntroTinyMakeFile)
@end[figure]
@begin[figure]
@blankspace (1 lines)
@begin(example, LeftMargin +0)
(DEFMODEL TINYCOMP
  (:MODULE DEFS :C-SOURCE "DEFINITIONS")
  (:MODULE PARSER :YACC-GRAMMAR "PARSER")
  (:MODULE CODE-GENERATOR :C-SOURCE "CODEGEN")
  (:MODULE LIBRARY :C-OBJECT "LIBRARY")

  (:INCLUDES PARSER DEFS)
  (:INCLUDES CODE-GENERATOR DEFS)
  (:CALLS PARSER LIBRARY)
  (:CALLS PARSER CODE-GENERATOR)
  (:CALLS CODE-GENERATOR LIBRARY))
@end(example)
@caption(@c<BUILD> Model For @C<tinycomp>)
@tag(introtinybuildmodel)
@end[figure]

@newpage
@subsection <Task Descriptions>

Upon receipt of a request to perform a task, @c<build> derives a task
graph which models the construction steps and dependencies necessary
to perform the task.  (Chapter @ref<task> presents @c<build> task
models and chapter @ref<deriving> explains how task models are derived
from system models.)  Once the task model has been derived, @C<build>
analyzes it in order to determine which components have changed and
what steps are needed in order to satisfy the task request.

@c<build> provides a static framework for modeling systems and
handling construction requests that makes use of programming
environment specific definitions.  New tasks can be added to
@c<build>'s repertoire by altering the set of definitions.

For example, figure @ref<source-code-Task> contains the forms needed
to define a task called @f0<:LIST-SOURCE-CODE> which produces
formatted listings of the source modules of a Lisp system.  (This
example will be explained in detail in chapter @ref<deriving>.)  The
first form allows @c<build> to represent the processing needed to list
a single Lisp source file.  The second form tells @c<build> what to do
when a @f0<:LIST-SOURCE-CODE> request is received.  The last two forms
tell @c<build> about the implications of the references @f0<:CALLS>
and @f0<:MACRO-CALLS> upon the @f0<:LIST-SOURCE-CODE> task.

Since task definitions are separate from system models, new tasks can
be performed on existing models without additional effort.  For
instance, once @f0<:LIST-SOURCE-CODE> has been defined, @c<build> will
be able to handle requests to format the source code for existing
systems without changing any system models.  Construction directive
based tools cannot be extended in a similar manner.

@begin[figure]
@blankspace (1 lines)
@begin(example, LeftMargin +0 Char)
(DEFINE-PROCESS-TYPE :LIST-LISP-SOURCE
    ((SOURCE :LISP-SOURCE :SINGLE))
    ((LISTING :PRESS :SINGLE SOURCE))
    OUTPUT-STREAM
    (FORMAT OUTPUT-STREAM "~%LIST ~A"
	    (PATHNAME-MINUS-VERSION SOURCE))
  (FORMAT OUTPUT-STREAM "~%LISTING ~A" SOURCE)
  (LIST-LISP-FILE SOURCE LISTING))

(DEFINE-REQUEST-HANDLER (:LIST-SOURCE-CODE :LISP-SOURCE :PRE)
			(SOURCE-NODE)
  (ACCESS* SOURCE-NODE ((SOURCE :LIST-LISP-SOURCE) LISTING)))

(DEFINE-REFERENCE-HANDLER ((:MACRO-CALLS :LISP-SOURCE :LISP-SOURCE)
			   (:LIST-SOURCE-CODE :LEFT))
			  (IGNORE CALLED-NODE)
  (PROCESS-REQUEST :LIST-SOURCE-CODE CALLED-NODE))

(DEFINE-REFERENCE-HANDLER ((:CALLS :LISP-SOURCE :LISP-SOURCE)
			   (:LIST-SOURCE-CODE :LEFT))
			  (IGNORE CALLED-NODE)
  (PROCESS-REQUEST :LIST-SOURCE-CODE CALLED-NODE))

@end(example)
@CAPTION(Definition For @f0<:LIST-SOURCE-CODE>)
@TAG(Source-Code-Task)
@end[figure]

 
 
