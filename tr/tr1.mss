@part(Chapter1, root = "tr.mss")
@Chapter (System Construction Tools)
@Label (SystemConstructionTools)

This chapter focuses on two tools that were designed to aid in the
management of the consistent construction problem.  Before they are
presented some terminology that will be used throughout this report is
introduced.

Different programming environments are geared to operate upon
different kinds of objects.  For instance, some environments are
designed to operate on files, and others on functions.  The term
@i<grain> will be used to refer to the objects manipulated in a
programming environment -- regardless of their nature.

The terminology introduced in this paragraph will be used to refer to
the kinds of grains that are manipulated during the construction
process.  @i<Source> grains are the components that are produced by
people and not programs (e.g., programming language source code).
Source grains are manipulated by programs to produce @i<derived>
grains (e.g., object code).  Grains that are the final products of the
construction process are called @i<goal> grains (e.g., executable
images of programs).  While goal grains are usually derived grains,
they can also be source grains.  Derived grains that are not goal
grains are called @i<intermediate> grains (e.g., object code that
requires linking in order to form executable images).

@section (MAKE)

@c<make> @cite<make>, available as part of UNIX@foot<UNIX is a
trademark of Bell Laboratories>, is a simple tool for managing systems
that has received widespread use.  @c<make> is driven by sets of
construction directives that form "recipes" for constructing systems.
These directives are stored in a text file called a MakeFile and have
the form:
@begin(example)
@f1<
TARGET-GRAIN @F0<:> INGREDIENT-GRAIN-1 INGREDIENT-GRAIN-2 ...
	COMMAND-1
	COMMAND-2>
	  :
	  :
@end(example)
Each entry declares that @f1<TARGET-GRAIN> depends on each of the
grains to the right of the colon.  The command sequence below the
construction dependency declaration line is executed in order to
construct @f1<TARGET-GRAIN>.  There are no constraints placed on the
commands which can appear in the command sequence.  Furthermore, there
are no ordering rules for MakeFile entries.

@c(make) has a simple macro substitution facility.  A macro is defined
in the following manner:
@begin(example)
@F1<MACRO-NAME@F0<=>MACRO-EXPANSION>
@end(example)
Any instance of @f1<MACRO-NAME> enclosed within parentheses and
preceded by a dollar sign (i.e., @f0<$(@F1<MACRO-NAME>)>) is replaced
by the text @f1<MACRO-EXPANSION> when the MakeFile that includes the
macro definition is processed.  The definition for a macro must
precede all of its uses.

@SubSection <A Small Example -- TINYCOMP>

Figure @ref(INTRO-TINY-CONSTRUCTION-GRAPH-2) depicts the construction
process for @c<TINYCOMP> and figure @ref<TinyMakeFile> contains a
corresponding MakeFile.  Given the MakeFile, @c<make> will perform the
appropriate construction when @c<tinycomp> components change.  For
instance, a change to @F0<PARSER.GRAMMAR> will cause a new parser to
be derived, compiled, and linked.  A change to @F0<CODEGEN.C> will
cause @f0<CODEGEN.C> to be compiled and linked.  A change to
@F0<DEFINITIONS.C> will cause @F0<PARSER.C> and @F0<CODEGEN.C> to be
compiled and linked.  Finally, a change to @f0<LIBRARY.O> will cause
linking but no compiling.
@newpage
@begin[figure]
@BEGIN(VERBATIM, FONT SMALLBODYFONT, FACECODE F)
@TABCLEAR
@TABDIVIDE(7)
@blankspace (.5 in)
PARSER.GRAMMAR@=YACC@\PARSER.C@\COMPILER@\PARSER.O
@BLANKSPACE(.75 IN)
@\@\DEFINITIONS.C
@BLANKSPACE(.75 IN)
@\@\CODEGEN.C@\COMPILER@\CODEGEN.O@\  LINKER@\TINYCOMP
@BLANKSPACE(.75 IN)
@\@\@\@\LIBRARY.O
@BLANKSPACE (.3 IN)
@TABCLEAR
@END(VERBATIM)
@CAPTION(Construction Graph For @c<Tinycomp>)
@TAG(INTRO-TINY-CONSTRUCTION-GRAPH-2)
@end[figure]
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
@tag(TinyMakeFile)
@end[figure]
The MakeFile entries are interpreted in the following manner:
@begin(description2)
@f0<PARSER.C: PARSER.GRAMMAR ...>@\@f0<PARSER.C> depends on
@f0<PARSER.GRAMMAR>.  It is created by running @c<YACC> on
@f0<PARSER.GRAMMAR>.

@f0<PARSER.O: PARSER.C DEFINITIONS.C ...>@\@f0<PARSER.O> depends on
@f0<PARSER.C> and @f0<DEFINITIONS.C>.  It is created by recompiling
@f0<PARSER.C>.

@f0<CODEGEN.O: CODEGEN.C DEFINITIONS.C ...>@\@f0<CODEGEN.O> depends on
@f0<CODEGEN.C> and @f0<DEFINITIONS.C>.  It is created by recompiling
@f0<CODEGEN.C>.

@f0<TINYCOMP: CODEGEN.O PARSER.O LIBRARY.O ...>@\@f0<TINYCOMP>
depends on @f0<CODEGEN.O>, @f0<PARSER.O>, and @f0<LIBRARY.O>.  It is
created by relinking the system.
@end(description2)

@SubSection (The Construction Process)

@c<make> is invoked with the following UNIX command line template
(brackets indicate optional fields):
@begin(definition)
MAKE [-f @F1<MAKEFILE>] [@f1<OPTION> ... ] [@f1<TARGET-GRAIN>]
@end(definition)
@begin(description)
@f1<MAKEFILE>@\Specifies the name of the file containing the
construction directives, if no @f0<-f> option is used then @c<make>
uses the file named @F0<MAKEFILE> in the working directory.

@f1<OPTION>@\Specifies options like @i<print but do not execute the
command sequences> or @i<update the modified date of the targets
without executing any command sequences>.

@f1<TARGET-GRAIN>@\ Specifies the name of the target grain to be
processed, if @f1<TARGET-GRAIN> is not specified then @c<make> will
process the first target grain named in the MakeFile.
@end(description)

@c<make> begins by constructing a dependency graph from the selected
MakeFile.  Each node in the graph corresponds to a grain mentioned in
the MakeFile.  The children of a node represent the grains that the
grain represented by the node depends on.  A request to @i<make> a
target grain is processed by doing a depth-first walk of the graph
starting with the node that corresponds to the target.  At each node
visited, any grains that are missing or whose children have changed
are updated.

@c<make> compares the creation dates of a target grain and its
ingredient grains as an approximate means of noting when changes
occur.  For instance if @f0<TARGET-1> depends on @f0<INGREDIENT-1>
then @c<make> will assume that @f0<INGREDIENT-1> has changed if and
only if its creation date is after the creation date of @f0<TARGET-1>.
Since UNIX allows file creation dates to be modified by users, it is
possible to fool @c<make> by changing file attributes.  However, since
most people do not change file attributes, the @c<make> mechanism is
reasonable.

Without information about how an ingredient has changed, @c<make>
cannot determine whether a change is significant or not.  Therefore,
@c<make> pessimistically assumes that every change to an ingredient
grain will effect the target grain, and it will always reconstruct a
target when one of its ingredients has changed.  Figure @ref<LispMake>
contains the @c<make> construction algorithm written in Lisp.
@begin(figure)
@blankspace(1 line)
@begin(Example, LeftMargin +0)
(DEFUN MAKE (NODE)
  (DOLIST (CHILD (GET-CHILDREN NODE))
    (MAKE CHILD))
  (IF (OR (NON-EXISTENT-P NODE) (CHILDREN-CHANGED-P NODE))
      (UPDATE NODE)))

(DEFUN CHILDREN-CHANGED-P (NODE)
  (< (CREATION-DATE NODE)
     (APPLY #'MAX
	    (MAPCAR #'GET-CREATION-DATE (GET-CHILDREN NODE)))))
@end(Example)
@caption(@c<MAKE> Construction Algorithm)
@tag(LispMake)
@end[figure]
@newpage
@SubSection <An Extended Example -- LINT>

The @c<Lint> system@cite<lint> is presented as an extended example of
using @c<make>.  @c<Lint> examines C source programs and detects bugs
that most C compilers cannot.  It is also sensitive to constructs that
are legal but may not be portable.

@C<lint> consists of a UNIX shell script driver, a set of @C<lint>
Library files, and two C programs.  Before programs are processed by
the first C program (i.e., the first pass of @c<Lint>), they are
processed by the C pre-processor, which handles macro expansion and
some compiler directives.

After being processed by the C pre-processor, programs are sent to the
first pass of @C<lint>.  This pass does lexical analysis on the input
text, constructs and maintains symbol tables, and builds trees for
expressions.  An intermediate file that consists of lines of ASCII
text is produced.  Each line contains an external identifier name, an
encoding of the context in which it was seen (use, definition,
declaration, etc.), a type specifier, and a source file name and line
number.  The information about variables local to a function or file
is collected by accessing the symbol table, and examining the
expression trees.  Comments about local problems are produced as
detected.  The information about external names is collected in the
intermediate file.

@C<lint> libraries are collections of definitions of external names
that are appended to the intermediate file generated by the first pass
of @C<lint>.  They are used to provide @C<lint> with a set of
definitions for commonly used external names without processing the
source that contains the definitions.  The most commonly used
libraries contain the definitions for the functions that are supplied
by the UNIX C run time environment.  Users can create their own
libraries of commonly used names in order to alleviate repeated
processing.

After all the source files and library descriptions have been
collected, the intermediate file is sorted to bring all information
collected about a given external name together.  The second pass of
@C<lint> then reads the lines from the intermediate file and compares
all of the definitions, declarations, and uses for consistency.

Figure @ref<LintMakeFile> contains the MakeFile for @c<Lint>.  The
primary point of this example is that MakeFiles, even for medium sized
systems like @C<lint>, are difficult to understand.  The @c<build>
description mechanism introduced in chapter @ref<Reference> provides a
much simpler way to describe systems.

@fullpagefigure[
@begin(SmallExample, LeftMargin +0)
M=/USR/SRC/LIB/MIP
CFLAGS=-O -DFLEXNAMES
LINTLIBS=LLIB-PORT.LN LLIB-LC.LN LLIB-LM.LN LLIB-LMP.LN LLIB-LCURSES.LN

ALL:	LPASS1 LPASS2 $(LINTLIBS)

LPASS1: CGRAM.O XDEFS.O SCAN.O COMM1.O PFTN.O TREES.O OPTIM.O LINT.O HASH.O
	CC CGRAM.O XDEFS.O SCAN.O COMM1.O PFTN.O TREES.O OPTIM.O LINT.O HASH.O -O LPASS1

TREES.O: $(M)/MANIFEST MACDEFS $(M)/MFILE1 $(M)/TREES.C
	CC -C $(CFLAGS) -I$(M) -I. $(M)/TREES.C
OPTIM.O: $(M)/MANIFEST MACDEFS $(M)/MFILE1 $(M)/OPTIM.C
	CC -C $(CFLAGS) -I$(M) -I. $(M)/OPTIM.C
PFTN.O: $(M)/MANIFEST MACDEFS $(M)/MFILE1 $(M)/PFTN.C
	CC -C $(CFLAGS) -I$(M) -I. $(M)/PFTN.C
LINT.O: $(M)/MANIFEST MACDEFS $(M)/MFILE1 LMANIFEST
	CC -C $(CFLAGS) -I$(M) -I. LINT.C
SCAN.O: $(M)/MANIFEST MACDEFS $(M)/MFILE1 $(M)/SCAN.C
	CC -C $(CFLAGS) -I$(M) -I. $(M)/SCAN.C
XDEFS.O: $(M)/MANIFEST $(M)/MFILE1 MACDEFS $(M)/XDEFS.C
	CC -C $(CFLAGS) -I$(M) -I. $(M)/XDEFS.C
COMM1.O: $(M)/MANIFEST $(M)/MFILE1 $(M)/COMMON MACDEFS $(M)/COMM1.C
	CC -C $(CFLAGS) -I. -I$(M) $(M)/COMM1.C
CGRAM.O: $(M)/MANIFEST $(M)/MFILE1 MACDEFS CGRAM.C
	CC -C $(CFLAGS) -I$(M) -I. CGRAM.C

CGRAM.C: $(M)/CGRAM.Y
	YACC $(M)/CGRAM.Y
	MV Y.TAB.C CGRAM.C

LLIB-PORT.LN: LLIB-PORT LPASS1
	-(/LIB/CPP -C -DLINT LLIB-PORT | ./LPASS1 -PUV > LLIB-PORT.LN )
LLIB-LM.LN: LLIB-LM LPASS1
	-(/LIB/CPP -C -DLINT LLIB-LM | ./LPASS1 -PUV > LLIB-LM.LN )
LLIB-LMP.LN: LLIB-LMP LPASS1
	-(/LIB/CPP -C -DLINT LLIB-LMP | ./LPASS1 -PUV > LLIB-LMP.LN )
LLIB-LC.LN: LLIB-LC LPASS1
	-(/LIB/CPP -C -DLINT LLIB-LC | ./LPASS1 -V > LLIB-LC.LN )
LLIB-LCURSES.LN: LLIB-LCURSES LPASS1
	-(/LIB/CPP -C -DLINT LLIB-LCURSES | ./LPASS1 -V > LLIB-LCURSES.LN )

LPASS2: LPASS2.O HASH.O
	CC LPASS2.O HASH.O -O LPASS2

LPASS2.O: $(M)/MANIFEST LMANIFEST
	CC $(CFLAGS) -C -I$(M) -I. LPASS2.C

LINTALL:
	LINT -HPV -I. -I$(M)  $(M)/CGRAM.C $(M)/XDEFS.C $(M)/SCAN.C \
		$(M)/PFTN.C $(M)/TREES.C $(M)/OPTIM.C LINT.C
INSTALL: ALL SHELL
	INSTALL -S LPASS1 /USR/LIB/LINT/LINT1
	INSTALL -S LPASS2 /USR/LIB/LINT/LINT2
	FOR I IN LLIB-*; DO INSTALL -C -M 644 $$I /USR/LIB/LINT; DONE
	INSTALL -C SHELL /USR/BIN/LINT
SHRINK:
	RM -F *.O

CLEAN: SHRINK
	RM -F LPASS1 LPASS2 CGRAM.C $(LINTLIBS)
@end(SmallExample)
@caption<MakeFile For @c<Lint>>
@tag<LintMakeFile>
]

The first part of the @c<Lint> MakeFile contains macro definitions.
These definitions are used to specify directories (e.g., @f0<M>),
compilation flags (e.g., @f0<CFLAGS>), and to group files (e.g.,
@f0<LINTLIBS>).  The target @F0<ALL> is used to name the major
subsystems of the @c<Lint>.  The next cluster of specifications
manages the first pass of @c<Lint>.  There is an entry for each
library file provided with @c<Lint>.  Each of these specifies that a
@c<Lint> library file is dependent upon a library source file and the
first pass of @c<Lint>.  Libraries depend on the first pass of
@c<Lint> because they are constructed by it.  The targets that specify
management for the second pass of @c<Lint> are @F0<LPASS2> and
@f0<LPASS2.O>.

The @F0<LINTALL>, @f0<INSTALL>, @f0<SHRINK>, and @F0<CLEAN> targets
are not grains at all, rather, they are used to initiate installation
and removal of @c<Lint>.  A request to @i<make> any of these will
always result in the associated command sequence being executed
because the corresponding files do not exist in the UNIX environment.
The use of non-existing grains to force command sequences to be
executed is a popular and useful feature of @c<make>.  The
functionality provided by these target grains is an example of how
construction tools can be used for more than just system construction.

@newpage
@SubSection (Deficiencies)

@b<Phrased in terms of construction.> The fundamental problem with
@c<make> is that it forces users to manipulate lists of construction
directives.  People do not normally think about systems in terms of
the steps used to construct them, and therefore these lists are
difficult to understand.  @c<make> should present a more natural user
interface and then work from the user supplied information towards the
construction information that it requires.

@c<make> does not include an adequate means for saving and reusing
common construction patterns.  The introduction of such a facility
would shorten MakeFiles since common patterns would be replaced with
single identifiers.  The definition of the identifier would document
and highlight the intended construction pattern.  The functionality
described in this paragraph is usually provided by a macro mechanism,
however the @c<make> macro facility is too simple -- it does not even
allow for parameterized macros.

@b<No underlying task descriptions.> Systems that keep knowledge about
construction separate from knowledge about systems can be extended by
adding to the construction knowledge without altering existing system
models.  Pitman @cite<Pitman> discusses the importance of separating
knowledge about systems from knowledge about construction tasks.
@c<make> does not use task descriptions at all and cannot be extended
without changing existing MakeFiles.

@b<Intermediate grains are referenced.> Maintainers can only change
systems by manipulating source grains or requesting that goal grains
be constructed.  Maintainers do not manipulate intermediate grains and
it would be nice if these grains did not need to appear in MakeFiles.

@b<All source grains need not be referenced.> @c<make> allows system
descriptions to omit source grains that are also goal grains since
there is no command sequence that uses or effects them.  For example,
there is nothing that forces UNIX Shell Scripts to be included in
MakeFiles.  The absence of references to Shell Scripts would be a
serious omission if someone were using a MakeFile to determine which
grains needed to be copied when transporting a system.
@newpage
@section (DEFSYSTEM)

@c<defsystem>@cite<LispMachine> is a construction directive based tool
that is used to install and maintain Lisp Machine software.  The
@c<defsystem> analog to @c<make>'s MakeFile is called a system
description.  @c<defsystem> system descriptions contain a mixture of
system modeling information and construction directives.
@c<defsystem> requires that command sequences (called transformations)
be formally defined before they are used; this is different from the
@c<make> approach of allowing unlimited use of UNIX command sequences.

System descriptions are made by @f0<DEFSYSTEM> macro.  Calls to
@F0<DEFSYSTEM> have the form:
@begin<Example>
(DEFSYSTEM @F1<SYSTEM-NAME>
  (@F1<KEYWORD ARGS> ...)
  (@F1<KEYWORD ARGS> ...)
  ...)
@end<Example>
The options selected by the keywords fall into two general categories:
properties of the system and transformations.

There are three main @c<defsystem> property keywords:
@begin(description)
@f0<:NAME>@\Specifies a "pretty" version of
@f1<SYSTEM-NAME> for use in printing.

@f0<:PATHNAME-DEFAULT>@\Specifies a local default within the
definition of the system for strings to be parsed into pathnames.

@f0<:MODULE>@\Assigns a name to a group of files within the system.
@end(description)

A transformation is an operation, such as compiling or loading, that
takes one or more files and performs some operation on them.  There
are two types of @c<defsystem> transformations: simple and complex.  A
simple transformation is a single operation on a module, such as
compiling it or loading it.  A complex transformation combines several
transformations; for example, compiling and then loading the results
of the compilation.

The general format of a simple transformation is:
@begin(Definition)
(@f1<NAME INPUT PRE-CONDITIONS>)
@end(Definition)
@begin(description)
@f1<NAME>@\The name of the transformation to be performed on the files
specified by @f1<INPUT>.  Examples of transformation names are
@f0(:FASLOAD) and @f0(:COMPILE-LOAD-INIT) (these transformations are
described below).

@f1<INPUT>@\A module or nested transformation.

@F1<PRE-CONDITIONS>@\Optional.  Specifies transformations that must
occur before the current transformation itself can take place.  The
format is either a list @f1<(NAME MODULE-NAMES ...)>, or a list of
such lists.  Each of these lists declares that the transformation
@f1<NAME> must be performed on the named modules before the current
transformation can take place. (The Lisp Machine documentation calls
pre-conditions @i(dependencies).)
@end(description)
@newpage

The following simple transformations are pre-defined:
@begin(description)
@f0<:FASLOAD>@\Loads the indicated file when a newer version of the
file exists than was read into the current environment.

@f0<:COMPILE>@\Compiles the indicated file when the source file has
been been updated since the compiled code file was written.
@end<description>

Unlike simple transformations, complex transformations do not have any
standard form.  The pre-defined complex transformations are:
@begin(description2)
@f0<:COMPILE-LOAD@ >@\Compiles and then loads the input files.  It has
the form:
@begin(example, Above 0, Below 0, LeftMargin +5) 
(:COMPILE-LOAD @f1<INPUT COMPILE-CONDITIONS LOAD-CONDITIONS>)
@end(example)
and is exactly the same as
@begin(example, Above 0, Below 0, LeftMargin +5)
(:FASLOAD (:COMPILE @f1<INPUT COMPILE-CONDITIONS) LOAD-CONDITIONS>)
@end(example)

@f0<:COMPILE-LOAD-INIT>@\Compiles and loads the input files.  This
transformation is sensitive to changes made to an additional
dependency list.  It has the form:
@begin(example, Above 0, Below 0, LeftMargin +5)
(:COMPILE-LOAD-INIT @f1<INPUT ADDITIONAL-DEPENDENCIES
 COMPILE-PRE-CONDITIONS LOAD-PRE-CONDITIONS>)
@end(example)
@f1<INPUT> will be compiled and loaded whenever its source file or any
of the modules listed in @f1<ADDITIONAL-DEPENDENCIES> are updated.
Note, the @f1<ADDITIONAL-DEPENDENCIES> field of this transformation
specifies the same kind of construction dependency as MakeFile entries
do.
@end<description2>

It is important to distinguish between transformation declarations and
transformation references.  Transformations are declared by keyword
lists in calls to @f0<DEFSYSTEM>.  Transformations are referenced in
pre-condition lists.  The transformations referenced in a
pre-condition list must be declared somewhere in the system
description.

@c<defsystem> contains a facility for defining new transformations.
New simple transformations are defined using the
@f0<DEFINE-SIMPLE-TRANSFORMATION> macro.  Calls have the form:
@begin(definition)
(DEFINE-SIMPLE-TRANSFORMATION @F1<NAME FUNCTION DEFAULT-CONDITION
			      INPUT-FILE-TYPES OUTPUT-FILE-TYPES>)
@end<definition>
@begin(description)
@f1<NAME>@\The name of the transformation being defined.

@f1<FUNCTION>@\A function to be called when the transformation is
performed.

@F1<DEFAULT-CONDITION>@\The function that is called in order to determine
if the transformation should be performed.

@F1<INPUT-FILE-TYPES>@\Specifies the types of the input files to the
transformation.  Lisp Machine file type specifications are filename
extensions (e.g., "lisp" or "bin").

@f1<OUTPUT-FILE-TYPES>@\Specifies the types of the output files produced by
the transformation.
@end(description)
For example, to define a simple transformation called @f0<:LISP-YACC>
that calls @f0<LISP-YACC> to derive parsers written in Lisp from BNF
grammars, the following definition could be made. (If a utility like
@c<yacc> were desired on the Lisp Machine it would probably be
implemented with a macro and not a separate parser generating tool.)
@begin<Example>
(DEFINE-SIMPLE-TRANSFORMATION :LISP-YACC #'LISP-YACC
  #'FILE-NEWER-THAN-FILE-P (:GRAMMAR) (:LISP))
@END<EXAMPLE>
@f0<LISP-YACC> will be invoked whenever the input file (i.e., the
grammar) is newer than the output file (i.e., the parser).  In other
words, the transformation will be performed whenever the source file
is updated.  Notice that this transformation relies on grain creation
dates in exactly the same way that @c<make> does.  

Complex transformations are defined as Lisp macros.  Here is the
definition of the @f0<:COMPILE-LOAD> transformation that was described
earlier:
@begin<Example, LeftMargin +0>
(DEFMACRO (:COMPILE-LOAD DEFSYSTEM-MACRO)
	  (INPUT &OPTIONAL COMPILE-PRE-CONDITIONS LOAD-PRE-CONDITIONS)
  `(:FASLOAD (:COMPILE ,INPUT ,COMPILE-PRE-CONDITIONS)
    ,LOAD-PRE-CONDITIONS))
@end<Example>

@SubSection <A Small Example -- TINYCOMP>

Figure @ref<TinySystemFile> contains the @c<defsystem> description for
a Lisp implementation of @C<tinycomp>.

@begin[figure]
@begin(Example, LeftMargin +0)
(DEFSYSTEM TINYCOMP
  (:MODULE DEFS "DEFINITIONS")
  (:MODULE PARSER "PARSER")
  (:MODULE CODE-GENERATOR "CODEGEN")
  (:MODULE LIBRARY "LIBRARY")

  (:FASLOAD DEFS)
  (:FASLOAD LIBRARY)
  (:COMPILE-LOAD-INIT CODE-GENERATOR  (DEFS) (:FASLOAD DEFS))
  (:COMPILE-LOAD-INIT (:LISP-YACC PARSER) (DEFS) (:FASLOAD DEFS)))

@end(Example)
@caption(@c<DEFSYSTEM> Description For @C<tinycomp>)
@tag(tinysystemfile)
@end[figure]

The @C<tinycomp> description contains a set of module definitions
followed by a series of transformations.  The transformations in the
description have the following interpretation:
@begin(description2)
@f0<(:FASLOAD DEFS)>@\Specifies that @f0<DEFS> should be loaded
whenever it is updated.  There are no pre-conditions to be satisfied
before the loading can take place.

@f0<(:FASLOAD LIBRARY)>@\Specifies that @f0<LIBRARY> should be loaded
whenever it is updated.  There are no pre-conditions to be satisfied
before the loading can take place.

@f0<(:COMPILE-LOAD-INIT CODE-GENERATOR (DEFS) (:FASLOAD DEFS))>@\
Specifies that @f0<CODE-GENERATOR> should be be compiled and loaded
whenever it or @f0<DEFS> changes.  Before the compilation can take
place, @f0<DEFS> must be loaded.

@f0<(:COMPILE-LOAD-INIT (:LISP-YACC PARSER) (DEFS) (:FASLOAD DEFS))>@\
Specifies that a parser derived from @f0<PARSER> is to be compiled and
loaded.  A new parser is produced whenever @f0<PARSER> changes.  The
compiler and loader are invoked whenever @f0<DEFS> or the derived
parser changes.  @f0<:LISP-YACC> will not be invoked if only @f0<DEFS>
changes.  Prior to compilation, @f0<DEFS> must be loaded.
@end(description2)

@SubSection <The Construction Process>

Systems previously modeled with @f0<DEFSYSTEM> are constructed by
calling @f0<MAKE-SYSTEM>.  Calls have the form:
@begin(Definition)
(MAKE-SYSTEM @f1<SYSTEM-NAME> &REST @F1<OPTIONS>)
@end(Definition)
@begin(description)
@f1<SYSTEM-NAME>@\Specifies a system previously modeled with
@F0<DEFSYSTEM>.

@f1<OPTIONS>@\Specifies options like @i<print the transformations that
would be done but don't do them> and so forth.
@end(description)

The construction dependency graph specified by the transformations and
pre-conditions in the @c<DEFSYSTEM> description of @f1<SYSTEM-NAME> is
analyzed in order to determine what construction needs to be done.
Each transformation is applied by first applying any transformations
referenced as pre-conditions, and then updating the input module if
it, or any modules listed in additional dependency lists, have been
changed.  Notice that the transformation applications are ordered by
the pre-condition lists.

Like @c<make>, @c<defsystem> uses simple functions based on file
creation dates in order to determine when a module should be
reconstructed.  However, unlike @c<make>, @c<defsystem> allows the
optional specification of predicates that control when construction is
done.  The new predicates can replace the simple ones that are
supplied with @c<defsystem>.

@c<defsystem> includes a patching facility.  It allows small changes
to be made to a system without invoking the @c<defsystem>
transformation/dependency mechanism.  Each set of changes is stored in
a patch file that typically contains new function definitions or
redefinitions of old functions.  Each patch is assigned a number.  If
a system contains patches, then the patches are loaded, in order,
after the unpatched version of the system is loaded.

@SubSection <An Extended Example -- LINT>

The @c<defsystem> description for a Lisp implementation of @c<Lint> is
presented in figure @ref<LintSystemFile>.  Although the @c<defsystem>
description is easier to understand than the corresponding
MakeFile (figure @ref<lintmakefile>), it is still difficult to
understand.

The @f0(:BUILD-LINT-LIBRARY) transformation is assumed to have been
defined and has the form:
@begin(Example)
(:BUILD-LINT-LIBRARY @f1<INPUT PRE-CONDITIONS>)
@end(Example)
It constructs @C<lint> library files from @C<lint> library sources.
The transformation allows the optional specification of pre-conditions,
and is applied if either @f1<INPUT>, or the first pass of @C<lint> is
updated.

@figure[
@begin(Example, LeftMargin +0)
(DEFSYSTEM LINT
  (:PATHNAME-DEFAULT "/USR/SRC/LIB/MIP")
  (:MODULE DEFINITIONS-1 ("MACDEFS" "MANIFEST" "MFILE1" "LMANIFEST"))
  (:MODULE DEFINITIONS-2 ("MANIFEST" "LMANIFEST"))
  (:MODULE PARSER "CGRAM")
  (:MODULE PASS1 ("XDEFS" "SCAN" "COMM1" "PFTN" "TREES" "OPTIM"
		  "LINT" "HASH"))
  (:MODULE PASS2 ("LPASS2" "HASH"))
  (:MODULE DRIVER "SHELL")
  (:MODULE LIBRARIES ("LLIB-PORT" "LLIB-LC" "LLIB-LM" "LLIB-LMP"
		      "LLIB-LCURSES"))

  (:FASLOAD DEFINITIONS-1)
  (:FASLOAD DEFINITIONS-2)
  (:COMPILE-LOAD DRIVER)
  (:COMPILE-LOAD-INIT PASS1 (DEFINITIONS-1) (:FASLOAD DEFINITIONS-1))
  (:COMPILE-LOAD-INIT PASS2 (DEFINITIONS-2) (:FASLOAD DEFINITIONS-2))
  (:COMPILE-LOAD-INIT (:LISP-YACC PARSER) (DEFINITIONS-1)
		      (:FASLOAD DEFINITIONS-1))
  (:BUILD-LINT-LIBRARY LIBRARIES (:FASLOAD DRIVER PARSER PASS1)))

@end<Example>
@caption<@c<DEFSYSTEM> Description For @C<lint>>
@tag<LintSystemFile>
]

The first keyword form in the @c<lint> @c<defsystem> description
specifies a system-wide default directory.  The next block of keyword
forms declare the various modules which comprise @c<lint>.  The final
block of forms declare the transformations used to construct @c<lint>.
Notice that as transformations are nested and pre-conditions are added,
the transformation declarations become increasingly difficult to
understand.

@SubSection <Deficiencies>

@b<Phrased in terms of construction.> Like @c<make>, @c<defsystem> is
a construction directive based tool.  This is the primary reason that
@c<defsystem> descriptions, although easier to understand than
MakeFiles, are still awkward.

One reason that @c<defsystem> descriptions are easier to understand
than MakeFiles is because @c<defsystem> is not purely construction
directive based.  @c<defsystem>'s @f0<:MODULE> declarations allow for
the logical grouping of grains into higher level modules.  This
grouping abstracts away from low level construction information, and
provides a more natural way for users to describe systems than
@c<make> does.

@c<defsystem> supports the sharing of common construction patterns
through the declaration of transformations.  This makes @c<defsystem>
system descriptions easier to produce and understand than MakeFiles.
However, since it is possible to avoid the declaration of a complex
transformation by using nested transformations, @c<defsystem> still
allows for common patterns to be repeated instead of shared.

@b<No underlying task descriptions.> Although @c<defsystem> has
embedded knowledge about Lisp compilation and loading it does not
include a mechanism for describing construction tasks and therefore
cannot be extended without great difficulty.

@b<Intermediate grains are referenced.> @c<defsystem> does not
differentiate between source, intermediate, and goal grains.  In
general, intermediate grains are hidden by complex transformations.
For example, there are no references to intermediate grains in figures
@ref<tinysystemfile> and @ref(lintsystemfile).  While @c<defsystem>
does not force intermediate grains to be included, it does not
prohibit them either.

@b<All source grains need not be referenced.> In a Lisp environment,
nothing can be used before it is loaded.  This means that any grain
that participates in a Lisp system will be involved in some
construction, and therefore, it is not as natural to omit a source
grain from a @c<defsystem> description as it is to omit one from a
MakeFile.  This difference between @c<make> and @c<defsystem> comes
from differences between the UNIX and Lisp environments, and not from
important differences between the two tools.

@section (Other Tools)

DeRemer and Kron introduced the terms programming-in-the-large and
programming-in-the-small @cite[DeRemer] to distinguish between the
writing of modules and the structuring of modules into systems.
Consistent construction is just one programming-in-the-large issue,
others include source code management, module interconnection
specification, and version control.  A brief summary of these other
issues and projects that focus upon them is presented here for
completeness.  The consistent construction components of these
projects do not differ from @c<make> or @c<defsystem> in any
significant way.

When several people are working on a system simultaneously, it is
important to regulate access to the source code modules in order to
ensure that someone does not attempt to modify a module while someone
else is modifying that same module.  A common scheme is to implement a
@i<librarian> that regulates access to system components via a
check-in/check-out mechanism.  In short, only one person is allowed to
check-out a module for update at any time.  Anyone can read a module
at any time.  Source code management systems are described in the
following papers @cite<SCCS,SMF,Pilot,DF>. 

All of the problems mentioned above are compounded if the programming
environment is distributed over a network.  Schmidt addresses these
issues@cite<Schmidt>.

It is often the case that there are families of systems being managed.
For example there may be several public releases of a system, internal
releases, experimental versions and so on.  It is also common for
there to be several versions of a system intended to run on different
hardware configurations.  Each member of a family of software systems
usually shares many components with other members of the family.
Maintainers of such families need to worry about which versions of
which modules are used in each member of the family.  Tichy and
Cooprider attacked the problems associated with the representation and
management of software families @cite<Cooprider, Tichy, RCS>.
 
