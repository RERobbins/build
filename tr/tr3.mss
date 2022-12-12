@part(Chapter3, root = "tr.mss")
@Chapter (The BUILD Task Level)
@Label (Task)

This chapter describes the task level representation of systems used
by @c<build>.  A task level model is derived from the reference level
model for each request that @c<build> receives.  The derived model is
then used to handle the request.  (The phrase @i<task level> is used
in place of the more specific phrase @i<construction level> because
@c<build> is used for more than just construction.)

@c<build> task level models are acyclic directed graphs with two kinds
of nodes; @i<g-nodes> which represent grains, and @i<p-nodes> which
represent the processes used to construct grains.  Leaf nodes
represent source grains, and root nodes represent goal grains.  The
link between grains and the processes that use them is modeled by
linking the g-nodes representing grains to the p-nodes representing
the processes that use them.

Figure @ref<SimpleTaskGraph> contains a portion of the task graph used
to represent the compilation of @f0<PARSER.LISP>, a grain from a Lisp
implementation of @c<tinycomp>.  This example assumes that
@f0<PARSER.LISP> is a source grain and ignores the fact that in
@c<tinycomp>, @f0<PARSER.LISP> is an intermediate module produced by
@f0<LISP-YACC>.  The ellipses represent g-nodes and the rectangles
represent p-nodes.  There are two source nodes, @f0<PARSER.LISP> and
@f0<DEFS.LISP>, and a single goal node, @f0<PARSER.IMAGE>.

Although the use of an acyclic directed graph to represent task
processing is not unique (@c<make> and @c<defsystem> use similar
representations) the derivation of task graphs from reference models
is novel.

@begin[figure]
@begin(verbatim, font SmallBodyFont, FaceCode F)
@tabclear
@tabdivide(5)
@blankspace(.5 in)
DEFS.LISP@\COMPILE@\DEFS.BIN@\LOAD@\DEFS.IMAGE
@blankspace (.75 in)
PARSER.LISP@\COMPILE@\PARSER.BIN@\LOAD@\PARSER.IMAGE
@blankspace (.5 in)
@tabclear
@end(Verbatim)
@caption(Simple Task Graph)
@tag<SimpleTaskGraph>
@end[figure]

@Section (Grain types)

Grain type objects are used to represent the classes of grains used by
the environment that @c<build> is working with.  They are used to
represent all of the kinds of grains that are manipulated by the
underlying environment, whether they are files or not.  For instance,
the grain type @f0<:LISP-IMAGE> is used to represent the objects that
result from loading files into the Lisp environment.

@SubSection (Defining Grain Types)

Grain types are defined with @f0<DEFINE-GRAIN-TYPE> and definitions
have the form:
@begin<Definition>
(DEFINE-GRAIN-TYPE @f1<NAME> &OPTIONAL @f1<FILENAME-EXTENSION>)
@end(Definition)
@begin(description)
@f1<NAME>@\The name of the grain type being defined.

@f1<FILENAME-EXTENSION>@\The default filename extension for grains of this
type.  If this field is null then @c<build> assumes that grains of
this type are not files.
@end(description)

Figure @ref<LispGrainDefs> contains the grain type definitions used to
model Lisp systems.  The @f0<:LISP-SOURCE> and @f0<:LISP-BINARY> grain
types correspond to files and hence their definitions include default
filename extensions (the Lisp Machine uses keyword symbols to
represent filename extensions).  The @f0<:LISP-IMAGE> grain type is
not associated with files and therefore has no default filename
extension.

@begin(figure)
@begin(example)
(DEFINE-GRAIN-TYPE :LISP-SOURCE :LISP)
(DEFINE-GRAIN-TYPE :LISP-BINARY :BIN)
(DEFINE-GRAIN-TYPE :LISP-IMAGE)
@end(example)
@caption(Grain Type Definitions for Lisp)
@tag(LispGrainDefs)
@end(figure)

@Section (G-nodes)

G-nodes represent grains in task graphs, they contain the following
information:
@begin(description)
@F1<NAME>@\The name of the grain represented by this g-node.

@f1<TYPE>@\The grain type object that the grain represented by this g-node
is an instance of.

@f1<MODULE>@\Optional.  The module that includes the grain represented
by this g-node.

@f1<CREATOR>@\Optional.  The p-node that represents the process that
created this g-node.  This field will be null if the g-node represents
a source grain.

@f1<USERS>@\A list of p-nodes that depend on this g-node to fill an input role.

@f1<INGREDIENTS>@\A list that represents the source grains used to
produce this g-node.  Each element of the list is a pair containing
the name and creation-date of an ingredient grain.

@F1<CREATE-DATE>@\A time stamp that represents the time and date when the
grain that is represented by this g-node was created.
@end(description)

@Section (Process Types)

Process type objects contain the information pertaining to classes of
process instances (represented by p-nodes).  For example, the Lisp
Machine implementation of @c<build> includes process type objects for
Lisp compilation and Lisp binary file loading.  

The grains that are used and produced by processes are partitioned
according to the roles that they play in them.  Grains that processes
use are said to play @i<input roles>.  Grains that are produced by
processes are said to play @i<output roles>.

Process type objects contain role descriptions for each of their input
and output roles.  Role descriptions contain the following
information:
@begin(description)

@f1<NAME>@\The name of the role.  It must be unique within the process
type being defined.

@f1<GRAIN-TYPE>@\The grain type name that grains filling this role must
have.

@f1<ARITY>@\Either @f0<:SINGLE> or @f0<:MULTIPLE>.  A role with arity
@f0<:SINGLE> can have no more than one grain filling it.  A role with
arity @f0<:MULTIPLE> can have an arbitrary number of grains filling it.

@f1<NAME-SOURCE>@\Optional.  The name of a role used to help
derive names for grains that will fill this role.
@end(description)

@SubSection (Defining Process Types)

Process types are defined with @f0<DEFINE-PROCESS-TYPE> and calls
have the form:
@begin<Definition>
(DEFINE-PROCESS-TYPE @F1<NAME INPUT-SPEC OUTPUT-SPEC STREAM-VAR
		     DESCRIBE-FORM> &REST @F1<CONSTRUCT-FORMS>)
@end(Definition)
@begin(description)
@f1<NAME>@\The name of the process type.

@f1<INPUT-SPEC>@\A list of input role descriptions (discussed above). 

@f1<OUTPUT-SPEC>@\A list of output role descriptions.

@f1<STREAM-VAR>@\A variable name that will be bound to the output
stream when @f1<DESCRIBE-FORM> and @f1<CONSTRUCT-FORMS> are evaluated.

@f1<DESCRIBE-FORM@ >@\A form to be evaluated in order to describe the
processing represented by an instance of this process type.  When the
form is evaluated, each role-name will be bound to the names of the
grains playing the role.  Also, the symbol named by @f1<STREAM-VAR>
will be bound to the output stream.

@f1<CONSTRUCT-FORMS>@\The forms to be evaluated in order to accomplish
the processing represented by an instance of the process type.  When
these forms are evaluated each of the role-names and the symbol named
by @f1<STREAM-VAR> will be bound as mentioned above.

@end(description)

Figure @ref<LispProcessDefs> contains the process type definitions for
Lisp compilation and Lisp binary loading.  The definition for
@f0<:LISP-COMPILE> specifies that there are two input roles,
@f0<SOURCE> and @f0<DEFINITIONS>, and a single output role,
@f0<BINARY>.  @f0<SOURCE> has singular arity and must be filled by a
@f0<:LISP-SOURCE> grain.  @f0<DEFINITIONS> has multiple arity and can
only be filled by @f0<:LISP-IMAGE> grains.  @f0<BINARY> has singular
arity and must be filled by a @f0<:LISP-BINARY> grain.  The describe
form produces descriptions like:
@begin(example)
"COMPILE PARSER.LISP"
@end(example)
The construct forms produce the grain playing the @f0<BINARY> role by
compiling the grain playing the @f0<SOURCE> role.  The construct forms
also cause a notification of the compilation to be sent to the output
stream.  The notification looks like:
@begin(example)
"COMPILING PARSER.LISP.5"
@end(Example)

@begin(figure)
@begin(Example, LeftMargin +0)

(DEFINE-PROCESS-TYPE :LISP-COMPILE
    ((SOURCE :LISP-SOURCE :SINGLE)	      ;SOURCE INPUT ROLE
     (DEFINITIONS :LISP-IMAGE :MULTIPLE))     ;DEFINITIONS INPUT ROLE
    ((BINARY :LISP-BINARY :SINGLE SOURCE))    ;BINARY OUTPUT ROLE
    OUTPUT-STREAM			      ;STREAM-VAR
    (FORMAT OUTPUT-STREAM "~%COMPILE ~A"      ;DESCRIBE-FORM
	    (PATHNAME-MINUS-VERSION SOURCE))	
  (FORMAT OUTPUT-STREAM "~%COMPILING ~A" SOURCE)     ;CONSTRUCT-FORMS
  (COMPILER:COMPILE-FILE SOURCE BINARY))

(DEFINE-PROCESS-TYPE :LISP-LOAD-BIN
    ((BINARY :LISP-BINARY :SINGLE)	      ;BINARY INPUT ROLE
     (DEFINITIONS :LISP-IMAGE :MULTIPLE))     ;DEFINITIONS INPUT ROLE
    ((IMAGE :LISP-IMAGE :SINGLE BINARY))      ;IMAGE OUTPUT ROLE
    OUTPUT-STREAM			      ;STREAM-VAR
    (FORMAT OUTPUT-STREAM "~%LOAD ~A"	      ;DESCRIBE-FORM
	    (PATHNAME-MINUS-VERSION BINARY))	
    (FORMAT OUTPUT-STREAM "~%LOADING ~A" BINARY)     ;CONSTRUCT-FORMS
  (SI:LOAD-BINARY-FILE BINARY NIL T))
@end(Example)
@Caption<Process Type Definitions For Lisp>
@tag<LispProcessDefs>
@end(Figure)

Processes often depend on grains not explicitly mentioned in their
invocations.  For example, in languages that rely on objects to be
specified or loaded before objects that refer to them can be compiled,
the compilation process type must include a role that is used to
capture that dependency.  The role @f0<DEFINITIONS> is used in
@f0<:LISP-COMPILE> in order to express the need for some things to be
defined before a Lisp grain can be compiled.  The link between the
g-node for @f0<DEFS.IMAGE> and the p-node representing the compilation
of @f0<PARSER.LISP> in the task model from figure
@ref<SimpleTaskGraph> is an example of such a dependency being
modeled.  Another situation in which it is necessary to model a
dependency not made explicitly in command line invocation is for C
compilation.  The @f0<:C-COMPILE> process type has the role
@f0<INCLUDE> to represent the dependency between a file and the files
that it includes via the C @f0<#INCLUDE> mechanism.

@Section (P-Nodes)

Each p-node represents a process to be invoked on the grains attached
to its input ports to produce the grains attached to its output ports.
Each role in a process type is represented as a port in p-nodes of
that type.  The grain type of each g-node attached to a port must be
the same as the grain type associated with the role.  A description of
the processing represented by a p-node and the g-nodes attached to its
ports can be produced by applying @f1<DESCRIBE-FORM> from the p-node's
process type object to the p-node.  The processing represented by the
p-node can be done by applying @f1<CONSTRUCT-FORMS> from the p-node's
process type object to the p-node.

Figure @ref<SamplePNode> contains an expanded view of the p-node used
to represent the compilation of @f0<PARSER.LISP> in @c<tinycomp>.

@begin[figure]
@begin(verbatim, font SmallBodyFont, FaceCode F)
@tabclear
@tabdivide(5)
@blankspace(.50 in)
DEFS.IMAGE@\DEFINITION
@blankspace (.25 in)
@\@\:LISP-COMPILE@\BINARY@\PARSER.BIN
@blankspace (.25 in)
PARSER.LISP@\SOURCE
@blankspace (.75 in)
@tabclear
@end(Verbatim)
@caption(Expanded P-Node)
@tag<SamplePNode>
@end[figure]

@Section (Task Graph Constraints)

Task graphs are constrained in the following ways:
@begin(enumerate)

Task graphs are acyclic.  A cycle in a graph would imply that some
grain was needed in order to construct itself.

The parent of a g-node, if there is one, must be a p-node.

A g-node can have no more than one parent.  

A g-node without a parent represents a source grain.

The children of a g-node, if there are any, must be p-nodes.  These
nodes represent processes that depend upon the grain represented by
the g-node.

A g-node without children represents a goal grain.

The children of a p-node must be g-nodes.  These g-nodes represent
grains derived by the process represented by the p-node.  Each p-node
must have at least one child.
@end(enumerate)
In other words, task graphs are acyclic graphs which begin with
g-nodes that represent source grains and end with g-nodes that
represent goal grains.  The g-nodes are separated by p-nodes that
represent the processes that derive later g-nodes from earlier ones.

Figures @ref<INTRO-TINY-CONSTRUCTION-GRAPH>,
@ref<INTRO-TINY-CONSTRUCTION-GRAPH-2>, and @ref<SimpleTaskGraph> are
examples of well formed task graphs.

@Section (The Construction Algorithm)

Figure @ref<BuildAlgorithm> contains the algorithm used by @c<build>
to perform the construction modeled by a task graph.  This algorithm
is similar to the one used by @c<make> and @c<defsystem> (figure
@ref<LispMake>), the primary difference between the two algorithms is
in how they make use of creation dates to determine when construction
is necessary.  The @c<make> algorithm uses file creation date ordering
between input and output grains in order to infer that an input has
changed (and therefore construction is triggered).  In practice this
method works, however, it relies on several assumptions that are not
necessarily true.

@c<make> and @c<defsystem> assume that files with the same name but
different extensions are related.  For instance, they assume that
@f0<MAIN.O> was created by compiling @f0<MAIN.C>.  While this is a
reasonable assumption, it does not have to be true.  Nothing prevents
users from renaming files and therefore, there is no guarantee that
@f0<MAIN.O> actually came from @f0<MAIN.C>.  

If an output grain contains a file creation date that is newer than
all of the input grains used to produce it, then @c<make> and
@c<defsystem> assume that the output grain does not need to be
rebuilt.  However, there is no guarantee that file creation dates have
not been tampered with.

@c<build> does not use file creation date ordering to infer that an
object has changed.  @c<build> compares a grain's ingredient list with
the ingredient list that would result if the processing modeled by the
task graph were done.  If the ingredient lists match, then the
construction is not done.

The prototype implementation of @c<build> keeps a separate data file
that contains grain creation dates and ingredients.  Such a file would
not be needed if the underlying environment recorded the ingredients
used to produce an object.  The Mesa environment@cite<Mesa,Schmidt>
keeps this information and exploits it in order to determine when
processing needs to be done.

@begin(figure)
@begin(example)
(DEFUN CONSTRUCT-G-NODE (G-NODE)
  (COND ((SOURCE-NODE-P G-NODE) T)
	((OR (NON-EXISTENT G-NODE) (INGREDIENTS-CHANGED G-NODE))
	 (MAPCAR #'CONSTRUCT-G-NODE (INPUTS (PARENT G-NODE)))
	 (DO-CONSTRUCTION (PARENT G-NODE)))))

(DEFUN INGREDIENTS-CHANGED (G-NODE)
  (NOT (EQUAL (INGREDIENTS G-NODE)
	      (DERIVE-INGREDIENTS G-NODE))))

(DEFUN SOURCE-NODE-P (G-NODE)
  ;; RETURNS T IF AND ONLY IF G-NODE
  ;; REPRESENTS A SOURCE GRAIN
  )

(DEFUN NON-EXISTENT (G-NODE)
  ;; RETURNS T IF THE GRAIN REPRESENTED BY G-NODE
  ;; DOES NOT EXIST
  )

(DEFUN PARENT (G-NODE)
  ;; RETURN THE PARENT P-NODE OF G-NODE
  )

(DEFUN INPUTS (P-NODE)
  ;; RETURN THE INPUT G-NODES OF P-NODE
  )

(DEFUN DO-CONSTRUCTION (P-NODE)
  ;; PERFORM CONSTRUCTION REPRESENTED BY P-NODE
  )

(DEFUN INGREDIENTS (G-NODE)
  ;; RETURN THE INGREDIENT LIST USED TO CONSTRUCT 
  ;; THE EXISTING VERSION OF G-NODE
  )

(DEFUN DERIVE-INGREDIENTS (G-NODE)
  ;; RETURN THE INGREDIENT LIST THAT WOULD RESULT IF
  ;; A NEW VERSION OF G-NODE WERE CONSTRUCTED
  )

@end(example)
@caption(@c[build] Construction Algorithm)
@tag<BUILDAlgorithm>
@end[figure]
