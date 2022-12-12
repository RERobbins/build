@part(Chapter4, root = "tr.mss")
@Chapter (Construction Requests and Task Graph Derivation)
@Label (Deriving)

After a system has been modeled with @f0<DEFMODEL>, @c<BUILD> can be
called upon to handle construction requests for it.  Each request has
the form:
@begin(definition)
(BUILD-REQUEST @f1<MODEL REQUEST> &OPTIONAL @f1<MODULE> (@f1<MODE> :NORMAL))
@end(definition)
@begin(description)
@f1<MODEL>@\The name of a model previously defined with @f0<DEFMODEL>.

@f1<REQUEST>@\The name of a request recognized by @c<build> (e.g
@f0<:COMPILE>, @f0<:LOAD>).

@f1<MODULE>@\The name of a module to operate upon.  If this field is
not specified then the default module for the system (as defined with
the @f0<:DEFAULT-MODULE> declaration form) is used.

@f1<MODE>@\Specifies one of several construction modes.  Construction
modes are discussed below.

@end(Description)

The prototype implementation of @c<build> has three construction modes
that behave as follows:
@begin(description)
@f0<:NORMAL>@\Describe all of the construction to be done, and then
ask the user if @c<build> should perform the construction just
described.

@f0<:DESCRIBE>@\Describe all of the construction to be done but do not
perform it.

@f0<:NO-CONFIRM>@\Perform the required construction without describing
it first.
@end(description)

Sample @c<build> requests are:
@begin(example)
(BUILD-REQUEST TINY-COMP :LOAD)
(BUILD-REQUEST LINT :LOAD DRIVER)
(BUILD-REQUEST LINT :LOAD DRIVER :DESCRIBE)
@end(example)

Once a request has been received, a three step process is executed for
each grain in the module stated in the request.  This process creates
a task model for the request which is then processed in the manner
outlined in chapter @ref<TASK>.  The three steps are:
@begin (enumerate)

Model the construction that can be deduced from the request without
considering any references.  This phase is called @i<pre-reference
request processing>.

Model the construction that is implied by the references that involve
the module associated with the request.  This phase is called
@i<reference processing>.

Model the construction that can be deduced from the request and the
graph built from the earlier steps.  This phase is called
@i<post-reference request processing>.
@end(enumerate)
After the post-reference processing has been completed the task graph
is complete and can be used to direct the construction needed to
handle the request.

Before the construction process can be explained in detail it is
necessary to present the functions used to view and manipulate task
graphs.

@Section (Viewing and Manipulating Task Graphs -- ACCESS)

Consider the following task graph:
@begin(verbatim, font SmallBodyFont, FaceCode F)
@tabclear
@tabdivide(5)
@blankspace(.50 in)
DEFS.LISP@\SOURCE @>BINARY@\@=DEFS.BIN@\BINARY @>IMAGE@\@=DEFS.IMAGE
@blankspace (1 line)
@\@=:LISP-COMPILE@\@\@=:LISP-LOAD-BIN
@blankspace (.50 in)
@tabclear
@end(Verbatim)
Starting at a p-node, the path to any of the g-nodes connected to one
of its ports can be specified by mentioning the name of the port
desired.  In the task graph above, starting at the @F0<:LISP-COMPILE>
p-node, the @i<step> @f0<BINARY> leads to @f0<DEFS.BIN>.

A step from a g-node to a p-node can be described by specifying the
process type of the connected p-node and the role played by the g-node
in the p-node.  In the sample task graph above, the step @f0<(BINARY
:LISP-COMPILE)> starting at @f0<DEFS.BIN> leads to the
@f0<:LISP-COMPILE> p-node.

@i<Paths> are formed by listing steps:
@begin(itemize)
The path @f0<((SOURCE :LISP-COMPILE) BINARY)> starting at @f0<DEFS.LISP>
leads to @f0<DEFS.BIN>.  

The path @f0<((SOURCE :LISP-COMPILE) BINARY (BINARY :LISP-LOAD-BIN))>
starting at @f0<DEFS.LISP> leads to the @f0(:LISP-LOAD-BIN) p-node.

The path@*
@f0<((SOURCE :LISP-COMPILE) BINARY (BINARY :LISP-LOAD) IMAGE)>@*
starting at @f0<DEFS.LISP> leads to @f0<DEFS.IMAGE>.

The path@*
@f0<((IMAGE :LISP-LOAD) BINARY (BINARY :LISP-COMPILE) SOURCE)>@*
starting at @f0<DEFS.IMAGE> leads to @f0<DEFS.LISP>.
@end(itemize)

The @f0<ACCESS> family of functions are designed to provide a
straightforward mechanism for both viewing and manipulating task
graphs.  These functions are used heavily during the task graph
derivation process.  There are three functions, @f0<ACCESS>,
@f0<ACCESS+>, and @f0<ACCESS*>, each of which is @f0<SETF>able.  The
@f0<ACCESS> functions have the form:

@begin(Definition)
(@f1<FUNCTION NODE PATH>)
@end(Definition)
@begin(description)
@f1<FUNCTION>@\@f0<ACCESS>, @F0<ACCESS+>, or @F0<ACCESS*>.

@f1<NODE>@\Either a p-node or a g-node.  This node is used as the root
of the path to be traced by @f1<ACCESS-FUNCTION>.

@f1<PATH>@\A list of steps to be traced from @f1<NODE>.
@end(description)
The functions behave in the following manner:
@begin(description)

@f0<ACCESS>@\Traces @f1<PATH> from @f1<NODE> and returns the last node
encountered.  An error is signalled if any step in @f1<PATH> cannot
be traced.  An error is signalled if there could be more than one node
that satisfies the path traced.

@f0<ACCESS+>@\Traces @f1<PATH> from @f1<NODE> and returns a list of
nodes that satisfy the path.  An error is signalled if any step in
@F1<PATH> cannot be traced.

@f0<ACCESS*>@\Traces @f1<PATH> from @f1<NODE> and returns the single
node that satisfies the path.  An error is signalled if there could be
more than one node that satisfies @f1<PATH>.  New nodes are created if
steps in @F1<PATH> do not exist.

@end(description)
Any @f0<ACCESS> call that returns a single node may be used to specify
the root of another call to @f0<ACCESS>, in other words, the following
two calls are equivalent:
@begin(example)
(ACCESS NODE (STEP1 STEP2 STEP3))
(ACCESS (ACCESS (ACCESS NODE STEP1) STEP2) STEP3)
@end(example)

Each of the @f0<ACCESS> functions can be @f0<SETF>ed.  Calls have the
form:
@begin(description2)

@f0<(SETF (ACCESS @f1<ROOT-NODE PATH>) @f1<END-NODE>)>@\Ensures that
future calls to @f0<ACCESS> with @f1<ROOT-NODE> and @f1<PATH>@*
(i.e., @f0<(ACCESS @f1<ROOT-NODE PATH>)>) will return @f1<END-NODE>.

@f0<(SETF (ACCESS+ @f1<ROOT-NODE PATH>) @f1<NODE-LIST>)>@\Ensures that
future calls to @f0<ACCESS+> with @f1<ROOT-NODE> and @f1<PATH>@*
(i.e., @f0<(ACCESS+ @f1<ROOT-NODE PATH>)>) will return @f1<NODE-LIST>.

@f0<(SETF (ACCESS* @f1<ROOT-NODE PATH>) @f1<END-NODE>)>@\Ensures that
future calls to @f0<ACCESS*> with @f1<ROOT-NODE> and @f1<PATH>@*
(i.e., @f0<(ACCESS* @f1<ROOT-NODE PATH>)>) will return @f1<END-NODE>.

@end(description2)

The @f0<ACCESS> functions differ in how they handle steps that cannot
be traced, and what they do when a path description fans out.  If
@f0<ACCESS> or @f0<ACCESS+> encounter a missing link, an error is
signalled.  @f0<ACCESS*> and the @f0<SETF> functions will create the
link and continue tracing the path.

A fanout condition occurs when an attempt it made to trace from a
@f0<:MULTIPLE> arity port of a p-node, or when more than one p-node
satisfies the role-name/process-type-name constraint tracing from a
g-node.  @f0<ACCESS, ACCESS*> and their associated @f0<SETF> functions
signal errors if fanout is encountered.  @f0<ACCESS+> will continue
tracing down all paths and returns the list of nodes that satisfied
the path description.  When @f0<SETF>ed, @f0<ACCESS+> will signal an
error if fanout is encountered before the last step in the path
description.

In Lisp Machine Lisp@cite<LispMachine> and Common
Lisp@cite<Common-Lisp>, the special form @f0<PUSH> can be used for
functions that are @f0<SETF>able.  @f0<PUSH> can be used to add a
g-node to a port.  For example:
@begin(example, above 0, below 0)
(PUSH SOME-G-NODE (ACCESS+ P-NODE SOME-PATH))
@end(example)
is equivalent to:
@begin(example, above 0)
(SETF (ACCESS+ P-NODE SOME-PATH)
      (CONS SOME-G-NODE (ACCESS+ P-NODE SOME-PATH)))
@end(example)

The @f0<SETF> forms and @f0<ACCESS*> can make additive changes to the
graph.  When a function needs to create a g-node and link it to a
p-node port, a name needs to be synthesized for the new g-node.  The
name of each g-node resembles a filename in that it has two parts, a
primary name and an extension.  In order to synthesize a g-node name,
the function copies the primary part from the grain attached to the
port specified as the @f1<NAME-SOURCE> port for the port being linked
to (see the paragraph about role descriptions in chapter @ref<task>).
An error is signalled if a function needs to derive a g-node name to
link to a port that has no @f1<NAME-SOURCE> port associated with it.
The extension of a g-node name is derived from its grain type object.
If the grain type represents files, then the extension is the
default-filename-extension, otherwise, it is the name of the grain
type itself.

@Section (Request Handlers)

Request handlers specify the task graph derivation steps that can be
taken whenever the request associated with the handler has been made,
without considering any reference declarations.  Requests are
identified with request signatures (much like reference signatures).
Each request signature contains two fields, a request name and a
grain type name.  For example the signature:
@begin(example)
<:COMPILE :LISP-SOURCE>
@end(example)
identifies the handler designed to build part of the task graph needed
to accomplish the compilation of a Lisp source grain.  The signature:
@BEGIN(example)
<:YACC :YACC-GRAMMAR>
@end(example)
identifies the handler that will build part of the task graph needed
to invoke @c<YACC> on a grammar.

Not all possible signatures will have handlers defined for them.  For
example the request signature:
@begin(example)
<:COMPILE :LISP-BINARY>
@end(example)
identifies a nonsensical request.

Pre-reference request handlers are used to construct the parts of a
task graph which will be needed regardless of the ramifications of
references.  For example, in order to model the compilation of some
@f0<:LISP-SOURCE> grain, @f0<G.LISP>, the following links can be made
without considering any references; the g-node representing
@f0<G.LISP> should be linked to the @f0<SOURCE> port of a
@f0<:LISP-COMPILE> p-node, and then the @f0<BINARY> port of this
p-node should be linked to a g-node representing the binary version of
@f0<G.LISP> (i.e., @f0<G.BIN>).

@BEGIN(VERBATIM, font SmallBodyFont, FaceCode F)
@tabclear
@tabdivide(4)
@blankspace(.30 in)
@=G.LISP@\SOURCE @>BINARY@\@=G.BIN@\
@blankspace (1 line)
@\@=:LISP-COMPILE@\@*
@blankspace (.30 in)
@tabclear
@end(Verbatim)

Post-reference request handlers are used for modeling processing that
can only be deduced after the implications of the references are added
to the task graph.  At this time it has not been necessary to use a
post reference handler, however, they are included because there may
be situations where their use is appropriate.

@SubSection (Defining Request Handlers)

Request handlers are defined with @F0<DEFINE-REQUEST-HANDLER>.  Calls
have the form:
@begin(definition)
(DEFINE-REQUEST-HANDLER @f1<(REQUEST GRAIN-TYPE-NAME PRE-OR-POST)>
                        @f1<(ARGS)>
   &BODY @F1<BODY>)
@end(definition)
@begin(description)
@f1<REQUEST>@\The name of the request being handled.

@f1<GRAIN-TYPE-NAME>@\The type of the grain that the handler is for.

@f1<PRE-OR-POST>@\@f0<:PRE> indicates that this is a pre-reference
handler.  @f0<:POST> indicates that this is a post reference handler.

@newpage
@f1<ARGS>@\The names of the variables passed to the handler.  There
must be at least one element in this list.  The first @f1<ARG> will be
bound to the g-node associated with the request when @f1<BODY> is
evaluated.

@f1<BODY>@\The forms that constitute the handler.  They are evaluated
with the arguments passed to the handler bound to the variables named
in @f1<ARGS>.

@end(description)
All requests made by users have a single argument, the name of the
module that the request is intended for.  Handlers may also make
requests, and these requests can contain more than one argument.  The
handlers for the @f0<:LOAD+> and @f0<:INCLUDE+> tasks presented in
Appendix @ref<AppendixA> are examples of handlers using additional
arguments.

Figure @ref<LispRequestDefs> contains the request handler definitions
for Lisp compilation and loading.  The first handler is invoked when a
@f0<:COMPILE> request is made on a @f0<:LISP-SOURCE> module.  It uses
@f0<ACCESS*> to ensure that the task graph being derived models the
fact that the @f0<:LISP-SOURCE> grains in the module need to be
compiled.

The second handler is invoked when a @f0<:LOAD> request is made on a
@f0<:LISP-SOURCE> module.  The first thing that the handler does is to
initiate a @f0<:COMPILE> request on each of the grains in the
@f0<:LISP-SOURCE> module, and then it models the fact that the
@f0<:BINARY> grains produced by compilation need to be loaded.

Handlers ensure that task graph paths exist.  After a handler has been
invoked on a grain once, additional invocations will have no effect.
Therefore, task definers need only be concerned that the proper
handlers are invoked at least once and do not need to worry about
additional invocations.

@begin(figure)
@begin(example, LeftMargin +0)

(DEFINE-REQUEST-HANDLER (:COMPILE :LISP-SOURCE :PRE) (SOURCE-NODE)
  (ACCESS* SOURCE-NODE ((SOURCE :LISP-COMPILE) BINARY)))

(DEFINE-REQUEST-HANDLER (:LOAD :LISP-SOURCE :PRE) (SOURCE-NODE)
  (PROCESS-REQUEST :COMPILE SOURCE-NODE)
  (ACCESS* SOURCE-NODE ((SOURCE :LISP-COMPILE) BINARY
			(BINARY :LISP-LOAD-BIN) IMAGE)))
@end(example)
@caption(Request Handler Definitions for Lisp)
@tag(LispRequestDefs)
@end(figure)

@Section (Reference Handlers)

Reference handlers realize the implications references upon
construction graphs.  The construction implications of a reference
depend upon the kind of reference, the request, and which part of the
reference (right or left) the module participating in the request
belongs to.  Each handler is identified by a reference handler
signature that includes five fields: the three fields from the
reference signature, the request name, and a participation marker
(either @F0<:RIGHT> or @f0<:LEFT>).  Sample signatures are:
@begin (example)
<<:CALLS :LISP-SOURCE :LISP-SOURCE> <:LOAD :LEFT>>
<<:CALLS :C-SOURCE :C-SOURCE> <:COMPILE :RIGHT>>
<<:MACRO-CALLS :LISP-SOURCE :LISP-SOURCE> <:COMPILE :LEFT>>
@end (example)

Not all references are relevant to every request made.  For instance,
the reference
@begin(example)
(:CALLS LISP-SOURCE-1 LISP-SOURCE-2)
@end(example)
has no implications when a request is made to compile
@f0<LISP-SOURCE-1>.  However, if the request is to load
@f0<LISP-SOURCE-1> for execution, then the reference implies that
@f0<LISP-SOURCE-2> needs to be loaded.  It is also important to
recognize that the direction of the reference matters.  For example,
the reference above has implications when @f0<LISP-SOURCE-1> is
loaded, but, it has none when @f0<LISP-SOURCE-2> is loaded.

@SubSection (Defining Reference Handlers)

Reference handlers are defined with @f0<DEFINE-REFERENCE-HANDLER>.
Calls have the form:
@begin(Definition)
(DEFINE-REFERENCE-HANDLER @f1<((REFERENCE LEFT-TYPE RIGHT-TYPE)>
                          @f1< (REQUEST DIRECTION))>
			  @f1<(ARGS)>
  &BODY @f1<BODY>)
@end(Definition)
@begin(description)
@f1<REFERENCE>@\The name of the reference being handled.

@f1<LEFT-TYPE>@\The grain type of the left (first) module in the
reference.

@f1<RIGHT-TYPE>@\The grain type of the right (second) module in the
reference.

@f1<REQUEST>@\The name of the request being handled.

@f1<DIRECTION>@\Either @f0<:LEFT> or @f0<:RIGHT>.  This field
identifies the module that the request being handled refers to.

@f1<ARGS>@\The names of the variables passed to the handler, these
will be bound when @f1<BODY> is evaluated.  There must be at least two
elements in this list.  The first @f1<ARG> will be bound to the left
grain of the reference.  The second @f1<ARG> will be bound to the
right grain of the reference.

@f1<BODY>@\The forms that constitute the handler.  They are evaluated
with the arguments passed to the handler bound to the variables named
in @f1<ARGS>.

@end(description)

Figure @ref<LispReferenceDefs> contains reference handler definitions
for Lisp compilation and loading.  The first handler models the fact
that the grain represented by @f0<CALLED-NODE> needs to be loaded, and
that the resulting @f0<:LISP-IMAGE> grain plays the role
@f0<DEFINITIONS> in the compilation of the grain represented by
@f0<CALLING-NODE>.  The second handler ensures that the grain
represented by @f0<:CALLED-NODE> is loaded.  Note, while these
handlers are sufficient to handle the common module interactions for
Lisp systems, they are not sufficient to handle all of the ways that
Lisp modules may interact.  More handlers would need to be defined in
order to properly handle all of the ways that Lisp modules can
interact.  The prototype implementation of @c<build> does not include
these additional handlers at this time.

@c<build> guarantees that reference handlers are invoked after
pre-reference request processing and therefore handler writers may
safely assume that the effects of pre-reference request handlers will
already be present in the graph.  For example, the @f0<:MACRO-CALLS>
handler discussed above assumes that the compilation of
@f0<CALLING-NODE> has already been modeled.

@section (A Task Description Definition Example)

This section presents an example of a task description definition.
The task defined is called @f0<:LIST-SOURCE-CODE> and it will produce
formatted source code listings for a @f0<:LISP-SOURCE> module and any
@f0<:LISP-SOURCE> modules that it references.  All of the defining
forms for @f0<:LIST-SOURCE-CODE> are in figure
@ref(Source-Code-Task-2).

First, the @f0<:LIST-LISP-SOURCE> process type is defined.  Instances
of this type have a single input role called @f0<SOURCE> and a single
output role called @f0<LISTING>.  The function @f0<LIST-LISP-FILE> is
called to produce the grain filling the output role from the grain
filling the input role.  The request handler for the task is very
simple, it models the fact that the source grain to be listed will
play the role @f0<SOURCE> in a @f0<:LIST-LISP-SOURCE> p-node and that
a g-node should be attached to the @f0<LISTING> role of that same
p-node.  The two reference handlers specify that grains which are
called by a grain being listed should themselves be listed.

@f0<:LIST-SOURCE-CODE> shows the virtue of keeping system models
separate from information about tasks; once its defining forms are
evaluated, formatted listings may be obtained for any previously
modeled Lisp system without altering any system models.

@begin(figure)
@blankspace (1 line)
@begin(example, LeftMargin +0, RightMargin -5)
(DEFINE-REFERENCE-HANDLER ((:MACRO-CALLS :LISP-SOURCE :LISP-SOURCE)
			   (:COMPILE :LEFT))
			  (CALLING-NODE CALLED-NODE)
  (PROCESS-REQUEST :LOAD CALLED-NODE)
  (PUSH (ACCESS CALLED-NODE ((SOURCE :LISP-COMPILE) BINARY
			     (BINARY :LISP-LOAD-BIN) IMAGE))
	(ACCESS+ CALLING-NODE ((SOURCE :LISP-COMPILE) DEFINITIONS))))

(DEFINE-REFERENCE-HANDLER ((:CALLS :LISP-SOURCE :LISP-SOURCE)
			   (:LOAD :LEFT))
			  (IGNORE CALLED-NODE)
  (PROCESS-REQUEST :LOAD CALLED-NODE))
@end(example)
@caption(Reference Handler Definitions for Lisp)
@tag(LISPReferenceDefs)
@end(figure)

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
@TAG(Source-Code-Task-2)
@end[figure]
