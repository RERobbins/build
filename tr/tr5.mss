@part(Chapter5, root = "tr.mss")
@Chapter (Reprise)
@Label (Reprise)

This chapter highlights several aspects of @c<build> that have been
presented in this report.  The first section summarizes how @c<build>
overcomes the difficulties associated with existing tools (see chapter
@ref<SystemConstructionTools>).  The second section discusses
@c<build>'s construction framework and how it provides a base for
describing new tasks within a static framework that conceals many low
level details from the task definer.  The final section proposes ways
that @c<build> could be extended to provide capabilities not found in
existing tools.

@section (BUILD Compared With Existing Tools)

@b<Phrased in terms of inter-module references.> The @c<build> system
modeling mechanism allows users to describe systems in terms that are
natural for them. @c<build> system models are easier to understand and
they provide more information than the construction directive lists
used by existing tools.

@b<User definable task descriptions.> @c<build>'s task description
mechanism is responsible for the fact that @c<build> is not
constrained to some embedded set of tasks.  By separating system
models and task descriptions, @c<build>'s knowledge about construction
can be modified without requiring that system models be changed.
However, if a new task is sensitive to a class of references
previously ignored, then existing models will have to be updated.

@b<Intermediate grains are not referenced.> The only grains that are
referred to in a system are the source grains that comprise modules.
While intermediate grains are used in @c<build>'s task graphs, these
grains never appear in system models.

@b<All source grains must be referenced.> All of the source grains
that participate in a system either reference other grains in the
system or are referenced by other grains in the system.  Therefore,
since @c<build> models encode system referencing patterns, all of the
source grains in a system must appear in any well formed @c<build>
model of that system.

@section (BUILD's Construction Framework)

@c<build> provides procedures which guide the construction process.
These procedures include hooks for the components of user supplied
task descriptions.  The set of fixed procedures take care of low level
construction details that are common to all tasks and allow task
definitions to contain just the details that are relevant to the
particular task being defined.

The task graph representation and analysis algorithm provide a uniform
way to describe and perform system maintenance tasks.  New process
types and grain types can easily be integrated into task graphs.

The @f0<ACCESS> family of functions provide a general way for viewing
and manipulating task graphs that isolates handler definitions from
the low level mechanics of instantiating nodes, matching grain types
between g-nodes and p-node ports, and actually linking nodes together.

The task graph derivation algorithm ensures that pre-reference request
handlers are invoked before reference handlers and that reference
handlers are invoked before post-reference request handlers.  This
algorithm is also responsible for translating module references into a
series of handler invocations, one for each grain involved in a
reference.  Finally, the task graph derivation algorithm ensures that
circular references (i.e., @f0<(:CALLS A B) (:CALLS B A)>) do not
cause infinite loops during reference handling.

@c<build>'s construction framework allows task definers to concentrate
on the significant details of the task being defined (e.g., what
process and grain types are used, what references are relevant and how
should they be handled etc.) and isolates them from low level details
(e.g., task graph analysis, node instantiation etc.).

@section (Extensions to BUILD)

@c<build> provides a more graceful way of modeling systems than
existing tools, yet it does not provide greater capabilities.  This
section proposes extensions to @c<build> that would allow it to provide
a set of facilities that other tools do not.  The extensions are
automatic derivation of system specifications from source code,
support for patching and similar maintenance styles, and the
incorporation of the nature of module change into the reconstruction
algorithms.

@subsection(Automatic Derivation of System Descriptions)

The @c<build> modeling mechanism provides a natural way to describe
systems but it does not ensure that the descriptions are complete or
correct.  Designers are still required to generate system models by
hand.  A tool that could derive system models from source code would
relieve designers of the chore of building system description files.

For simple languages, an analyzer could build a great deal of the
model and locate areas that might present difficulties.  For example,
in most C systems all of the dependencies are caused by use of the
@f0<#INCLUDE> compiler directive and calls to externally defined
symbols -- the reference assertions from these references could be
synthesized automatically.

While there may be programming environments in which it is possible to
mechanize the derivation of system models there are certainly
languages for which such derivation would become arbitrarily complex.
For example, Pitman develops an argument against automatic derivation
of Lisp system models based on the complications caused by
macros@cite<Pitman>.

@subsection(Patching)

There are many instances where a system maintainer may want to
introduce changes into a system without making sure that the resulting
system is consistent; for example, debugging experiments where small
changes are introduced to examine some small part of the system.
These changes may not be intended to become part of a released system,
it may even be known that they will cause compilation of some other
module to fail.  Another instance where the ability to patch a system
is important is when a quick fix is being attempted and it is
important that the effects be seen quickly.  This kind of change
represents a tentative guess on the part of the maintainer.  The
introduction of such changes into systems must be supported by system
management tools if such tools are going to help and not hinder
maintainers.

The @c<defsystem> patch facility provides some support for producing
inconsistent systems.  Unfortunately, the @c<defsystem> patching
facility makes no use of the dependency information that the rest of
the tool uses.  No analysis of the effect of a patch is available.
Nothing guarantees that a patch will even be loaded correctly
according to the dependency information that is available.  For
example, if a patch file includes a modified macro definition and two
calls to it, the calls will not refer to the new version of the macro
unless they are placed after the definition in the patch file by the
user.

System management tools should make use of system models in order to
support patching.  Patching mechanisms should also supply information
about the effect that a patch may have on the rest of the system.  In
@c<build>, the analysis could be done by propagating the effects of a
change through a task graph and then identifying those modules that
were affected by the change but ignored by the patch.

@subsection(More Precise Change Analysis)

All of the tools mentioned in this paper (including @c<build>) are
sensitive to the fact that some change has occurred to a module in a
system.  However, no attention is paid to the nature of the change.
By exploring the nature of a change it is possible to limit the amount
of processing done when updating systems.

If source code is changed in a way that cannot alter its compilation,
there is no reason for the source module to be recompiled.  For
example, compilation should not be done when source code has only been
reformatted or had commentary added to it.  If a function is added to
a module, but no existing modules are updated to contain calls to the
new function, nothing should be done to the existing modules.  Lint
libraries are dependent upon the first pass of Lint, however, most
changes to the first pass of Lint will not affect the libraries.

Change analysis can also provide important debugging information.  For
example, if a module interface is changed, but not all of the modules
that contain references to that module are changed, there is a
possibility that an error of omission has been made. 

Unlike @c<make> and @c<build>, @c<defsystem> can be extended to
include more complicated predicates for deciding when changes are
significant.  There is nothing preventing a @c<defsystem> system
definition from using parsers and source code comparison programs in
order to decide when transformations should take place. However, no
enhanced predicates are supplied with @c<defsystem> and none of the
@c<defsystem> descriptions encountered while preparing this paper
included definitions of such specialized predicates.

Specialized predicates can only be useful if they require less
processing to determine that a transformation can be avoided than
applying the transformation in the first place.  For instance, there
is no point in using a predicate to determine that compilation of a
module can be avoided if that predicate requires more processing than
the compiler.  @c<build> can step around this issue by assuming that
it is a single tool embedded in an integrated environment in which the
tools that are used to modify modules can supply information to
@c<build> about the nature of changes.

@c<build> could be extended to provide an interface for communicating
information about changes to modules.  The information passed to
@c<build> would include the name of the grain modified, the kind of
modification made, and the name of the new (i.e., updated) grain.  A new
class of handlers called @i<change handlers> would be introduced to
aid in the determination of @i<significant> changes by the
construction algorithm.

For example, the change assertion:
@begin(example, above 0, below 0)
(:ADDED-STRUCT DEFS)
@end(example)
would inform @c<build> that @F0<DEFS> has been changed by adding a new
structure and therefore modules that rely on @f0(DEFS) do not have to
be re-compiled.  The compilation of unaltered modules can be avoided
since there is no way for them to refer to the new structure.  The
assertions:
@begin(example, above 0, below 0)
(:ADDED-COMMENT DEFS)
(:RE-FORMATTED DEFS)
@end(example)
imply that no changes that can alter the compilation of @f0<DEFS> have
been made and therefore no re-compilation needs to be done.

The change handlers would contain listings of how types of changes
alter the way in which grains play their roles.  For instance, one
handler would note that re-formatting a piece of source code does not
change the way that it plays the role @f0<SOURCE> in instances of
@f0<:LISP-COMPILE>.
    
