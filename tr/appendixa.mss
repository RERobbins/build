@part(AppendixA, root = "tr.mss")
@Appendix (BUILD Definitions For C)
@Label (AppendixA)
@define <verbatim2=verbatim,font SmallBodyFont, FaceCode F, LeftMargin +5>

The definitions used by @c<build> to model a Lisp environment have
been given in the body of this report as examples.  This appendix
contains the definitions used by @c<build> to model a C programming
environment.  There are more kinds of commonly used grain types in
UNIX environments than in Lisp environments, hence there are more
definitions needed to model all of the ways that UNIX grains can refer
to each other.  Commentary has been added to highlight the
definitions.

@subsection(Grain Type Definitions)

@begin(verbatim2)
(DEFINE-GRAIN-TYPE :YACC-GRAMMAR :Y)
(DEFINE-GRAIN-TYPE :C-SOURCE :C)
(DEFINE-GRAIN-TYPE :C-OBJECT :O)
(DEFINE-GRAIN-TYPE :C-EXECUTE :EXE)
(DEFINE-GRAIN-TYPE :SHELL-SCRIPT :SCRIPT)
@end(verbatim2)

@subsection(Process Type Definitions)

It is assumed that the functions @f0<C-COMPILE, C-LOAD>, and @f0<YACC>
are available.

@begin(verbatim2)
(DEFINE-PROCESS-TYPE C-COMPILE
    ((SOURCE :C-SOURCE :SINGLE) (INCLUDES :C-SOURCE :MULTIPLE))
    ((OBJECT :C-OBJECT :SINGLE SOURCE))
    STREAM
    (FORMAT STREAM "~%COMPILE ~A" (PATHNAME-MINUS-VERSION SOURCE))
  (FORMAT STREAM "~%COMPILING ~A" SOURCE)
  (C-COMPILE SOURCE OBJECT))
@hinge
(DEFINE-PROCESS-TYPE C-LOAD
    ((PRIMARY :C-OBJECT :SINGLE) (SECONDARY :C-OBJECT :MULTIPLE))
    ((IMAGE :C-EXECUTE :SINGLE PRIMARY))
    STREAM
    (FORMAT STREAM "~%LINK: ~A ~{~%  ~A~}"
	    (PATHNAME-MINUS-VERSION PRIMARY)
	    (MAPCAR #'PATHNAME-MINUS-VERSION SECONDARY))
  (FORMAT STREAM "~%LINKING: ~A ~{~%  ~A~}" PRIMARY SECONDARY)
  (C-LOAD PRIMARY SECONDARY IMAGE))
@hinge
(DEFINE-PROCESS-TYPE YACC
    ((GRAMMAR :YACC-GRAMMAR :SINGLE))
    ((PARSER :C-SOURCE :SINGLE GRAMMAR))
    STREAM
    (FORMAT STREAM "~%YACC ~A" (PATHNAME-MINUS-VERSION GRAMMAR))
  (FORMAT STREAM "~%YACCING ~A" GRAMMAR)
  (YACC GRAMMAR PARSER))
@end(verbatim2)
@newpage
@subsection(Request and Reference Handlers)

The request handler for C compilation models the fact that the source
grain needs to be compiled.  The only reference that can have an
effect on C compilation is @f0<:INCLUDES>.  If @f0<GRAIN-1> includes
@f0<GRAIN-2>, then @f0<GRAIN-1> indirectly includes any grains that
@f0<GRAIN-2> includes.  The task @f0<:INCLUDE+> (described later) is
responsible for gathering all of the grains included indirectly by a
grain and attaching the corresponding g-nodes to the @f0<INCLUDES>
port of the @f0<:C-COMPILE> p-node for the grain being compiled.

@begin(verbatim2)

;;;
;;;        :COMPILE :C-SOURCE
;;;

(DEFINE-REQUEST-HANDLER (:COMPILE :C-SOURCE :PRE) (SOURCE-NODE)
  (ACCESS* SOURCE-NODE ((SOURCE C-COMPILE) OBJECT)))

(DEFINE-REFERENCE-HANDLER ((:INCLUDES :C-SOURCE :C-SOURCE) (:COMPILE :LEFT))
                          (INCLUDING-NODE INCLUDED-NODE)
  (LET ((COMPILE-PROCESS (ACCESS INCLUDING-NODE ((SOURCE C-COMPILE)))))
    (PUSH INCLUDED-NODE (ACCESS+ COMPILE-PROCESS (INCLUDES)))
    (PROCESS-REQUEST :INCLUDE+ INCLUDED-NODE COMPILE-PROCESS)))

@end(verbatim2)

If a @f0<:C-SOURCE> grain calls another grain, then @c<build>
pessimistically assumes that it indirectly calls any grain called by
the second grain.  The task @f0<:LOAD+> gathers all of the grains
called indirectly by a grain in order to ensure that the proper set of
grains is linked together.  The lack of a task like @f0<:LOAD+> in
Lisp is due to the fact that in Lisp environments, grains are loaded
incrementally instead of being explicitly linked together.

@begin(verbatim2)
;;;
;;;        :LOAD :C-SOURCE
;;;

(DEFINE-REQUEST-HANDLER (:LOAD :C-SOURCE :PRE) (SOURCE-NODE)
  (PROCESS-REQUEST :COMPILE SOURCE-NODE)
  (ACCESS* SOURCE-NODE ((SOURCE C-COMPILE) OBJECT (PRIMARY C-LOAD) IMAGE)))

(DEFINE-REFERENCE-HANDLER ((:CALLS :C-SOURCE :C-SOURCE) (:LOAD :LEFT))
			  (CALLING-NODE CALLED-NODE)
  (LET ((LINKING-PROCESS
	  (ACCESS CALLING-NODE ((SOURCE C-COMPILE) OBJECT (PRIMARY C-LOAD)))))
    (PROCESS-REQUEST :COMPILE CALLED-NODE)
    (PUSH (ACCESS CALLED-NODE ((SOURCE C-COMPILE) OBJECT))
	  (ACCESS+ LINKING-PROCESS (SECONDARY)))
    (PROCESS-REQUEST :LOAD+ CALLED-NODE LINKING-PROCESS)))

(DEFINE-REFERENCE-HANDLER ((:CALLS :C-SOURCE :C-OBJECT) (:LOAD :LEFT))
			  (CALLING-NODE CALLED-NODE)
  (LET ((LINKING-PROCESS
	  (ACCESS CALLING-NODE ((SOURCE C-COMPILE) OBJECT (PRIMARY C-LOAD)))))
    (PUSH CALLED-NODE (ACCESS+ LINKING-PROCESS (SECONDARY)))
    (PROCESS-REQUEST :LOAD+ CALLED-NODE LINKING-PROCESS)))
@end(verbatim2)

Sometimes compiled objects are used as source grains (e.g. supplied
libraries).  These definitions encode the knowledge needed to handle
the loading of @f0<:C-OBJECT> grains.

@begin(verbatim2)

;;;
;;;        :LOAD :C-OBJECT
;;;

(DEFINE-REQUEST-HANDLER (:LOAD :C-OBJECT :PRE) (OBJECT-NODE)
  (ACCESS* OBJECT-NODE ((PRIMARY C-LOAD) IMAGE)))
@newpage
(DEFINE-REFERENCE-HANDLER ((:CALLS :C-OBJECT :C-SOURCE) (:LOAD :LEFT))
			  (CALLING-NODE CALLED-NODE)
  (LET ((LINKING-PROCESS (ACCESS CALLING-NODE ((PRIMARY C-LOAD)))))
    (PROCESS-REQUEST :COMPILE CALLED-NODE)
    (PUSH (ACCESS CALLED-NODE ((SOURCE C-COMPILE) OBJECT))
	  (ACCESS+ LINKING-PROCESS (SECONDARY)))
    (PROCESS-REQUEST :LOAD+ CALLED-NODE LINKING-PROCESS)))
@hinge
(DEFINE-REFERENCE-HANDLER ((:CALLS :C-OBJECT :C-OBJECT) (:LOAD :LEFT))
			  (CALLING-NODE CALLED-NODE)
  (LET ((LINKING-PROCESS (ACCESS CALLING-NODE ((PRIMARY C-LOAD)))))
    (PUSH CALLED-NODE (ACCESS+ LINKING-PROCESS (SECONDARY)))
    (PROCESS-REQUEST :LOAD+ CALLED-NODE LINKING-PROCESS)))

@END(VERBATIM2)

Here are the handlers for @f0<:INCLUDE+> and @f0<:LOAD+>.  There are
no request handlers associated with these requests as all of the
significant construction information that they imply arises from
references.  These handlers illustrate the use of more than two values
being passed to reference handlers.  The additional parameter for
@f0<:INCLUDE+> is the @f0<:C-COMPILE> p-node which models the
compilation to be done.  The additional parameter for @f0<:LOAD+> is
the p-node which models the linking to be done.

@begin(verbatim2)
;;;
;;;        :INCLUDE+ :C-SOURCE C-COMPILE
;;;

(DEFINE-REFERENCE-HANDLER ((:INCLUDES :C-SOURCE :C-SOURCE) (:INCLUDE+ :LEFT))
			  (IGNORE INCLUDED-NODE INCLUDING-PROCESS)
  (PUSH INCLUDED-NODE (ACCESS+ INCLUDING-PROCESS (INCLUDES)))
  (PROCESS-REQUEST :INCLUDE+ INCLUDED-NODE INCLUDING-PROCESS))
@HINGE
;;;
;;;        :LOAD+ :C-SOURCE C-LOAD
;;;

(DEFINE-REFERENCE-HANDLER ((:CALLS :C-SOURCE :C-SOURCE) (:LOAD+ :LEFT))
			  (IGNORE CALLED-NODE LINKING-PROCESS)
  (PROCESS-REQUEST :COMPILE CALLED-NODE)
  (PUSH (ACCESS CALLED-NODE ((SOURCE C-COMPILE) OBJECT))
	(ACCESS+ LINKING-PROCESS (SECONDARY)))
  (PROCESS-REQUEST :LOAD+ CALLED-NODE LINKING-PROCESS))

(DEFINE-REFERENCE-HANDLER ((:CALLS :C-SOURCE :C-OBJECT) (:LOAD+ :LEFT))
			  (IGNORE CALLED-NODE LINKING-PROCESS)
  (PUSH CALLED-NODE (ACCESS+ LINKING-PROCESS (SECONDARY)))
  (PROCESS-REQUEST :LOAD+ CALLED-NODE LINKING-PROCESS))
@HINGE
;;;
;;;        :LOAD+ :C-OBJECT C-LOAD
;;;

(DEFINE-REFERENCE-HANDLER ((:CALLS :C-OBJECT :C-SOURCE) (:LOAD+ :LEFT))
			  (IGNORE CALLED-NODE LINKING-PROCESS)
  (PROCESS-REQUEST :COMPILE CALLED-NODE)
  (PUSH (ACCESS CALLED-NODE ((SOURCE C-COMPILE) OBJECT))
	(ACCESS+ LINKING-PROCESS (SECONDARY)))
  (PROCESS-REQUEST :LOAD+ CALLED-NODE LINKING-PROCESS))

(DEFINE-REFERENCE-HANDLER ((:CALLS :C-OBJECT :C-OBJECT) (:LOAD+ :LEFT))
			  (IGNORE CALLED-NODE LINKING-PROCESS)
  (PUSH CALLED-NODE (ACCESS+ LINKING-PROCESS (SECONDARY)))
  (PROCESS-REQUEST :LOAD+ CALLED-NODE LINKING-PROCESS))
@end(verbatim2)

@newpage
Here are the definitions used to model @c<YACC>'s interaction with C
systems.  The handlers capture the fact that @c<YACC> grammars may
include and call other grains.

@begin(verbatim2)
;;;
;;; :YACC :YACC-GRAMMAR
;;;

(DEFINE-REQUEST-HANDLER (:YACC :YACC-GRAMMAR :PRE) (GRAMMAR-NODE)
  (ACCESS* GRAMMAR-NODE ((GRAMMAR YACC) PARSER)))
@HINGE
;;;
;;;        :COMPILE :YACC-GRAMMAR
;;;

(DEFINE-REQUEST-HANDLER (:COMPILE :YACC-GRAMMAR :PRE) (GRAMMAR-NODE)
  (PROCESS-REQUEST :YACC GRAMMAR-NODE)
  (ACCESS* GRAMMAR-NODE ((GRAMMAR YACC) PARSER (SOURCE C-COMPILE) OBJECT)))

(DEFINE-REFERENCE-HANDLER ((:INCLUDES :YACC-GRAMMAR :C-SOURCE) (:COMPILE :LEFT))
			  (INCLUDING-NODE INCLUDED-NODE)
  (LET ((COMPILE-PROCESS
	  (ACCESS INCLUDING-NODE ((GRAMMAR YACC) PARSER (SOURCE C-COMPILE)))))
    (PUSH INCLUDED-NODE (ACCESS+ COMPILE-PROCESS (INCLUDES)))
    (PROCESS-REQUEST :INCLUDE+ INCLUDED-NODE COMPILE-PROCESS)))
@HINGE
;;;
;;;        :LOAD :YACC-GRAMMAR
;;;

(DEFINE-REQUEST-HANDLER (:LOAD :YACC-GRAMMAR :PRE) (GRAMMAR-NODE)
  (PROCESS-REQUEST :COMPILE GRAMMAR-NODE)
  (ACCESS* GRAMMAR-NODE ((GRAMMAR YACC) PARSER
			 (SOURCE C-COMPILE) OBJECT
			 (PRIMARY C-LOAD) IMAGE)))

(DEFINE-REFERENCE-HANDLER ((:CALLS :YACC-GRAMMAR :C-SOURCE) (:LOAD :LEFT))
			  (CALLING-NODE CALLED-NODE)
  (LET ((LINKING-PROCESS (ACCESS CALLING-NODE ((GRAMMAR YACC) PARSER
					       (SOURCE C-COMPILE) OBJECT
					       (PRIMARY C-LOAD)))))
    (PROCESS-REQUEST :COMPILE CALLED-NODE)
    (PUSH (ACCESS CALLED-NODE ((SOURCE C-COMPILE) OBJECT))
	  (ACCESS+ LINKING-PROCESS (SECONDARY)))
    (PROCESS-REQUEST :LOAD+ CALLED-NODE LINKING-PROCESS)))

(DEFINE-REFERENCE-HANDLER ((:CALLS :YACC-GRAMMAR :C-OBJECT) (:LOAD :LEFT))
			  (CALLING-NODE CALLED-NODE)
  (LET ((LINKING-PROCESS (ACCESS CALLING-NODE ((GRAMMAR YACC) PARSER
					       (SOURCE C-COMPILE) OBJECT
					       (PRIMARY C-LOAD)))))
    (PUSH CALLED-NODE (ACCESS+ LINKING-PROCESS (SECONDARY)))
    (PROCESS-REQUEST :LOAD+ CALLED-NODE LINKING-PROCESS)))
@end(verbatim2)

@newpage
Here are the definitions used to handle @f0<:SHELL-SCRIPT> grains.
A request to compile or load a shell script is interpreted to mean
that all of the modules called by the script should be compiled or loaded.

@begin(verbatim2)
;;;
;;;        :COMPILE :SHELL-SCRIPT
;;;

(DEFINE-REFERENCE-HANDLER ((:CALLS :SHELL-SCRIPT :C-SOURCE) (:COMPILE :LEFT))
			  (IGNORE CALLED-NODE)
  (PROCESS-REQUEST :COMPILE CALLED-NODE))

(DEFINE-REFERENCE-HANDLER ((:CALLS :SHELL-SCRIPT :C-OBJECT) (:COMPILE :LEFT))
			  (IGNORE CALLED-NODE)
  (PROCESS-REQUEST :COMPILE CALLED-NODE))

(DEFINE-REFERENCE-HANDLER ((:CALLS :SHELL-SCRIPT :YACC-GRAMMAR) (:COMPILE :LEFT))
			  (IGNORE CALLED-NODE)
  (PROCESS-REQUEST :COMPILE CALLED-NODE))

;;;
;;;        :LOAD :SHELL-SCRIPT
;;;

(DEFINE-REFERENCE-HANDLER ((:CALLS :SHELL-SCRIPT :C-SOURCE) (:LOAD :LEFT))
			  (IGNORE CALLED-NODE)
  (PROCESS-REQUEST :LOAD CALLED-NODE))

(DEFINE-REFERENCE-HANDLER ((:CALLS :SHELL-SCRIPT :C-OBJECT) (:LOAD :LEFT))
			  (IGNORE CALLED-NODE)
  (PROCESS-REQUEST :LOAD CALLED-NODE))

(DEFINE-REFERENCE-HANDLER ((:CALLS :SHELL-SCRIPT :YACC-GRAMMAR) (:LOAD :LEFT))
			  (IGNORE CALLED-NODE)
  (PROCESS-REQUEST :LOAD CALLED-NODE))

@end(verbatim2)
  
