@make(report)
@Style(Font Helvetica10, Indent 0, DoubleSided No, HyphenBreak Yes,
	 Spacing 1, Spread 1)
@modify(enumerate, font titlefont2)
@pageheading(left "An Introduction To BUILD", center <@value(date)>,
right <@value(page)>)

@begin(titlepage)

@majorheading[An Introduction To BUILD]

@begin(enumerate)
MAINTAINING LARGE SYSTEMS

EXISTING TOOLS - MAKE & DEFSYSTEM

SOME PROBLEMS WITH MAKE & DEFSYSTEM

BUILD

EXTENSIONS TO BUILD
@end(enumerate)
@end(titlepage)

@newpage
@majorheading(MAINTAINING LARGE SYSTEMS)
@blankspace(.25 in)
@begin(enumerate)
Maintainers Need To Understand High Level Structure

Maintainers Need To Reconfigure Systems When Modules Change
@end(enumerate)

@blankspace(1 in)
@begin[figure]
@begin(verbatim, font SmallBodyFont)
@tabclear
@tabdivide(7)
@blankspace(.50 in)
parser.grammar    YACC@\parser.c@\Compiler@\parser.o
@blankspace(1 in)
@\@\definitions.c
@blankspace(1 in)
@\@\codegen.c@\Compiler@\codegen.o@\  Linker@\TinyComp
@blankspace(1 in)
@\@\@\@\library.o
@blankspace(1 in)
@tabclear
@end(verbatim)
@caption(A Small Example - TinyComp)
@end[figure]

@newpage
@majorheading(EXISTING TOOLS - MAKE)
@blankspace(.25 in)
@begin(enumerate)
UNIX & C

MakeFiles

Targets & MakeFile Entries
@end(enumerate)
@blankspace(1 in)
@begin(figure)
@begin(verbatim)
TARGET-MODULE: MODULE@-(1) MODULE@-(2) MODULE@-(3) ... MODULE@-(M)
	COMMAND@-(1)
	COMMAND@-(2)
	COMMAND@-(3)
	 ...
	COMMAND@-(N)

@end(verbatim)
@caption(MakeFile Entry Template)
@end(figure)
@blankspace(1 in)
@begin[figure]
@begin(verbatim)
PARSER.C: PARSER.GRAMMAR
	YACC PARSER.GRAMMAR	#GENERATE PARSER INTO FILE Y.TAB.C
	MV Y.TAB.C PARSER.C	#CHANGE NAME OF OUTPUT

PARSER.O: PARSER.C DEFINITIONS.C
	CC -C PARSER.C		# -C FOR COMPILATION

CODEGEN.O: CODEGEN.C DEFINITIONS.C
	CC -C CODEGEN.C		# -C FOR COMPILATION

TINYCOMP: CODEGEN.O PARSER.O LIBRARY.O
	CC CODEGEN.O PARSER.O LIBRARY.O -O TINYCOMP # -O FOR LINKING

@end(verbatim)
@caption(MakeFile For TinyComp)
@end[figure]

@newpage
@majorheading(EXISTING TOOLS - DEFSYSTEM)
@blankspace(.25 in)
@begin(enumerate)
Lisp Machine - Lisp

Defsystem Descriptions

Module Definitions, Transformations & Pre-conditions
@end(enumerate)
@blankspace(1 in)
@begin(figure)
@begin(verbatim)
(@b(:MODULE) MODULE-NAME FILE-LIST)

(@b(:FASLOAD) INPUT-MODULE PRE-CONDITIONS)

(@b(:COMPILE) INPUT-MODULE PRE-CONDITIONS)

(@b(:COMPILE-LOAD-INIT) INPUT
	 DEPENDENCIES COMPILE-PRE-CONDITIONS LOAD-PRE-CONDITIONS)

@end(verbatim)
@caption(DEFSYSTEM Module & Transformation Templates)
@end(figure)
@blankspace(1 in)
@begin[figure]
@begin(verbatim)

(DEFSYSTEM TINY-COMP
  (:MODULE DEFS "DEFINITIONS")
  (:MODULE GRAMMAR "PARSER.Y")
  (:MODULE CODE-GENERATOR "CODEGEN.LISP")
  (:MODULE LIBRARY "LIBRARY")

  (:FASLOAD DEFS)
  (:FASLOAD LIBRARY)
  (:COMPILE-LOAD-INIT CODE-GENERATOR  (DEFS) (:FASLOAD DEFS))
  (:COMPILE-LOAD-INIT (:YACC GRAMMAR) (DEFS) (:FASLOAD DEFS)))

@end(verbatim)
@caption(DEFSYSTEM Description For TinyComp)
@end[figure]

@newpage
@majorheading(SOME PROBLEMS WITH MAKE & DEFSYSTEM)
@blankspace(.25 in)
@begin(enumerate)
Construction Dependency Based

No Type Constraints

Explicit References To Intermediate Modules
@end(enumerate)
@blankspace(1 in)
@center[@p(WE SHOULD DESCRIBE SYSTEMS LIKE WE THINK ABOUT THEM!)]
@newpage
@figure[
@begin(verbatim, font SmallBodyFont)
M=/usr/src/lib/mip
CFLAGS=-O -DFLEXNAMES
LINTLIBS=llib-port.ln llib-lc.ln llib-lm.ln llib-lmp.ln llib-lcurses.ln

all:	lpass1 lpass2 $(LINTLIBS)

lpass1: cgram.o xdefs.o scan.o comm1.o pftn.o trees.o optim.o lint.o hash.o
	CC cgram.o xdefs.o scan.o comm1.o pftn.o trees.o optim.o lint.o hash.o -o lpass1

trees.o: $(M)/manifest macdefs $(M)/mfile1 $(M)/trees.c
	CC -c $(CFLAGS) -I$(M) -I. $(M)/trees.c
optim.o: $(M)/manifest macdefs $(M)/mfile1 $(M)/optim.c
	CC -c $(CFLAGS) -I$(M) -I. $(M)/optim.c
pftn.o: $(M)/manifest macdefs $(M)/mfile1 $(M)/pftn.c
	CC -c $(CFLAGS) -I$(M) -I. $(M)/pftn.c
lint.o: $(M)/manifest macdefs $(M)/mfile1 lmanifest
	CC -c $(CFLAGS) -I$(M) -I. lint.c
scan.o: $(M)/manifest macdefs $(M)/mfile1 $(M)/scan.c
	CC -c $(CFLAGS) -I$(M) -I. $(M)/scan.c
xdefs.o: $(M)/manifest $(M)/mfile1 macdefs $(M)/xdefs.c
	CC -c $(CFLAGS) -I$(M) -I. $(M)/xdefs.c
comm1.o: $(M)/manifest $(M)/mfile1 $(M)/common macdefs $(M)/comm1.c
	CC -c $(CFLAGS) -I. -I$(M) $(M)/comm1.c
cgram.o: $(M)/manifest $(M)/mfile1 macdefs cgram.c
	CC -c $(CFLAGS) -I$(M) -I. cgram.c

cgram.c: $(M)/cgram.y
	yacc $(M)/cgram.y
	mv y.tab.c cgram.c

llib-port.ln: llib-port lpass1
	-(/lib/cpp -C -Dlint llib-port | ./lpass1 -puv > llib-port.ln )
llib-lm.ln: llib-lm lpass1
	-(/lib/cpp -C -Dlint llib-lm | ./lpass1 -puv > llib-lm.ln )
llib-lmp.ln: llib-lmp lpass1
	-(/lib/cpp -C -Dlint llib-lmp | ./lpass1 -puv > llib-lmp.ln )
llib-lc.ln: llib-lc lpass1
	-(/lib/cpp -C -Dlint llib-lc | ./lpass1 -v > llib-lc.ln )
llib-lcurses.ln: llib-lcurses lpass1
	-(/lib/cpp -C -Dlint llib-lcurses | ./lpass1 -v > llib-lcurses.ln )

lpass2: lpass2.o hash.o
	CC lpass2.o hash.o -o lpass2

lpass2.o: $(M)/manifest lmanifest
	CC $(CFLAGS) -c -I$(M) -I. lpass2.c
	
lintall:
	lint -hpv -I. -I$(M)  $(M)/cgram.c $(M)/xdefs.c $(M)/scan.c \
		$(M)/pftn.c $(M)/trees.c $(M)/optim.c lint.c

install: all SHELL
	install -s lpass1 /usr/lib/lint/lint1
	install -s lpass2 /usr/lib/lint/lint2
	for i in llib-*; do install -c -m 644 $$i /usr/lib/lint; done
	install -c SHELL /usr/bin/lint

shrink:
	rm -f *.o

clean: shrink
	rm -f lpass1 lpass2 cgram.c $(LINTLIBS)

@end(verbatim)
@caption<MakeFile For Lint>
]
@newpage
@blankspace(1 in)
@figure[
@begin(verbatim)
(DEFSYSTEM LINT
  (:NAME "Lint")
  (:MODULE DEFINITIONS-1 ("MACDEFS" "MANIFEST" "MFILE1"  "LMANIFEST"))
  (:MODULE DEFINITIONS-2 ("MANIFEST" "LMANIFEST"))
  (:MODULE PASS1 ("XDEFS.LISP" "SCAN.LISP" "COMM1.LISP" "PFTN.LISP"
		  "TREES.LISP" "OPTIM.LISP" "LINT.LISP" "HASH.LISP"))
  (:MODULE PASS2 ("LPASS2.LISP"	"HASH.LISP"))
  (:MODULE DRIVER "SHELL.LISP")
  (:MODULE GRAMMAR "LISPGRAM.LISP")
  (:MODULE LIBRARIES ("LLIB-PORT.LN" "LLIB-LC.LN" "LLIB-LM.LN"
		      "LLIB-LMP.LN" "LLIB-LCURSES.LN"))

  (:FASLOAD DEFINITIONS-1)
  (:FASLOAD DEFINITIONS-2)
  (:COMPILE-LOAD DRIVER)
  (:COMPILE-LOAD-INIT PASS1 (DEFINITIONS-1) (:FASLOAD DEFINITIONS-1))
  (:COMPILE-LOAD-INIT PASS2 (DEFINITIONS-2) (:FASLOAD DEFINITIONS-2))
  (:COMPILE-LOAD-INIT (:YACC GRAMMAR) (DEFINITIONS-1)
		      (:FASLOAD DEFINITIONS-1))
  (:BUILD-LINT-LIBRARY LIBRARIES (:FASLOAD DRIVER GRAMMAR PASS1)))

@end<verbatim>
@caption<DEFSYSTEM Description For Lint>
]

@newpage
@majorheading(BUILD)
@blankspace (.25 in)
@begin(enumerate)
No particular machine or language

Module Declaration Assertions

Module Reference Assertions

Defining New Module and Reference Types
@end(enumerate)
@blankspace(1 in)
@begin(figure)
@begin(verbatim)
(@b(MODULE) MODULE-NAME MODULE-TYPE FILE-LIST)

(REFERENCE-TYPE REFERENCING-MODULES REFERENCED-MODULES)

(@b(DEFINE-MODULE-TYPE) NAME INTERMEDIATE INSTANTIATION-PROCESSING)

(@b(DEFINE-REFERENCE-TYPE) NAME REFERENCING-TYPE REFERENCED-TYPE PROCESSING)

@end(verbatim)
@caption(BUILD Base Operation Templates)
@end(figure)
@blankspace(1 in)
@begin(figure)
@begin(verbatim)
(DEFBUILD-DESCRIPTION  TINYCOMP ()
 
 (MODULE DEFS LISP ("DEFINITIONS"))
 (MODULE PARSER GRAMMAR ("PARSER.GRAMMAR"))
 (MODULE GENERATOR LISP ("CODEGEN.SOURCE"))
 (MODULE LIBRARY  LISP ("LIBRARY" LISP))

 (USES-DEFINITIONS-FROM (PARSER GENERATOR) DEFS)
 (CALLS PARSER (LIBRARY GENERATOR))
 (CALLS GENERATOR LIBRARY))

@end(verbatim)
@caption<BUILD Description For TinyComp>
@end<figure>

@newpage
@blankspace(.75 in)
@begin(figure)
@begin[figure]
@begin(verbatim)
PARSER.C: PARSER.GRAMMAR
	YACC PARSER.GRAMMAR	#GENERATE PARSER INTO FILE Y.TAB.C
	MV Y.TAB.C PARSER.C	#CHANGE NAME OF OUTPUT

PARSER.O: PARSER.C DEFINITIONS.C
	CC -C PARSER.C		# -C FOR COMPILATION

CODEGEN.O: CODEGEN.C DEFINITIONS.C
	CC -C CODEGEN.C		# -C FOR COMPILATION

TINYCOMP: CODEGEN.O PARSER.O LIBRARY.O
	CC CODEGEN.O PARSER.O LIBRARY.O -O TINYCOMP # -O FOR LINKING

@end(verbatim)
@caption(MakeFile For TinyComp)
@end[figure]
@begin[figure]
@begin(verbatim)
(DEFSYSTEM TINY-COMP
  (:MODULE DEFS "DEFINITIONS")
  (:MODULE GRAMMAR "PARSER.Y")
  (:MODULE CODE-GENERATOR "CODEGEN.LISP")
  (:MODULE LIBRARY "LIBRARY")

  (:FASLOAD DEFS)
  (:FASLOAD LIBRARY)
  (:COMPILE-LOAD-INIT CODE-GENERATOR  (DEFS) (:FASLOAD DEFS))
  (:COMPILE-LOAD-INIT (:YACC GRAMMAR) (DEFS) (:FASLOAD DEFS)))

@end(verbatim)
@caption(DEFSYSTEM Description For TinyComp)
@end[figure]
@begin(figure)
@begin(verbatim)
(DEFBUILD-DESCRIPTION  TINYCOMP ()
 
 (MODULE DEFS LISP ("DEFINITIONS"))
 (MODULE PARSER GRAMMAR ("PARSER.GRAMMAR"))
 (MODULE GENERATOR LISP ("CODEGEN.SOURCE"))
 (MODULE LIBRARY  LISP ("LIBRARY" LISP))

 (USES-DEFINITIONS-FROM (PARSER GENERATOR) DEFS)
 (CALLS PARSER (LIBRARY GENERATOR))
 (CALLS GENERATOR LIBRARY))

@end(verbatim)
@caption<BUILD Description For TinyComp>
@end<figure>
@end[figure]
@newpage
@blankspace(1 in)
@figure[
@begin(verbatim)

(DEFBUILD-DESCRIPTION  LINT
  ((SUBSYSTEM LIBRARIES (LIBRARY-SOURCES))
   (SUBSYSTEM PASS1 (COMMON-DEFS DEFS-1 LINT1 SCANNER PARSER OPTIMIZER
		      TREES HASH LOW-LEVEL))
   (SUBSYSTEM PASS2 (COMMON-DEFS LINT2 HASH)))

  (MODULE DRIVER LISP ("SHELL"))
  (MODULE COMMON-DEFS LISP ("MANIFEST" "LMANIFEST"))
  (MODULE DEFS-1 LISP ("MACDEFS" "MFILE1"))
  (MODULE LINT1 LISP ("LINT.LISP"))
  (MODULE LINT2 LISP ("LPASS2.LISP"))
  (MODULE SCANNER LISP ("SCAN.LISP"))
  (MODULE PARSER GRAMMAR ("CGRAM.Y"))
  (MODULE OPTIMIZER LISP ("OPTIM.LISP"))
  (MODULE TREES LISP ("TREES.LISP"))
  (MODULE HASH LISP ("HASH.LISP"))
  (MODULE LOW-LEVEL LISP ("PFTN.LISP" "XDEFS.LISP" "COMM1.LISP"))
  (MODULE LIBRARY-SOURCES LINT-LIBRARY-SOURCE
	  ("LLIB-LC.LN" "LLIB-LM.LN" "LLIB-LCURSES.LN" "LLIB-PORT.LN"
			"LLIB-LMP.LN"))

  (USES-DEFINITIONS-FROM (LOW-LEVEL LINT2 HASH TREES) COMMON-DEFS)
  (USES-DEFINITIONS-FROM (LINT1 SCANNER PARSER OPTIMIZER) DEFS-1)
  (CALLS DRIVER (LINT1 LINT2 LIBRARIES))
  (CALLS LINT1 (SCANNER PARSER OPTIMIZER))
  (CALLS SCANNER LOW-LEVEL)
  (CALLS PARSER (TREES HASH LOW-LEVEL))
  (CALLS OPTIMIZER TREES)
  (CALLS (TREES HASH) LOW-LEVEL)
  (CALLS LINT2 HASH)

@end(verbatim)
@caption(BUILD Description For Lint)
]
@newpage
@majorheading(EXTENSIONS TO BUILD)
@blankspace(.25 in)
@begin(enumerate)

Automatic Derivation Of System Descriptions

Patching Support

More Precise Change Analysis
@end(enumerate)
