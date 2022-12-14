@Part[Preface, Root "tr.mss"]
@Begin[TitlePage]
@Begin[Heading, FaceCode R]
@Begin[Center]
MASSACHUSETTS INSTITUTE OF TECHNOLOGY
ARTIFICIAL INTELLIGENCE LABORATORY
@End[Center]
@End[Heading]

@Begin[Display]
A.I. T.R. No. 874@>November 1985
@End[Display]

@Begin[Center]
@Begin[MajorHeading]
BUILD:

A Tool For Maintaining Consistency In Modular Systems
@End[MajorHeading]

by

Richard Elliot Robbins

@End[Center]

@begin(Abstract)

@c<build> is a tool for keeping modular systems in a consistent state
by managing the construction tasks (e.g. compilation, linking etc.)
associated with such systems. It employs a user supplied system model
and a procedural description of a task to be performed in order to
perform the task.  This differs from existing tools which do not
explicitly separate knowledge about systems from knowledge about how
systems are manipulated.
@blankspace(1 line)
@c<build> provides a static framework for modeling systems and
handling construction requests that makes use of programming
environment specific definitions.  By altering the set of definitions,
@c<build> can be extended to work with new programming environments
and to perform new tasks.

@end(Abstract)
@blankspace(3 lines)
@begin(center)
Copyright (c) Massachusetts Institute of Technology, 1985
@end(center)

@begin(text, indent 0)
This report describes research done at the Artificial Intelligence
Laboratory of the Massachusetts Institute of Technology.  Support for
the laboratory's artificial intelligence research has been provided in
part by the Advanced Research Projects Agency of the Department of
Defense under Office of Naval Research contract N00014-80-C-0505, in
part by National Science Foundation grant MCS-8117633, in part by the
International Business Machines Corporation, and in part by Honeywell
Information Systems, Incorporated.
@blankspace(1 line)
The views and conclusions contained in this document are those of the
author, and should not be interpreted as representing the policies,
either expressed or implied, of the Department of Defense, of the
National Science Foundation, of the International Business Machines
Corporation, or of Honeywell Information Systems, Incorporated.
@blankspace(1 line)
This report is a revised version of a thesis submitted to the
Department of Electrical Engineering and Computer Science on June 3,
1985 in partial fulfillment of the requirements for the degree of
Master of Science.
@end(text)
@end(titlepage)
@newpage
@PrefaceSection(Acknowledgments)
@begin(text)
I would like to acknowledge the role that my thesis advisor, Dick
Waters, played in the work that this report is based on; the
importance of his guidance and encouragement cannot be over-estimated.
I would also like to thank Bob Zieve, Jacques Bouvard and Honeywell
Information Systems for supporting this research.  Finally, I would
like to thank Donna Gorshel, Dave Wheeler, Pete Sterpe, Suzanne Witty,
Marc Zissman, Dave Kravitz, Sam Levitin, and all of the people who
were even remotely connected with the Honeywell Day Care Center for
providing the friendship and moral support that allowed me to see this
project to its completion.
@end(text)
@newpage
@PrefaceSection(Dedicated To)
@center(The memory of my grandmothers, Ruth and Esther.)
@newpage
 
