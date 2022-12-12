@make(article,form 1)
@style(indent = 0)
@begin(titlepage)
@begin(titlebox)
@majorheading(Software Engineering Symposium

on

Practical Software Development Environments)
@end(titlebox)
@blankspace(3.0 in)
@heading(A Summary)
@value(month) @value(year)

@b[Rich Robbins]
@end(Titlepage)
@section(Abstract)
Programming environments support the creation, modification,
execution and debugging of programs.  The goal of integrating
a programming environment is more than simply building tools
that share a common data base and provide a consistent user
interface.  Ideally, the programming environment appears to the
programmer as a single tool; there are no firewalls separating the
various functions provided by the environment.@foot<Delisle, Menicosy, and
Schwartz, "Viewing a Programming Environment as a Single Tool", Proceedings
of the ACM SIGSOFT/SIGPLAN Software Engineering Symposium on Practical
Software Development Environments, May 1984.>
@newpage
@section(Highlights)
@begin(enumerate)
Language Oriented Editors
@begin(enumerate)
How are LOE's different from text editors?

Are LOE's better for developing software?

What should a well designed LOE provide?

Semantics Directed Editors.  (text --> syntax --> semantics)
@end(enumerate)

Programming In The Large
@begin(enumerate)
What is Programming In The Small?

What is Programming In The Large?

New tools are required to support Programming In The Large.
@begin(enumerate)
Configuration Management

Version Control

Software Families
@end(enumerate)
@end(enumerate)

Environment Case Studies
@begin(enumerate)
SmallTalk-80, @i(Object Oriented, Portable)

Interlisp, @i(Power tools for the AI programmer)

MESA, @i(Building Bridges)

GNOME, @i(A System For Teaching Introductory Programming Courses)
@end(enumerate)

Environment Generating Tools
@begin(enumerate)
The Synthesizer Generator

GANDALF
@end(enumerate)
@end(enumerate)
