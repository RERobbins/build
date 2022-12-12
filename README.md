# build
BUILD -- A System Construction Tool

BUILD is a proposed tool for constructing systems from existing modules. BUILD system descriptions are composed of module declarations and assertions of how modules refer to each other. An extensible library of information about module types and module interaction types is maintained. The library contains information that allows BUILD to derive construction dependencies from the module declarations and referencing patterns enumerated in system descriptions. BUILD will support facilities not adequately provided by existing tools; including automatic derivation of system descriptions, patching of systems, and incorporation of information about how modules change (e.g. the ability to differentiate between the effect of adding a function definition and the effect of adding a comment).
