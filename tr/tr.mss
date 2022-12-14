@Use(DataBase="<robbins.tr>")
@Use(bibliography="<robbins.tr>robbins.bib")
@make(TR)
@Style<WidowAction Force, References STDAlphabetic, DoubleSided Yes>
@SpecialFont(F0="Gacha10")
@SpecialFont(F1="Gacha10I")
@Modify [Example, Above 1, Below 1, FaceCode F]
@Define [SmallExample=Example, Font SmallBodyFont]
@Define [Definition=Example, LeftMargin +0]
@modify (Description, Spread 1 line)
@define (Description2=Description, LeftMargin +2, Indent -2)
@pageheading()
@pagefooting()
@include (preface.mss)
@set(page 0)
@pagefooting(center <@value(page)>)
@include (intro.mss)
@include (tr1.mss)
@include (tr2.mss)
@include (tr3.mss)
@include (tr4.mss)
@include (tr5.mss)
@unnumbered[References]
@bibliography
@include (appendixa.mss)
    
