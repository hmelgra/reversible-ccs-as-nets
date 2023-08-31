# reversible-ccs-as-nets
Haskell implementation of the encoding of reversible CCS as reversible Petri nets

This is a stack project. After installing the project, the execution of a
reversible CCS process can be simulated by running `revccs-exe`c <<CCS-process>>`.
For instance,
```
revccs-exe "Rec X ((Out 2 :. Var X) :+ (Out 1 :. Nil))"
```
