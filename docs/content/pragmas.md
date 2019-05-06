# Pragmas 

## Introduction
Pragmas are used as instructions to the compiler to alter its behavior.  

This pragma sets the name of the following assembly design.

```GSL
// Example pragma
#name myconstruct
pTDH3>gERG10
```

Pragmas can be global as in the above example, but can also be attached to a specific part using the {} syntax.

```
pTDH3 ; gERG10[1...10e] {#name mySpecialErg10}
```

Multiple part attached pragmas can be attached to one part separated by semi colons

```
pTDH3 ; gERG10[1..10e] {#name mySpecialErg10 ; #uri http://dev.null/part/1234 }
```


## Pragmas

A complete list of pragmas with basic documentation can be obtained from the compiler with the command line flag `--helpPragmas`

NB: documentation is incomplete, but got to start somewhere.

### Part length modifying pragmas

The `#termlen` pragma alters the default length for a terminator part.  E.g. `tADH1` would default to a length of approximately 500bp, with some variation due to primer optimization.  `#termlen 250` would set it to a shorter length.  The pragma will take affect till the value is changed again with another #termlen instruction or the scope ends. i.e. end of a function.


The `#promlen` pragma provides similar functionality but affects the promoter length which defaults to 500bp.

The `#termlenmrna` pragma affects the length of a terminator when it is part of an mRNA part.  This is controlled independently from the stand-alone terminator part.  It defaults to 200bp.

### Structure modifying pragmas

The `#topology` pragma controls the structure of the output of a particular design. It can be either `circular` or `linear`. Linear is the default and to be used in most of the cases. However, if you want to design something that has circular structure, like a plasmid, use `circular` topology.
