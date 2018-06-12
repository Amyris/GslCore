# Tagging

Tags are terms within namespaces that can be associated with assemblies using the `tag` pragma.

```GSL
// Example tag pragma, term red in the color namespace
#tag color:red
pTDH3>gERG10
```

Tags are transient pragmas that associate with the next emitted assembly.  Each tag consists of a namespace and term.  More than one namespace pair can be specified with each `#tag`.

```GSL
// Multiple tags for one assembly
#tag color:red flavor:vanilla
pTDH3>gERG10
```

Multiple tags may be specified on different lines leading up to an assembly.  Tags accumulate till an assembly is emitted.

```GSL
#tag color:red 
#tag flavor:vanilla
pTDH3>gERG10
```

Tags may be emitted in various output formats.  For example, the flatfile output emits tags using the `TA` line.

```
// GSL compiler version 0.4.32
##### Assembly 0 #######
A# 0
NA basic_delete
TA color:yellow id:123
NP 7
```