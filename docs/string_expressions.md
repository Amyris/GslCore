## Supported operations

### Concatenation

It is possible to concatenate two strings in GSL in a variable binding using the `+` operator:
```
let foo = "bar" + "baz"
```
Which is equivalent of saying:
```
let foo = "barbaz"
```

#### Remarks

String concatenation works **only in variable bindings**, it doesn't work "inside" an assembly.

This doesn't work for instance:
```
#name "foo" + "bar"
```

You should write:
```
let baz = "foo" + "bar"

#name &baz
```
