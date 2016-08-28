# language-haskell-ast

[`haskell-src-exts`](https://hackage.haskell.org/package/haskell-src-exts)’s parser produces AST modelled in a number of concrete, specific datatypes. This is a good thing for most purposes, but it also means that some simple tasks like walking the tree to e.g. count the nodes requires writing code for each specific type.

`language-haskell-ast` adds generic interfaces for this sort of walk. It also knows how to pretty-print the AST as s-expressions, if you like that sort of thing.

For example, here’s the AST for the first couple of lines of the program which produces the AST:

```
$ language-haskell-ast app/Main.hs
(Module (ModuleHead (ModuleName "Main"))
        (LanguagePragma (Ident "DefaultSignatures")
                        (Ident "FlexibleContexts")
                        (Ident "FlexibleInstances")
                        (Ident "RecordWildCards")
                        (Ident "TypeOperators"))
…
```

…and so on.
