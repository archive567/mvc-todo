<meta charset="utf-8">
<link rel="stylesheet" href="http://tonyday567.github.io/other/lhs.css">

[test-ghcjs](https://github.com/tonyday567/test-ghcjs) [![Build Status](https://travis-ci.org/tonyday567/test-ghcjs.png)](https://travis-ci.org/tonyday567/test-ghcjs)
======================================================================================================================================================================

A bare-minimum ghcjs installation constructed with the following steps:

-   `stack new test-ghcjs readme-lhs`
-   edited stack.yaml to grab the `ghc-8.0.1` ghcjs documented
    [here](https://docs.haskellstack.org/en/stable/ghcjs/).
-   `stack build` compiles readme.lhs aka the code below.

and that's it!

`node $(stack path --local-install-root)/bin/readme.jsexe/all.js` to
check if it's working.

Incredibly

<pre>
  <code style="white-space: pre-wrap;">
stack build --exec "node $(stack path --local-install-root)/bin/readme.jsexe/all.js" --exec "pandoc -f markdown+lhs -i readme.lhs -t html -o index.html" --exec "pandoc -f markdown+lhs -i readme.lhs -t markdown -o readme.md" --file-watch
  </code>
</pre>
gives you a compile loop!

``` {.sourceCode .literate .haskell}
import Protolude

main :: IO ()
main = putStrLn ("ghcjs rocks!" :: Text)
```

ToDo
====

Hook ghcjs index.html up so that it gets insta-blogged on push.
