test-ghcjs
===

[![Build Status](https://travis-ci.org/tonyday567/test-ghcjs.png)](https://travis-ci.org/tonyday567/test-ghcjs)

test-ghcjs
---

A bare-minimum ghcjs installation constructed with the following steps:

- `stack new test-ghcjs readme-lhs`
- edited stack.yaml to grab the `ghc-8.0.1` ghcjs documented [here](https://docs.haskellstack.org/en/stable/ghcjs/).
- `stack build` compiles readme.lhs aka the code below.

and that's it!

`node $(stack path --local-install-root)/bin/readme.jsexe/all.js` checks to see it's all working.

Incredibly

<pre>
  <code style="white-space: pre-wrap;">
stack build --exec "node $(stack path --local-install-root)/bin/readme.jsexe/all.js" --exec "pandoc -f markdown+lhs -i readme.lhs -t html -o index.html --filter pandoc-include --mathjax" --exec "pandoc -f markdown+lhs -i readme.lhs -t markdown -o readme.md --filter pandoc-include --mathjax" --file-watch
  </code>
</pre>

gives you a compile loop!

> import Protolude
>
> main :: IO ()
> main = putStrLn ("ghcjs rocks!" :: Text)
>
