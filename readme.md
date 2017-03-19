compiling
===

The recipe below handles the bits and bobs you need to do every re-compile:

- stack build
- rendering of the page
- optimize (and copy) the js via [closure](http://dl.google.com/closure-compiler/compiler-latest.zip)[^closuredownload]

<pre>
  <code style="white-space: pre-wrap;">
stack build --exec "pandoc -f markdown+lhs -i readme.lhs -t html -o index.html" --exec "pandoc -f markdown+lhs -i readme.lhs -t markdown -o readme.md" --exec "java -jar $(stack path --local-bin)/closure-compiler-v20170124.jar --js_output_file=other/ghcjs-testing.js $(stack path --local-install-root)/bin/readme.jsexe/all.js"
  </code>
</pre>

<pre>
  <code style="white-space: pre-wrap;">
stack build --exec "java -jar $(stack path --local-bin)/closure-compiler-v20170124.jar --js_output_file=other/mvc-todo.js $(stack path --local-install-root)/bin/mvc-todo.jsexe/all.js"
  </code>
</pre>

# Haskell TodoMVC Example

> Haskell is a strongly-typed, lazily-evaluated, functional programming language.

This example demonstrates an idiomatic haskell approach to the TodoMVC problem domain involving:

- compilation of haskell to javascript using [ghcjs][ghcjs].
- The specification of a `Model` representing the problem domain, consisting of
  - specification of Abstract Data Types (ADTs) for inputs, state and outputs.
  - an algebra between state and actions, utilising the [lens][lens] library.
- Use of the [mvc][mvc] library for specification, asynchronicity and separation of model, view and controllers.
- The creation of `View`s that consume model outputs, by creating Dom effects.
- The creation of `Controller`s that produce model inputs, by listening for Dom events.

build/develop
===

- install [ghcjs][ghcjs]
- `cabal configure --enable-tests && cabal build && cabal test`
- point browser at index.html (or serve file)

ghci
---

ghcjs doesn't currently support interactive usage such as ghci.  However, GHCJS.Extended includes dummy definitions to enable normal compilation by ghc and thus ability to use ghci for type checking and such.  Just:

 - edit cabal.configure to remove `compiler: ghcjs`
 - run `cabal configure --flags=no-ghcjs && cabal build`

(Idea ripped from [ghcjs-react](https://github.com/fpco/ghcjs-react))

[mvc]: https://hackage.haskell.org/package/mvc
[lens]: https://hackage.haskell.org/package/lens
[ghcjs]: https://github.com/ghcjs/ghcjs

