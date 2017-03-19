<meta charset="utf-8">
<link rel="stylesheet" href="https://tonyday567.github.io/other/lhs.css">
<script type="text/javascript" async src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML"></script>

Haskell TodoMVC Example
=======================

> Haskell is a strongly-typed, lazily-evaluated, functional programming
> language.

[live demo](other/mvc-todo.html) with
[automation](other/mvc-todo-auto.html)

This example demonstrates an idiomatic haskell approach to the TodoMVC
problem domain involving:

-   compilation of haskell to javascript using
    [ghcjs](https://github.com/ghcjs/ghcjs).
-   The specification of a `Model` representing the problem domain,
    consisting of
-   specification of Abstract Data Types (ADTs) for inputs, state
    and outputs.
-   an algebra between state and actions.
-   Use of the [mvc](https://hackage.haskell.org/package/mvc) library
    for specification, asynchronicity and separation of model, view
    and controllers.
-   The creation of `View`s that consume model outputs, by using vanilla
    javascript effects.
-   The creation of `Controller`s that produce model inputs, by
    listening for Dom events using vanilla javascript.

recipe
======

The recipe below handles the bits and bobs you need to do every
re-compile. This includes a compression step via
[closure](http://dl.google.com/closure-compiler).

<pre>
  <code style="white-space: pre-wrap;">
stack build --exec "pandoc -f markdown -i other/index.md -t html -o index.html --filter pandoc-include" --exec "pandoc -f markdown -i other/index.md -t markdown -o readme.md --filter pandoc-include" --exec "java -jar $(stack path --local-bin)/closure-compiler-v20170124.jar --js_output_file=other/mvc-todo-auto.js $(stack path --local-install-root)/bin/mvc-todo-auto.jsexe/all.js" --exec "java -jar $(stack path --local-bin)/closure-compiler-v20170124.jar --js_output_file=other/mvc-todo.js $(stack path --local-install-root)/bin/mvc-todo.jsexe/all.js"
  </code>
</pre>

