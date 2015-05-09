# Haskell TodoMVC Example

> Haskell is a lazy evaluation, functional programming language.

This example demonstrates an idiomatic haskell approach to the TodoMVC problem domain involving:

- compilation of haskell to javascript using `ghcjs`.
- The specification of a `Model` representing the problem domain, consisting of
  - specification of Abstract Data Types (ADTs) for input actions, state and output actions.
  - an algebra between state and actions, partly utilising the `lens` library.
- Use of the `mvc` library for specification, asynchronicity and separation of model, view and controllers.
- The creation of `View`s that consume model outputs, by creating Dom effects.
- The creation of `Controller`s that produce model inputs, by listening for Dom events.



notes
===

events
---
http://davidwalsh.name/event-delegate



mvc
---

from http://www.artima.com/articles/dci_vision.html


Object oriented programming grew out of Doug Englebart's vision of the computer as an extension of the human mind.

the (mvc) framework exists to separate the representation of information from user interaction

instead

... separate pure computation from effects

So that we can:
  - equationally reason
  - refactor aggressively
  - cut down on bugz

Data: representing the user's mental model of things in their world

A particularly simplistic rule of thumb in early object-oriented design was: nouns (e.g. in the requirements document) are objects, and verbs are methods. This dichotomy naturally fit the two concepts that programming languages could express.

So the first new concept we introduce is roles. Whereas objects capture what objects are, roles capture collections of behaviors that are about what objects do.


http://en.wikipedia.org/wiki/Data,_context_and_interaction

Data, context and interaction
