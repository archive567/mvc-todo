<!doctype html>
<html lang="en">
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Haskell • GHCJS • Testing</title>
<link rel="stylesheet" href="https://tonyday567.github.io/other/lhs.css">

[ghcjs-testing](https://github.com/tonyday567/ghcjs-testing) [![Build Status](https://travis-ci.org/tonyday567/ghcjs-testing.png)](https://travis-ci.org/tonyday567/ghcjs-testing)
===

Minimal ghcjs install and test.

This is old-school web page development: grab some html page, and create some javascript that tweaks said page. This page starts off as a literate haskell file, gets built as a ghcjs piece of javascript, and then gets rendered as the said html page being tweaked.

todomvc
---

```include
other/todomvc.md
```


testing area
---

<h2>test button</h2>
<button class="click-me">Click me!</button>

<!-- GHCJS scripts. -->
<script language="javascript" src="other/ghcjs-testing.js"></script>

code
===

> import qualified Data.Map as Map
> import           GHCJS.Extended (onload)
> import           MVC hiding ((<>))
> import           MVC.Prelude as MVC
> import           Todo.Controllers (controllers)
> import           Todo.Model
> import           Todo.Views (render)
> import Control.Monad.Trans.State.Strict (State, StateT)
> import Protolude hiding (State, StateT, loop)
> 
> main :: IO ()
> main = onload (void run)
> 
> initialState :: Todos
> initialState =
>   Todos "" Nothing (ItemId 3) Nothing
>     (Map.fromList $ 
>      zip (ItemId <$> [0 ..])
>      [ Item Active "write view"
>      , Item Active "write controllers"
>      , Item Completed "render a todo list"
>      ])
> 
> run :: IO Todos
> run = do
>   (o, i) <- spawn unbounded
>   controllers o
>   runMVC initialState (asPipe $ MVC.loop model)
>     ((,) <$> 
>      pure (asSink render_) <*> 
>      ( pure (asInput i) `mappend`
>        producer unbounded (yield Refresh)))
> 
> render_ :: Out -> IO ()
> render_ (ActionOut action) = print action
> render_ (StateOut tds) = render tds
> 

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



notes
===

initial setup
---

The repo was constructed using the following steps:

- `stack new ghcjs-testing readme-lhs`
- edited stack.yaml to grab the `ghc-8.0.1` ghcjs documented [here](https://docs.haskellstack.org/en/stable/ghcjs/).
- `stack build`

found ghcjs examples
---

https://github.com/luite/hs15-talk/blob/master/src/Common.hs

[auto](https://github.com/mstksg/auto-examples/blob/master/src/TodoJS.hs)

from [ghcjs-base](https://github.com/ghcjs/ghcjs-base):

~~~
import GHCJS.Foreign.Callback
import Data.JSString -- This includes an IsString instance for JSString
import GHCJS.Types (JSVal)

foreign import javascript unsafe
  "require('console').log($1)" js_consoleLog :: JSVal -> IO ()

foreign import javascript unsafe
  "require('fs').stat($1, $2)"
  js_fsStat :: JSString -> Callback (JSVal -> JSVal -> IO ()) -> IO ()

main :: IO ()
main = do
  cb <- asyncCallback2 $ \err stat -> js_consoleLog stat
  js_fsStat "/home" cb
  releaseCallback cb
~~~

from [ghcjs-dom-hello](https://github.com/ghcjs/ghcjs-dom-hello)

~~~
main = do
  putStrLn "<a href=\"http://localhost:3708/\">http://localhost:3708/</a>"
  run 3708 $ do
    Just doc <- currentDocument
    body <- getBodyUnsafe doc
    setInnerHTML body (Just "<h1>Kia ora (Hi)</h1>")
    on doc D.click $ do
        (x, y) <- mouseClientXY
        newParagraph <- createElementUnsafe doc (Just "p") >>= unsafeCastTo HTMLParagraphElement
        text <- createTextNode doc $ "Click " ++ show (x, y)
        appendChild newParagraph text
        appendChild body (Just newParagraph)
        return ()

    -- Make an exit button
    exitMVar <- liftIO newEmptyMVar
    exit <- createElementUnsafe doc (Just "span") >>= unsafeCastTo HTMLSpanElement
    text <- createTextNode doc "Click here to exit"
    appendChild exit text
    appendChild body (Just exit)
    on exit E.click $ liftIO $ putMVar exitMVar ()

    -- Force all all the lazy evaluation to be executed
    syncPoint

    -- In GHC compiled version the WebSocket connection will end when this
    -- thread ends.  So we will wait until the user clicks exit.
    liftIO $ takeMVar exitMVar
    setInnerHTML body (Just "<h1>Ka kite ano (See you later)</h1>")
    return ()
~~~

component testing (todo)
===

react-flux
---

http://blog.wuzzeb.org/full-stack-web-haskell/client.html

https://facebook.github.io/flux/docs/in-depth-overview.html#content

http://hackage.haskell.org/package/react-flux

https://bitbucket.org/wuzzeb/react-flux/src/tip/example/

Material UI
---

http://www.material-ui.com/#/

footnotes
---

~~~
<script language="javascript" src="other/rts.js"></script>
<script language="javascript" src="other/lib.js"></script>
<script language="javascript" src="other/out.js"></script>
<script language="javascript" src="other/runmain.js"></script>
<script language="javascript" src="other/all.js"></script>
~~~

This doesn't work once you assume a browser window.

<pre>
  <code style="white-space: pre-wrap;">
stack build --exec "node $(stack path --local-install-root)/bin/readme.jsexe/all.js"
  </code>
</pre>

[^closuredownload]: from http://dl.google.com/closure-compiler/compiler-latest.zip
