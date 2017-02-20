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

testing area
---

<h2>test button</h2>
<button class="click-me">Click me!</button>

<!-- GHCJS scripts. -->
<script language="javascript" src="other/ghcjs-testing.js"></script>

code
===

> {-# LANGUAGE OverloadedStrings #-}
> import Protolude
> import GHCJS.Foreign.Callback
> import Data.JSString -- This includes an IsString instance for JSString
> import GHCJS.Types (JSVal)
> import GHCJS.DOM (currentWindow)
> 
> foreign import javascript unsafe
>   "console.log($1)" consoleLog :: JSString -> IO ()
> foreign import javascript unsafe
>   "alert($1)" alert :: JSString -> IO ()
> 
> foreign import javascript unsafe "window.onload = $1"
>   onload :: Callback (IO ()) -> IO ()
>
> main :: IO ()
> main = do
>   putStrLn ("a putStrLn" :: Text)
>   -- consoleLog $ ("a consoleLog")
> 
>   w <- currentWindow
>   case w of
>      Nothing -> putStrLn ("no window in currentWindow" :: Text)
>      Just w' -> alert "an alert"
> 
>   -- onload =<< asyncCallback (alert "post window.onload alert!!")

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
