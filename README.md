Overview
===================
This is the code that accompanies a talk I gave at my
local functional programming users group about [exploring
typed functional web applications](http://hanstolpo.github.io/TalksAndStuff/slides/2015-11-23-Exploring_Typed_Functional_Web_Applications_Part_1.html#/exploring-typed-functional-web-applications-part-1).

There is a little Haskell server written in servant for the server side and the client side
is PureScript using purescript-halogen and building using WebPack.

Directory Structure
-----------------------
* `server` - The server side code and build infrastructure.
* `client` - The client side code and build infrastructure.
* `bin` - The output target where the server executable is installed to.
* `data` - All the resources served by the server and also the output destination for the client build.


Server
====================

Setup
--------------------
* Install Haskell Stack tool.
* In `server` directory run `stack install`

Running it
-------------------
Easiest is to use GHCi through stack, so run `stack repl`. This
will load all the modules into GHCi and then you can run the
different servers by calling `runApp` from the different modules,
first enable overloaded strings though (`:set -XOverloadedStrings`)

* `Echo.runApp 8086` - Run the echo server on port 8086
* `Client.runApp 8086` - Run the client server on port 8086 serving up the client code for 'CounterExample'
* `EchoClient.runApp 8086 "EchoOnly"` - Run the composed echo and client server on port 8086 serving up the client code for the 'EchoOnly'

Client
===================

Setup
-------------------
* Install new enough PureScript using stack e.g. `cabal fetch purescript-0.7.6.1 && cd purescript-0.7.6.1 && stack install`
* Install NodeJS and NPM
* Install Bower
* In the `client` directory
    * Install NPM dependencies `npm install`
    * Install bower dependencies `bower install`


Running / Building it
---------------------
In the `client` directory do one of the following:

* `npm run build` - Build the web application
* `npm run start-css` - Run a local development web server that continuously serves the empty example, only compiles in CSS.
* `npm run start-echo` - Run a local development web server that continuously serves the `EchoOnly.purs` example.
* `npm run start-routes` - Run a local development web server that continuously serves the `RoutesExample.purs` example.
* `npm start` - Run a local development web server that continuously servers an example depending on text in `settings.js`
* `npm run clean` - clean build output
* `npm run clean-all` - clean build output and all installed dependencies.

References
===================
Content mostly derived from reading the following:

* SurviveJs is a good guide for front end WebDev (good section on WebPack) https://survivejs.com/
* Documentation and example for PureScript loader https://github.com/ethul/purs-loader
* Documentation and examples for Halogen https://github.com/slamdata/purescript-halogen
* A guide to use purescript-routing with purescript-halogen http://www.parsonsmatt.org/programming/2015/10/22/purescript_router.html
* The PureScript book https://leanpub.com/purescript/read
* The Pursuit search engine http://pursuit.purescript.org/
* The Servant tutorial https://haskell-servant.github.io/tutorial/

