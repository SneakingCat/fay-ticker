Fay-Ticker
==========

Introduction
------------
Fay-Ticker is a demo of the Fay programming language (see
https://github.com/faylang/fay/wiki). The application is showing what
a "throughput ticker" could look like.

Fay-Ticker is using a very ugly cabal integration

Build:
cabal configure --enable-tests --user
cabal build
cabal test

TODO
----
* Factor out the library parts of the Fay-Ticker to their own repo.
* Connect a real server to make it a "real" ticker application.