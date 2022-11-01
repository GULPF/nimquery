# Package

version       = "2.0.1"
author        = "Oscar Nihlgård"
description   = "Library for querying HTML using CSS-selectors (like JavaScripts document.querySelector)"
license       = "MIT"

skipDirs = @["tests"]

requires "nim >= 0.20.0"

task test, "Run the tests":
    exec "nim c -r tests/incltests"
    rmFile "tests/incltests"
    exec "nim c -r tests/tests"
    rmFile "tests/tests"
