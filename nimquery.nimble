# Package

version       = "1.1.1"
author        = "Oscar Nihlgård"
description   = "Library for querying HTML using CSS-selectors (like JavaScripts document.querySelector)"
license       = "MIT"

skipFiles = @["tests.nim"]

requires "nim >= 0.18.0"

task test, "Run the tests":
    exec "nim c -r tests.nim"
    rmFile "tests"