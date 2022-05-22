# Nimquery ![CI](https://github.com/GULPF/nimquery/workflows/CI/badge.svg)
A library for querying HTML using CSS selectors, like JavaScripts `document.querySelector`/`document.querySelectorAll`.

## Installation

Nimquery is available on Nimble:
```
nimble install nimquery
```

## Usage
```nim
from xmltree import `$`
from htmlparser import parseHtml
import nimquery

let html = """
<!DOCTYPE html>
<html>
  <head><title>Example</title></head>
  <body>
    <p>1</p>
    <p>2</p>
    <p>3</p>
    <p>4</p>
  </body>
</html>
"""
let xml = parseHtml(html)
let elements = xml.querySelectorAll("p:nth-child(odd)")
echo elements
# => @[<p>1</p>, <p>3</p>]
```

## API

```nim
proc querySelectorAll*(root: XmlNode,
                       queryString: string,
                       options: set[QueryOption] = DefaultQueryOptions): seq[XmlNode]
```
Get all elements matching `queryString`.  
Raises `ParseError` if parsing of `queryString` fails.  
See [Options](#options) for information about the `options` parameter.

- - -

```nim
proc querySelector*(root: XmlNode,
                    queryString: string,
                    options: set[QueryOption] = DefaultQueryOptions): XmlNode
```
Get the first element matching `queryString`, or `nil` if no such element exists.  
Raises `ParseError` if parsing of `queryString` fails.  
See [Options](#options) for information about the `options` parameter.

- - -

```nim
proc parseHtmlQuery*(queryString: string,
                     options: set[QueryOption] = DefaultQueryOptions): Query
```
Parses a query for later use.  
Raises `ParseError` if parsing of `queryString` fails.  
See [Options](#options) for information about the `options` parameter. 

- - -

```nim
proc exec*(query: Query,
           root: XmlNode,
           single: bool): seq[XmlNode]
```
Execute an already parsed query. If `single = true`, it will never return more than one element.

### Options <a name="options"></a>
The `QueryOption` enum contains flags for configuring the behavior when parsing/searching:

- `optUniqueIds`: Indicates if id attributes should be assumed to be unique.
- `optSimpleNot`: Indicates if only simple selectors are allowed as an argument to the `:not(...)` psuedo-class. Note that combinators are not allowed in the argument even if this flag is excluded.
- `optUnicodeIdentifiers`: Indicates if unicode characters are allowed inside identifiers. Doesn't affect strings where unicode is always allowed.

The default options is defined as `const DefaultQueryOptions* = { optUniqueIds, optUnicodeIdentifiers, optSimpleNot }`.

Below is an example of using the options parameter to allow a complex `:not(...)` selector.

```nim
import xmltree
import htmlparser
import streams
import nimquery

let html = """
<!DOCTYPE html>
  <html>
    <head><title>Example</title></head>
    <body>
      <p>1</p>
      <p class="maybe-skip">2</p>
      <p class="maybe-skip">3</p>
      <p>4</p>
    </body>
  </html>
"""
let xml = parseHtml(newStringStream(html))
let options = DefaultQueryOptions - { optSimpleNot }
let elements = xml.querySelectorAll("p:not(.maybe-skip:nth-child(even))", options)
echo elements
# => @[<p>1</p>, <p class="maybe-skip">3</p>, <p>4</p>]
```

## Unsupported selectors
Nimquery supports all [CSS3 selectors](https://www.w3.org/TR/css3-selectors) except the following: `:root`, `:link`, `:visited`, `:active`, `:hover`, `:focus`, `:target`, `:lang(...)`, `:enabled`, `:disabled`, `:checked`, `::first-line`, `::first-letter`, `::before`, `::after`. These selectors will not be implemented because they don't make much sense in the situations where Nimquery is useful.
