# nimquery
A library for querying HTML using CSS selectors, like JavaScripts `document.querySelector`.

## Usage
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
      <p>2</p>
      <p>3</p>
      <p>4</p>
    </body>
  </html>
"""
let xml = parseHtml(newStringStream(html))
let elements = xml.querySelectorAll("p:nth-child(odd)")
echo elements
# => @[<p>1</p>, <p>3</p>]
```

## Supported selectors
Nimquery supports all combinators, the comma operator and all [CSS3 selectors](https://www.w3.org/TR/css3-selectors) except the following (most of which have no meaning in a headless context):
- :root
- :link
- :visited
- :active
- :hover
- :focus
- :target
- :lang(...)
- :enabled
- :disabled
- :checked
- ::first-line 
- ::first-letter
- ::before
- ::after 

## API

```nim
proc querySelectorAll*(XmlNode: root, queryString: string, options: set[NimqueryOption] = NimqueryDefaultOptions): seq[XmlNode]
```
Get all elements matching `queryString`.  
Raises `ParseError` if parsing of `queryString` fails.  
See [Options](#options) for information about the `options` parameter.

- - -

```nim
proc querySelector*(XmlNode: root, queryString: string, options: set[NimqueryOption] = NimqueryDefaultOptions): XmlNode
```
Get the first element matching `queryString`, or `nil` if no such element exists.  
Raises `ParseError` if parsing of `queryString` fails.  
See [Options](#options) for information about the `options` parameter.

- - -

```nim
proc parseHtmlQuery*(queryString: string, options: set[NimqueryOption] = NimqueryDefaultOptions): Query
```
Parses a query for later use.  
Raises `ParseError` if parsing of `queryString` fails.  
See [Options](#options) for information about the `options` parameter. 

- - -

```nim
proc exec*(query: Query, root: XmlNode, single: static[bool]): seq[XmlNode]
```
Execute an already parsed query. If `single = true`, it will never return more than one element.

### Options <a name="options"></a>
The `NimqueryOption` enum contains flags for configuring the behavior when parsing/searching:

- `optUniqueIds`: Indicates if id attributes should be assumed to be unique.
- `optSimpleNot`: Indicates if only simple selectors are allowed in the `:not(...)`. Note that combinators are never allowed.
- `optUnicodeIdentifiers`: Indicates if unicode characters are allowed inside identifiers. Doesn't affect strings where unicode is always allowed.

The default options are exported as `const nimqueryDefaultOptions* = { optUniqueIds, optUnicodeIdentifiers, optSimpleNot }`.

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
let options = nimqueryDefaultOptions - { optSimpleNot }
let elements = xml.querySelectorAll("p:not(.maybe-skip:nth-child(even))", options)
echo elements
# => @[<p>1</p>, <p class="maybe-skip">3</p>, <p>4</p>]
```
