# nimquery
A library for querying HTML using CSS selectors, like JavaScripts `document.querySelector`.

Supports all combinators, the comma operator and all [CSS3 selectors](https://www.w3.org/TR/css3-selectors) except the following (most of which have no meaning in a headless context):
- :root
- :link
- :visited
- :active
- :hover
- :focus
- :target
- :lang(fr)
- :enabled
- :disabled
- :checked
- ::first-line 
- ::first-letter
- ::before
- ::after 

## Usage
```nim
import xmltree
import htmlparser
import streams

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

## API

```nim
proc querySelectorAll*(queryString: string): seq[XmlNode]
```
Get all elements matching `queryString`.

- - -

```nim
proc querySelector*(queryString: string): XmlNode
```
Get the first element matching `queryString`, or `nil` if no such element exists.

- - -

```nim
proc parseHtmlQuery*(queryString: string): Query
```
Parses a query for later use.

- - -

```nim
proc exec*(query: Query, root: XmlNode, single: static[bool]): seq[XmlNode]
```
Execute an already parsed query. If `single = true`, it will never return more than one element.

