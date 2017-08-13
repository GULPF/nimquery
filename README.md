# nimquery
Library for querying HTML using CSS-selectors

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

```nim
proc querySelector*(queryString: string): XmlNode
```
Get the first element matching `queryString`.

```nim
proc parseHtmlQuery*(queryString: string): Query
```
Parses a query for later use.

```nim
proc exec*(query: Query, root: XmlNode, single: static[bool]): seq[XmlNode]
```
Execute an already parsed query. If `single = true`, it will never return more than one element.
