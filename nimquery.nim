# Spec: https://www.w3.org/TR/css3-selectors/

import std / [xmltree, strutils, strtabs, unicode, math, deques, parseutils]

const DEBUG = false

type
    ParseError* = object of ValueError

    TokenKind = enum
        tkInvalid

        tkBracketStart, tkBracketEnd
        tkParam
        tkComma

        # NOTE: These are handled the same in some contexts, but they
        #       are different. `tkIdentifier` can only contain a very specific
        #       subset of characters, but tkString can contain anything.
        #       This means that both `#foo%` and `[id=foo%]` is invalid,
        #       but not `[id="foo%"]` or `#foo\%`.
        tkIdentifier, tkString

        tkClass, tkId, tkElement

        tkCombinatorDescendents, tkCombinatorChildren
        tkCombinatorNextSibling, tkCombinatorSiblings

        tkAttributeExact     # [attr=...]
        tkAttributeItem      # [attr~=...]
        tkAttributePipe      # [attr|=...]
        tkAttributeExists    # [attr]
        tkAttributeStart     # [attr^=...]
        tkAttributeEnd       # [attr$=...]
        tkAttributeSubstring # [attr*=...]

        tkPseudoNthChild, tkPseudoNthLastChild
        tkPseudoNthOfType, tkPseudoNthLastOfType

        tkPseudoFirstOfType, tkPseudoLastOfType
        tkPseudoOnlyChild, tkPseudoOnlyOfType, tkPseudoEmpty
        tkPseudoFirstChild, tkPseudoLastChild

        tkPseudoNot

        tkEoi # End of input

    Token = object
        kind: TokenKind
        value: string

const AttributeKinds = {
    tkAttributeExact, tkAttributeItem,
    tkAttributePipe, tkAttributeExists,
    tkAttributeStart, tkAttributeEnd,
    tkAttributeSubstring
}

const NthKinds = {
    tkPseudoNthChild, tkPseudoNthLastChild,
    tkPseudoNthOfType, tkPseudoNthLastOfType
}

type
    Demand = object
        case kind: Tokenkind
        of AttributeKinds:
            attrName, attrValue: string
        of NthKinds:
            a, b: int
        of tkPseudoNot:
            notQuery: QueryPart
        of tkElement:
            element: string
        else: discard

    NodeWithParent = tuple
        parent: XmlNode
        # Index is the index used by `xmltree`,
        # elementIndex is the index when only counting elements
        # (not text nodes etc).
        index, elementIndex: int

    Combinator = enum
        cmDescendants = tkCombinatorDescendents,
        cmChildren = tkCombinatorChildren,
        cmNextSibling = tkCombinatorNextSibling,
        cmSiblings = tkCombinatorSiblings,
        cmLeaf # Special case for the last query

    QueryOption* = enum
        optUniqueIds          ## Assume unique id's or not
        optUnicodeIdentifiers ## Allow non-ascii in identifiers (e.g `#exämple`)
        optSimpleNot          ## Only allow simple selectors as the argument
                              ## for ":not". Combinators and/or commas are not
                              ## allowed even if this option is excluded.

    Lexer = object
        input: string
        pos: int
        options: set[QueryOption]
        current, next: Token

    Query* = object ## Represents a parsed query.
        queries: seq[seq[QueryPart]]
        options: set[QueryOption]
        queryStr: string ## The original input string

    QueryPart = object
        demands: seq[Demand]
        combinator: Combinator

{.deprecated: [NimqueryOption: QueryOption].}

const DefaultQueryOptions* = {optUniqueIds, optUnicodeIdentifiers,
    optSimpleNot}
const NimqueryDefaultOptions* {.deprecated.} = DefaultQueryOptions

const Identifiers = Letters + Digits + {'-', '_', '\\'}
# NOTE: This is not the same as `strutils.Whitespace`.
#       These values are defined by spec.
const CssWhitespace = {'\x20', '\x09', '\x0A', '\x0D', '\x0C'}
const Combinators = CssWhitespace + {'+', '~', '>'}

const PseudoNoParamsKinds = {
    tkPseudoFirstOfType, tkPseudoLastOfType,
    tkPseudoOnlyChild, tkPseudoOnlyOfType,
    tkPseudoEmpty, tkPseudoFirstChild,
    tkPseudoLastChild
}

const PseudoParamsKinds = NthKinds + {tkPseudoNot}

const CombinatorKinds = {
    tkCombinatorChildren, tkCombinatorDescendents,
    tkCombinatorNextSibling, tkCombinatorSiblings
}

template log(msg: string) =
    when DEBUG:
        echo msg

func safeCharCompare(str: string, idx: int, cs: set[char]): bool {.inline.} =
    if idx > high(str): return false
    if idx < low(str): return false
    return str[idx] in cs

func safeCharCompare(str: string, idx: int, c: char): bool {.inline.} =
    return str.safeCharCompare(idx, {c})

func node(pair: NodeWithParent): XmlNode =
    return pair.parent[pair.index]

func attrComparerString(kind: TokenKind): string =
    case kind
    of tkAttributeExact: return "="
    of tkAttributeItem: return "~="
    of tkAttributePipe: return "|="
    of tkAttributeExists: return ""
    of tkAttributeStart: return "^="
    of tkAttributeEnd: return "$="
    of tkAttributeSubstring: return "*="
    else: raiseAssert "Invalid attr kind: " & $kind

func newUnexpectedCharacterException(s: string): ref ParseError =
    return newException(ParseError, "Unexpected character: '" & s & "'")

func newUnexpectedCharacterException(c: char): ref ParseError =
    newUnexpectedCharacterException($c)

func initNotDemand(notQuery: QueryPart): Demand =
    result = Demand(kind: tkPseudoNot, notQuery: notQuery)

func initElementDemand(element: string): Demand =
    result = Demand(kind: tkElement, element: element)

func initPseudoDemand(kind: TokenKind): Demand =
    result = Demand(kind: kind)

func initAttributeDemand(kind: TokenKind, name, value: string): Demand =
    case kind
    of AttributeKinds:
        result = Demand(kind: kind, attrName: name, attrValue: value)
    else:
        raiseAssert "invalid kind: " & $kind

func initNthChildDemand(kind: TokenKind, a, b: int): Demand =
    case kind
    of NthKinds:
        result = Demand(kind: kind, a: a, b: b)
    else:
        raiseAssert "invalid kind: " & $kind

func `$`(demand: Demand): string =
    case demand.kind:
    of AttributeKinds:
        if demand.kind == tkAttributeExists:
            result = "[" & demand.attrName & "]"
        else:
            result = "[" & demand.attrName & demand.kind.attrComparerString &
                "'" & demand.attrValue & "']"
    of tkPseudoNot:
        result = ":" & $demand.kind & "(" & $demand.notQuery & ")"
    of NthKinds:
        result = ":" & $demand.kind & "(" & $demand.a & "n, " & $demand.b & ")"
    of PseudoNoParamsKinds:
        result = ":" & $demand.kind
    of tkElement:
        result = demand.element
    else:
        result = $demand.kind

func `==`(d1, d2: Demand): bool =
    if d1.kind != d2.kind: return false
    case d1.kind
    of AttributeKinds:
        return d1.attrName == d2.attrName and d1.attrValue == d2.attrValue
    of NthKinds:
        return d1.a == d2.b
    of tkPseudoNot:
        return d1.notQuery == d2.notQuery
    of tkElement:
        return d1.element == d2.element
    else:
        raise newException(Exception, "Invalid demand kind: " & $d1.kind)

iterator children(node: XmlNode,
                  offset: NodeWithParent = (nil, -1, -1)): NodeWithParent =
    var idx = offset.index + 1
    var elIdx = offset.elementIndex + 1
    while idx < node.len:
        let el = node[idx]
        if el.kind == xnElement:
            yield (parent: node, index: idx, elementIndex: elIdx).NodeWithParent
            elIdx.inc
        idx.inc

func initToken(kind: TokenKind, value: string = ""): Token =
    return Token(kind: kind, value: value)

func initQueryPart(demands: seq[Demand], combinator: Combinator): QueryPart =
    return QueryPart(demands: demands, combinator: combinator)

func canFindMultiple(q: Querypart, comb: Combinator,
                     options: set[QueryOption]): bool =
    # Returns true if the current queries demands can be satisfied by
    # multiple elements. This is used to check if the search should stop
    # after the first element has been found.
    for demand in q.demands:
        if optUniqueIds in options and demand.kind in AttributeKinds and
                demand.attrName == "id":
            return false
        if comb in {cmChildren, cmSiblings} and demand.kind in
                {tkPseudoFirstOfType, tkPseudoLastOfType,
                    tkPseudoFirstChild, tkPseudoLastChild, tkPseudoOnlyOfType}:
            return false

    return true

func `$`*(q: Query): string =
    ## Returns the original input string used to construct the query
    result = q.queryStr

func isValidNotQuery(q: Query, options: set[QueryOption]): bool =
    return
        q.queries.len == 1 and
        q.queries[0].len == 1 and
        (q.queries[0][0].demands.len == 1 or not (optSimpleNot in options))

func readEscape(input: string, idx: var int, buffer: var string) =
    assert input[idx] == '\\'
    idx.inc

    # Linefeed, carriage return and form feed can't be escaped.
    if input[idx] in {'\x0C', '\x0D', '\x0A'}:
        raise newUnexpectedCharacterException(input[idx])

    # No special handling is required for these.
    # E.g '\n' means 'n', not 'newline'.
    if input[idx] notin HexDigits:
        # FIXME: Should this read a grapheme instead of a rune? I don't know
        let runeStr = input.runeAt(idx).toUTF8
        buffer.add runeStr
        idx.inc runeStr.len

    else:
        var hexStr = ""

        while input[idx] in HexDigits and hexStr.len < 6:
            hexStr.add input[idx]
            idx.inc

        # Skip whitespace after hex input
        if input[idx] in CssWhitespace:
            idx.inc

        try:
            let runeStr = hexStr.parseHexInt.Rune.toUTF8
            buffer.add runeStr
        except ValueError:
            raiseAssert "Can't happen"

func readStringLiteral(input: string, idx: var int, buffer: var string) =
    assert input[idx] in {'\'', '"'}

    let ch = input[idx]
    idx.inc

    while input[idx] != ch:
        if input[idx] == '\\':
            readEscape(input, idx, buffer)
        else:
            buffer.add input[idx]
            idx.inc

        if idx > high(input):
            raise newException(ParseError, "Non-terminated string")

    idx.inc

func readIdentifier(input: string, idx: var int, buffer: var string) =
    const intIdentifiers = {
        'a'.int .. 'z'.int, 'A'.int .. 'Z'.int,
        '0'.int .. '9'.int,
        '-'.int, '_'.int, '\\'.int
    }

    if input[idx] == '_' or
            input[idx] in Digits or
            (input[idx] == '-' and
                input.safeCharCompare(idx + 1, {'-'} + Digits)):
        raise newUnexpectedCharacterException(input[idx + 1])

    func isValidIdentifier(rune: Rune): bool =
        if rune.int32 in intIdentifiers:
            return true
        # Spec: https://www.w3.org/TR/CSS21/syndata.html#value-def-identifier
        return rune >=% 0x00A0.Rune

    while idx < input.len:
        # NOTE: `idx` is the byte offset of input, so `runeAt(idx)` is correct.
        let rune = input.runeAt(idx)

        if not isValidIdentifier(rune):
            break

        if rune == '\\'.Rune:
            readEscape(input, idx, buffer)
        else:
            let unicodeCh = $rune
            idx.inc unicodeCh.len
            buffer.add unicodeCh

func readIdentifierAscii(input: string, idx: var int, buffer: var string) =
    if input[idx] == '-' and input.safeCharCompare(idx + 1, {'-'} + Digits):
        raise newUnexpectedCharacterException(input[idx + 1])

    while input[idx] in Identifiers and idx < input.len:
        if input[idx] == '\\':
            readEscape(input, idx, buffer)
        else:
            buffer.add input[idx]
            idx.inc

func readParams(input: string, idx: var int, buffer: var string) =
    # Fragile, ugly, ok
    var paramContextCount = 0
    var dblQuoteStringContext = false
    var sglQuoteStringContext = false
    idx.inc

    while input[idx] != ')' or paramContextCount > 0 or
            dblQuoteStringContext or sglQuoteStringContext:
        if input[idx] == '"' and not sglQuoteStringContext:
            dblQuoteStringContext = not dblQuoteStringContext

        if input[idx] == '\'' and not dblQuoteStringContext:
            sglQuoteStringContext = not sglQuoteStringContext

        if input[idx] == '(' and not dblQuoteStringContext and
                not sglQuoteStringContext:
            paramContextCount.inc

        if input[idx] == ')' and not dblQuoteStringContext and
                not sglQuoteStringContext:
            paramContextCount.dec

        if input[idx] == '\\':
            buffer.add input[idx]
            idx.inc

        buffer.add input[idx]
        idx.inc

        if idx > high(input):
            raise newException(ParseError,
                "Non-terminated pseudo argument list")

    idx.inc

func parsePseudoNthArguments(input: string): tuple[a: int, b: int] =
    var buffer = ""
    var idx = 0
    idx.inc skipWhile(input, CssWhitespace, idx)

    template takeInt: int =
        var v: int
        try:
            v = buffer.parseInt
            buffer = ""
        # NOTE: This branch can only be taken in case of overflow
        except ValueError as err:
            raise newException(ParseError, err.msg)
        v

    if idx + 2 < input.len and input[idx..idx+2].cmpIgnoreCase("odd") == 0:
        result = (2, 1)
        idx.inc 3
    elif idx + 3 < input.len and input[idx..idx+3].cmpIgnoreCase("even") == 0:
        result = (2, 0)
        idx.inc 4
    else:
        if idx < input.len and input[idx] in {'+', '-'}:
            buffer.add input[idx]
            idx.inc
        if idx >= input.len:
            raise newException(ParseError, "Invalid parameter for ':nth-*'")
        if input[idx] notin Digits:
            buffer.add "1"
        while idx < input.len and input[idx] in Digits:
            buffer.add input[idx]
            idx.inc
        if idx < input.len and input[idx] in {'n', 'N'}:
            idx.inc
            result.a = takeInt()
            idx.inc skipWhile(input, CssWhitespace, idx)
            if idx < input.len and input[idx] in {'+', '-'}:
                buffer.add input[idx]
                idx.inc
                idx.inc skipWhile(input, CssWhitespace, idx)
                if idx >= input.len or input[idx] notin Digits:
                    raise newUnexpectedCharacterException(input[idx])
                while idx < input.len and input[idx] in Digits:
                    buffer.add input[idx]
                    idx.inc
                result.b = takeInt()
            else:
                discard # done, only "a" was specified
        else:
            result.b = takeInt()

    idx.inc skipWhile(input, CssWhitespace, idx)
    if idx <= input.high:
        raise newUnexpectedCharacterException(input[idx])

func initPseudoToken(str: string): Token =
    let kind = case str
    of ":empty":            tkPseudoEmpty
    of ":only-child":       tkPseudoOnlyChild
    of ":only-of-type":     tkPseudoOnlyOfType
    of ":first-child":      tkPseudoFirstChild
    of ":last-child":       tkPseudoLastChild
    of ":last-of-type":     tkPseudoLastOfType
    of ":first-of-type":    tkPseudoFirstOfType
    of ":not":              tkPseudoNot
    of ":nth-child":        tkPseudoNthChild
    of ":nth-last-child":   tkPseudoNthLastChild
    of ":nth-of-type":      tkPseudoNthOfType
    of ":nth-last-of-type": tkPseudoNthLastOfType
    else:
        raise newException(ParseError, "Unknown pseudo selector: " & str)
    result = initToken(kind)

func isFinishedSimpleSelector(prev: Token, prevPrev: Token): bool =
    # Checks if the last two tokens represents the end of a simple selector.
    # This is needed to determine if a space is significant or not.
    if prev.kind in {tkBracketEnd, tkParam, tkElement} + PseudoNoParamsKinds:
        return true
    if prev.kind == tkIdentifier and prevPrev.kind in {tkClass, tkId}:
        return true

proc forward(lexer: var Lexer) =
    if lexer.pos > lexer.input.high:
        lexer.current = lexer.next
        lexer.next = initToken(tkEoi)
        return

    let ch = lexer.input[lexer.pos]
    var skip = false
    var token: Token
    log "char: '" & ch & "'"

    case ch:

    of {'"', '\''}:
        var buffer = ""
        readStringLiteral(lexer.input, lexer.pos, buffer)
        token = initToken(tkString, buffer)

    of CssWhitespace:
        if lexer.pos + 1 < lexer.input.len and
                lexer.input[lexer.pos + 1] notin Combinators and
                isFinishedSimpleSelector(lexer.next, lexer.current):
            token = initToken(tkCombinatorDescendents)
        else:
            skip = true

        lexer.pos.inc

    of '~':
        if lexer.input.safeCharCompare(lexer.pos + 1, '='):
            token = initToken(tkAttributeItem)
            lexer.pos.inc 2
        else:
            token = initToken(tkCombinatorSiblings)
            lexer.pos.inc

    of '+':
        token = initToken(tkCombinatorNextSibling)
        lexer.pos.inc

    of '>':
        token = initToken(tkCombinatorChildren)
        lexer.pos.inc

    of '[':
        token = initToken(tkBracketStart)
        lexer.pos.inc

    of ']':
        token = initToken(tkBracketEnd)
        lexer.pos.inc

    of ':':
        var buffer = ""
        buffer.add ch
        lexer.pos.inc
        while lexer.pos <= lexer.input.high and
                lexer.input[lexer.pos] in Identifiers:
            buffer.add lexer.input[lexer.pos]
            lexer.pos.inc

        token = initPseudoToken(buffer.toLowerAscii)

    of '#':
        lexer.pos.inc
        token = initToken(tkId)

    of '.':
        lexer.pos.inc
        token = initToken(tkClass)

    of '*':
        if lexer.input.safeCharCompare(lexer.pos + 1, '='):
            token = initToken(tkAttributeSubstring)
            lexer.pos.inc 2
        else:
            lexer.pos.inc
            # No need to emit since tkUniversal matches everything?
            # token = initToken(tkUniversal)
            skip = true

    of '(':
        var buffer = ""
        readParams(lexer.input, lexer.pos, buffer)
        token = initToken(tkParam, buffer)

    of '=':
        token = initToken(tkAttributeExact)
        lexer.pos.inc

    of '|':
        if lexer.input.safeCharCompare(lexer.pos + 1, '='):
            token = initToken(tkAttributePipe)
            lexer.pos.inc 2

    of '^':
        if lexer.input.safeCharCompare(lexer.pos + 1, '='):
            token = initToken(tkAttributeStart)
            lexer.pos.inc 2

    of '$':
        if lexer.input.safeCharCompare(lexer.pos + 1, '='):
            token = initToken(tkAttributeEnd)
            lexer.pos.inc 2

    of ',':
        token = initToken(tkComma)
        lexer.pos.inc

    else:
        var buffer = ""
        if optUnicodeIdentifiers in lexer.options:
            readIdentifier(lexer.input, lexer.pos, buffer)
        else:
            readIdentifierAscii(lexer.input, lexer.pos, buffer)

        if buffer.len == 0:
            let rune = lexer.input.runeAt(lexer.pos)
            raise newUnexpectedCharacterException($rune)

        if lexer.next.kind in CombinatorKinds + {tkComma, tkInvalid}:
            token = initToken(tkElement, buffer.toLowerAscii)
        else:
            token = initToken(tkIdentifier, buffer)

    if not skip:
        if token.kind == tkInvalid:
            raise newUnexpectedCharacterException(ch)

        # TODO: It might be wise to perform some validation here.
        #       e.g tkParam is only valid after tkPseudoNot tkPseudoNth*
        lexer.current = lexer.next
        lexer.next = token
    else:
        lexer.forward

proc initLexer(input: string, options: set[QueryOption]): Lexer =
    # TODO: Get rid of strip
    result.input = strutils.strip(input)
    result.pos = 0
    result.options = options
    forward(result)
    forward(result)

proc eat(lexer: var Lexer, kind: set[TokenKind]): Token =
    if lexer.next.kind notin kind:
        raise newException(ParseError, "")
    lexer.forward()
    result = lexer.current

proc eat(lexer: var Lexer, kind: TokenKind): Token {.inline.} =
    lexer.eat({kind})

func hasAttr(node: XmlNode, attr: string): bool {.inline.} =
    return not node.attrs.isNil and node.attrs.hasKey(attr)

func validateNth(a, b, nSiblings: int): bool =
    if a == 0:
        return nSiblings == b - 1
    let n = (nSiblings - (b - 1)) / a
    return n.floor == n and n >= 0

func satisfies(pair: NodeWithParent, demands: seq[Demand]): bool
               {.raises: [Defect], gcsafe.}

func satisfies(pair: NodeWithParent, demand: Demand): bool =
    let node = pair.node

    case demand.kind
    of tkAttributeExists:
        return node.hasAttr(demand.attrName)

    of tkAttributeItem:
        return node.hasAttr(demand.attrName) and
            (demand.attrValue.len > 0) and
            demand.attrValue in node.attr(demand.attrName).split(CssWhitespace)

    # Empty attrValue is allowed,
    # and will match any value starting with '-'
    of tkAttributePipe:
        return node.hasAttr(demand.attrName) and
            demand.attrValue == node.attr(demand.attrName).split("-")[0]

    of tkAttributeExact:
        return node.attr(demand.attrName) == demand.attrValue

    of tkAttributeStart:
        return demand.attrValue.len > 0 and
            node.attr(demand.attrName).startsWith(demand.attrValue)

    of tkAttributeEnd:
        return demand.attrValue.len > 0 and
            node.attr(demand.attrName).endsWith(demand.attrValue)

    of tkAttributeSubstring:
        return demand.attrValue.len > 0 and
            node.attr(demand.attrName) in demand.attrValue

    of tkElement:
        return node.tag == demand.element

    of tkPseudoEmpty:
        return node.len == 0

    of tkPseudoOnlyChild:
        for siblingPair in pair.parent.children:
            if siblingPair.node != node:
                return false
        return true

    of tkPseudoOnlyOfType:
        for siblingPair in pair.parent.children:
            if siblingPair.node != node and
                    siblingPair.node.tag == node.tag:
                return false
        return true

    of tkPseudoFirstChild:
        return pair.elementIndex == 0

    of tkPseudoLastChild:
        for siblingPair in pair.parent.children(offset = pair):
            return false
        return true

    of tkPseudoFirstOfType:
        for siblingPair in pair.parent.children:
            if siblingPair.node.tag == node.tag:
                return siblingPair.node == node

    of tkPseudoLastOfType:
        for siblingPair in pair.parent.children(offset = pair):
            if siblingPair.node.tag == node.tag:
                return false
        return true

    of tkPseudoNot:
        return not pair.satisfies(demand.notQuery.demands)

    of tkPseudoNthChild:
        return validateNth(demand.a, demand.b, pair.elementIndex)

    of tkPseudoNthLastChild:
        var nSiblingsAfter = 0
        for siblingPair in pair.parent.children(offset = pair):
            nSiblingsAfter.inc
        return validateNth(demand.a, demand.b, nSiblingsAfter)

    of tkPseudoNthOfType:
        var nSiblingsOfTypeBefore = 0
        for siblingPair in pair.parent.children:
            if siblingPair.node == node:
                break
            elif siblingPair.node.tag == node.tag:
                nSiblingsOfTypeBefore.inc

        return validateNth(demand.a, demand.b, nSiblingsOfTypeBefore)

    of tkPseudoNthLastOfType:
        var nSiblingsOfTypeAfter = 0
        for siblingPair in pair.parent.children(offset = pair):
            if siblingPair.node.tag == node.tag:
                nSiblingsOfTypeAfter.inc

            return validateNth(demand.a, demand.b, nSiblingsOfTypeAfter)
    else:
        raiseAssert "Invalid demand: " & $demand

func satisfies(pair: NodeWithParent, demands: seq[Demand]): bool =
    for demand in demands:
        if not pair.satisfies(demand):
            return false
    return true

iterator searchDescendants(queryPart: QueryPart,
                           position: NodeWithParent): NodeWithParent =
    var queue = initDeque[NodeWithParent]()
    for nodeData in position.node.children:
        queue.addLast((parent: position.node, index: nodeData.index,
            elementIndex: nodeData.elementIndex))

    while queue.len > 0:
        let pair = queue.popFirst()
        if pair.satisfies queryPart.demands:
            yield pair

        for nodeData in pair.node.children:
            queue.addLast((parent: pair.node, index: nodeData.index,
                elementIndex: nodeData.elementIndex))

iterator searchChildren(queryPart: QueryPart,
                        position: NodeWithParent): NodeWithParent =
    for pair in position.node.children:
        if pair.satisfies queryPart.demands:
            yield pair

iterator searchSiblings(queryPart: QueryPart,
                        position: NodeWithParent): NodeWithParent =
    for pair in position.parent.children(offset = position):
        if pair.satisfies queryPart.demands:
            yield pair

iterator searchNextSibling(queryPart: QueryPart,
                           position: NodeWithParent): NodeWithParent =
    # It's a bit silly to have an iterator which will only yield 0 or 1
    # element, but it's nice for consistency with how the other
    # combinators are implemented.

    for pair in position.parent.children(offset = position):
        if pair.satisfies queryPart.demands:
            yield pair
        break # by definition, there can only be one next sibling

type SearchIterator = iterator(q: QueryPart,
                               p: NodeWithParent): NodeWithParent {.inline.}

func exec(parts: seq[QueryPart],
          root: NodeWithParent,
          single: bool,
          options: set[QueryOption],
          result: var seq[XmlNode]) =
    var combinator = cmDescendants
    var buffer = initDeque[NodeWithParent]()
    var partIndex = 0
    buffer.addLast root

    template search(position: NodeWithParent, itr: SearchIterator) =
        for next in itr(parts[partIndex], position):
            if partIndex == high(parts):
                result.add next.node
                if single:
                    return
            else:
                buffer.addLast next

            if not parts[partIndex].canFindMultiple(combinator, options):
                break

    while buffer.len > 0:
        for _ in 0..<buffer.len:
            let position = buffer.popFirst
            case combinator
            of cmDescendants: search(position, searchDescendants)
            of cmChildren:    search(position, searchChildren)
            of cmSiblings:    search(position, searchSiblings)
            of cmNextSibling: search(position, searchNextSibling)
            of cmLeaf: discard

        combinator = parts[partIndex].combinator
        partIndex.inc

func exec*(query: Query, root: XmlNode, single: bool): seq[XmlNode]
           {.raises: [Defect].} =
    ## Execute an already parsed query. If `single = true`,
    ## it will never return more than one element.
    result = newSeq[XmlNode]()

    # The <wrapper> element is needed due to how execRecursive is implemented.
    # The "current" position is never matched against,
    # only the childs/siblings (depending on combinator). So to make sure that
    # the original root is tested, we need to set the starting position to an
    # imaginary wrapper element. Since `NodeWIthParent` always require a parent,
    # we also add a wrapper-root element.
    let wrapper = <>wrapper(root)
    let wRoot = (parent: <>"wrapper-root"(wrapper), index: 0, elementIndex: 0)
    for parts in query.queries:
        parts.exec(wRoot, single, query.options, result)

func parseHtmlQuery*(queryString: string,
                     options: set[QueryOption] = DefaultQueryOptions): Query
                     {.raises: [Defect, ParseError].} =
    ## Parses a query for later use.
    ## Raises `ParseError` if parsing of `queryString` fails.
    result.queries = @[]
    result.queryStr = queryString
    var parts = newSeq[QueryPart]()
    var demands = newSeq[Demand]()
    var lexer = initLexer(queryString, options)

    try:
        while true:
            case lexer.current.kind

            of tkClass:
                demands.add initAttributeDemand(tkAttributeItem, "class",
                    lexer.eat(tkIdentifier).value)

            of tkId:
                demands.add initAttributeDemand(tkAttributeExact, "id",
                    lexer.eat(tkIdentifier).value)

            of tkElement:
                demands.add initElementDemand(lexer.current.value)

            of tkBracketStart:
                let f = lexer.eat(tkIdentifier)
                let nkind = lexer.next.kind
                case nkind
                of AttributeKinds - {tkAttributeExists}:
                    discard lexer.eat(nkind)
                    let v = lexer.eat({tkIdentifier, tkString})
                    demands.add initAttributeDemand(nkind, f.value, v.value)
                    discard lexer.eat(tkBracketEnd)
                of tkBracketEnd:
                    demands.add initAttributeDemand(tkAttributeExists,
                        f.value, "")
                    discard lexer.eat(tkBracketEnd)
                else:
                    raise newException(ParseError, "")

            of PseudoNoParamsKinds:
                demands.add initPseudoDemand(lexer.current.kind)

            of PseudoParamsKinds:
                let pseudoKind = lexer.current.kind
                let params = lexer.eat(tkParam)
                case pseudoKind
                of tkPseudoNot:
                    # Not the cleanest way to this, but eh
                    let notQuery = parseHtmlQuery(params.value, options)

                    if not notQuery.isValidNotQuery(options):
                        raise newException(ParseError,
                            ":not argument must be a simple selector, but " &
                            "was '" & params.value & "'")

                    demands.add initNotDemand(notQuery.queries[0][0])
                of NthKinds:
                    let (a, b) = parsePseudoNthArguments(params.value)
                    demands.add initNthChildDemand(pseudoKind, a, b)
                else: doAssert(false) # can't happen

            of CombinatorKinds:
                parts.add initQueryPart(demands, lexer.current.kind.Combinator)
                demands = @[]

            of tkComma:
                parts.add initQueryPart(demands, cmLeaf)
                result.queries.add parts
                demands = @[]
                parts = @[]

            of tkIdentifier, tkString, tkBracketEnd,
                    tkParam, tkInvalid, AttributeKinds:
                raise newException(ParseError, "")

            of tkEoi:
                break

            lexer.forward()
    except ParseError as err:
        let msg =
            if err.msg == "":
                "Failed to parse CSS query '" & queryString & "'"
            else:
                "Failed to parse CSS query '" & queryString & "': " & err.msg
        raise newException(ParseError, msg)

    parts.add initQuerypart(demands, cmLeaf)
    result.queries.add parts
    result.options = options

    log "\ninput: \n" & queryString
    # log "\nquery: \n" & result.debugToString

func querySelector*(root: XmlNode, queryString: string,
                    options: set[QueryOption] = DefaultQueryOptions): XmlNode
                    {.raises: [Defect, ParseError].} =
    ## Get the first element matching `queryString`,
    ## or `nil` if no such element exists.
    ## Raises `ParseError` if parsing of `queryString` fails.
    let query = parseHtmlQuery(queryString, options)
    let lst = query.exec(root, single = true)
    if lst.len > 0:
        lst[0]
    else:
        nil

func querySelectorAll*(root: XmlNode, queryString: string,
                       options: set[QueryOption] = DefaultQueryOptions):
                       seq[XmlNode] {.raises: [Defect, ParseError].} =
    ## Get all elements matching `queryString`.
    ## Raises `ParseError` if parsing of `queryString` fails.
    let query = parseHtmlQuery(queryString, options)
    result = query.exec(root, single = false)
