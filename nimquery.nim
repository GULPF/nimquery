# Spec: https://www.w3.org/TR/css3-selectors/

# TODO:
#  - If I change PartialQuery.Combinator to indicate combinator of
#    PartialQuery, it will simplify a lot of things, including
#    making the comma optimizations better.

import xmltree
import strutils
import strtabs
import unicode
import math
import deques

const DEBUG = false

type
    ParseError* = object of Exception

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
            notQuery: PartialQuery
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
        optSimpleNot          ## Disallow more complex :not selectors.
                              ## Annoying but that's the spec.
                              ## Combinators/comma are not allowed even if true.

    Lexer = object
        input: string
        pos: int
        options: set[QueryOption]
        current, next: Token

    Query* = object ## Represents a parsed query.
        roots: seq[PartialQuery] # Because of the comma operator,
                                 # a query can consist of multiple
                                 # chained queries.
        options: set[QueryOption]

    PartialQuery = ref object
        # `nextQueries` will never contain more than one element while parsing.
        # It's used in `optimize(q: Query)` to partialy merge similiar queries.
        nextQueries: seq[PartialQuery]
        # Elements which satiesfies these demands are matched by the query.
        demands: seq[Demand]
        # Indicates what type of search should be used for `nextQueries`.
        combinator: Combinator

    SearchContext = object
        options: set[QueryOption]
        position: NodeWithParent
        combinator: Combinator
        single: bool

{.deprecated: [NimqueryOption: QueryOption].}

const DefaultQueryOptions* = { optUniqueIds, optUnicodeIdentifiers,
    optSimpleNot }
const NimqueryDefaultOptions* {.deprecated.} = DefaultQueryOptions

const Identifiers = Letters + Digits + { '-', '_', '\\' }
# NOTE: This is not the same as `strutils.Whitespace`.
#       These values are defined by spec.
const CssWhitespace = { '\x20', '\x09', '\x0A', '\x0D', '\x0C' }
const Combinators = CssWhitespace + {  '+', '~', '>' }

const PseudoNoParamsKinds = {
    tkPseudoFirstOfType, tkPseudoLastOfType,
    tkPseudoOnlyChild, tkPseudoOnlyOfType,
    tkPseudoEmpty, tkPseudoFirstChild,
    tkPseudoLastChild
}

const PseudoParamsKinds = NthKinds + { tkPseudoNot }

const CombinatorKinds = {
    tkCombinatorChildren, tkCombinatorDescendents,
    tkCombinatorNextSibling, tkCombinatorSiblings
}

proc satisfies(pair: NodeWithParent, demands: seq[Demand]): bool
proc `$`(q: PartialQuery): string {. noSideEffect .}
proc parseHtmlQuery*(queryString: string,
                     options: set[QueryOption] = DefaultQueryOptions): Query

template log(msg: string): typed =
    when DEBUG:
        echo msg

proc indent(str, space: string): string =
    result = str.replace("\n", "\n" & space)

proc safeCharCompare(str: string, idx: int, cs: set[char]): bool {. inline .} =
    if idx > high(str): return false
    if idx < low(str): return false
    return str[idx] in cs

proc safeCharCompare(str: string, idx: int, c: char): bool {. inline .} =
    return str.safeCharCompare(idx, { c })

proc node(pair: NodeWithParent): XmlNode =
    return pair.parent[pair.index]

proc `$`(comb: Combinator): string =
    case comb
    of cmDescendants: return " "
    of cmNextSibling: return " + "
    of cmSiblings: return " ~ "
    of cmChildren: return " > "
    of cmLeaf: return ""

proc attrComparerString(kind: TokenKind): string =
    case kind
    of tkAttributeExact: return "="
    of tkAttributeItem: return "~="
    of tkAttributePipe: return "|="
    of tkAttributeExists: return ""
    of tkAttributeStart: return "^="
    of tkAttributeEnd: return "$="
    of tkAttributeSubstring: return "*="
    else: raise newException(Exception, "Invalid attr kind: " & $kind)

proc newUnexpectedCharacterException(s: string): ref ParseError =
    return newException(ParseError, "Unexpected character: '" & s & "'")

proc newUnexpectedCharacterException(c: char): ref ParseError =
    newUnexpectedCharacterException($c)

proc newParseException(q: string): ref ParseError =
    return newException(ParseError, "Failed to parse HTML query '" & q & "'")

proc initDemand(kind: TokenKind, notQuery: PartialQuery): Demand =
    result.kind = kind
    result.notQuery = notQuery

proc initDemand(kind: TokenKind, element: string): Demand =
    result.kind = kind
    result.element = element

proc initPseudoDemand(kind: TokenKind): Demand =
    result.kind = kind

proc initAttributeDemand(kind: TokenKind, attrName, attrValue: string): Demand =
    result.kind = kind
    result.attrName = attrName
    result.attrValue = attrValue

proc initPseudoDemand(kind: TokenKind, a, b: int): Demand =
    result.kind = kind
    result.a = a
    result.b = b

proc `$`(demand: Demand): string =
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
        result =  ":" & $demand.kind & "(" & $demand.a & "n, " & $demand.b & ")"
    of PseudoNoParamsKinds:
        result  = ":" & $demand.kind
    of tkElement:
        result = demand.element
    else:
        result = $demand.kind

proc `==`(d1, d2: Demand): bool =
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

proc initToken(kind: TokenKind, value: string = ""): Token =
    return Token(kind: kind, value: value)

proc `$`(token: Token): string =
    result = "[" & $token.kind
    if not token.value.isNilOrEmpty:
        result.add " : " & token.value & "]"
    else:
        result.add "]"

proc newPartialQuery(demands: seq[Demand],
                     combinator: Combinator): PartialQuery =
    return PartialQuery(demands: demands, combinator: combinator)

proc debugToString(q: PartialQuery): string =
    result = ""

    for idx, d in q.demands:
        result.add "[" & $d.kind & " " & $d & "]"
        if idx != high(q.demands):
            result.add ", "

    if q.nextQueries.len > 0:
        result.add "\n\t"

        var joined = ""
        for idx, part in q.nextQueries:
            joined.add part.debugToString
            if idx != high(q.nextQueries):
                joined.add "\n"
        result.add joined.indent "\t"

proc rootStrings(q: PartialQuery): seq[string] =
    var common = ""
    var current = q

    common.add current.demands.join("")
    common.add $current.combinator

    while current.nextQueries.len == 1:
        current = current.nextQueries[0]
        common.add current.demands.join("")
        common.add $current.combinator

    if current.nextQueries.len > 1:
        result = @[]
        for part in current.nextQueries:
            for str in part.rootStrings:
                result.add common & str
    else:
        result = @[common]

proc `$`(q: PartialQuery): string =
    q.rootStrings.join ", "

proc isIdenticalRoot(q1, q2: PartialQuery): bool =
    if q1.demands.len != q2.demands.len or q1.combinator != q2.combinator:
        return false

    for d1 in q1.demands:
        var found = false
        for d2 in q2.demands:
            if d1 == d2:
                found = true

        if not found:
            return false

    return true

proc append(q: var PartialQuery, demands: seq[Demand], combinator: Combinator) =
    if q.isNil:
        q = newPartialQuery(demands, combinator)
    else:
        var itr = q
        while itr.nextQueries.len > 0:
            itr = itr.nextQueries[0]
        itr.nextQueries = @[ newPartialQuery(demands, combinator) ]

proc canFindMultiple(q: PartialQuery, comb: Combinator,
                     options: set[QueryOption]): bool =
    # Returns true if the current queries demands can be satisfied by
    # multiple elements. This is used to check if the search should stop
    # after the first element has been found.
    for demand in q.demands:
        if optUniqueIds in options and demand.kind in AttributeKinds and
                demand.attrName == "id":
            return false
        if comb in { cmChildren, cmSiblings } and demand.kind in
                { tkPseudoFirstOfType, tkPseudoLastOfType,
                    tkPseudoFirstChild, tkPseudoLastChild, tkPseudoOnlyOfType }:
            return false

    return true

proc `$`*(q: Query): string =
    result = q.roots.join "\n"

proc debugToString(q: Query): string =
    result = ""
    for idx, root in q.roots:
        result.add root.debugToString
        if idx != high(q.roots):
            result.add "\n"

proc optimize(query: var Query) =
    # Optimizes similiar looking root queries (created with the comma operator)
    # by partially merging them. This way, the common part of the qoot queries
    # only has to be searched for once.
    # E.g in the query `div p, div a` all div elements only needs to be found once.
    #
    # This implementation is not perfect, but it might be good enough.
    # It prioritizes merging roots to the start of the root list,
    # which is arbitrary (but predictable).
    # The best optimization is dependent on the structure of the HTML
    # document being queried anyway so I don't think it matters.
    #
    # It doesn't optimize partials with different combinators,
    # e.g `div > p, div a` isn't optimized.
    # This might be easy enough to fix that it's worth it,
    # but it's a rare enough case that I don't care for now.

    if query.roots.len == 1: return

    var mergeToIdx = 0
    var mergeFromIdx = 1

    while mergeToIdx < high(query.roots):
        if isIdenticalRoot(query.roots[mergeFromIdx], query.roots[mergeToIdx]):

            var qFrom = query.roots[mergeFromIdx]
            var qTo = query.roots[mergeToIdx]

            while isIdenticalRoot(qFrom.nextQueries[0], qTo.nextQueries[0]):
                qFrom = qFrom.nextQueries[0]
                qTo = qTo.nextQueries[0]

            qTo.nextQueries.add qFrom.nextQueries
            query.roots.delete mergeFromIdx

        else:
            mergeFromIdx.inc

        if mergeFromIdx > high(query.roots):
            mergeToIdx.inc
            mergeFromIdx = mergeToIdx + 1

proc isValidNotQuery(q: Query, options: set[QueryOption]): bool =
    return
        q.roots.len == 1 and
        q.roots[0].nextQueries.len == 0 and
        (q.roots[0].demands.len == 1 or not (optSimpleNot in options))

proc initSearchContext(pos: NodeWithParent, comb: Combinator, single: bool,
                       options: set[QueryOption]): SearchContext =
    SearchContext(position: pos, combinator: comb,
        options: options, single: single)

proc forward(ctx: SearchContext, pos: NodeWithParent,
             comb: Combinator): SearchContext =
    # Create the next context state, going forward in the search
    initSearchContext(pos, comb, ctx.single, ctx.options)

proc readNumerics(input: string, idx: var int, buffer: var string) =
    while input[idx] in Digits:
        buffer.add input[idx]
        idx.inc

proc readEscape(input: string, idx: var int, buffer: var string) =
    assert input[idx] == '\\'
    idx.inc

    # Linefeed, carriage return and form feed can't be escaped.
    if input[idx] in { '\x0C', '\x0D', '\x0A'}:
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

        let runeStr = hexStr.parseHexInt.Rune.toUTF8
        buffer.add runeStr

proc readStringLiteral(input: string, idx: var int, buffer: var string) =
    assert input[idx] in { '\'', '"' }

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

proc readIdentifier(input: string, idx: var int, buffer: var string) =
    const intIdentifiers = {
        'a'.int .. 'z'.int, 'A'.int .. 'Z'.int,
        '0'.int .. '9'.int,
        '-'.int, '_'.int, '\\'.int
    }

    if input[idx] == '_' or
            input[idx] in Digits or
            (input[idx] == '-' and
                input.safeCharCompare(idx + 1, { '-' } + Digits)):
        raise newUnexpectedCharacterException(input[idx + 1])

    proc isValidIdentifier(rune: Rune): bool =
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

proc readIdentifierAscii(input: string, idx: var int, buffer: var string) =
    if input[idx] == '-' and input.safeCharCompare(idx + 1, { '-' } + Digits):
        raise newUnexpectedCharacterException(input[idx + 1])

    while input[idx] in Identifiers and idx < input.len:
        if input[idx] == '\\':
            readEscape(input, idx, buffer)
        else:
            buffer.add input[idx]
            idx.inc

proc readParams(input: string, idx: var int, buffer: var string) =
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

proc parsePseudoNthArguments(raw: string): tuple[a: int, b: int] =
    let input = raw.strip
    if input == "odd":
        return (2, 1)
    elif input == "even":
        return (2, 0)
    else:
        var idx = 0
        var a = ""
        var b = ""
        var buffer = ""

        # NOTE: Spacing between first sign and `a` is not allowed.
        while idx < input.len:

            var allowSpace = true

            case input[idx]
            of { '+', '-' }:
                buffer.add $input[idx]
                # NOTE: Spaces is allowed around second sign,
                #       but not around first.
                allowSpace = false
                idx.inc
            of Digits:
                readNumerics input, idx, buffer

                if input[idx] == 'n':
                    if not a.isNilOrEmpty:
                        raise newUnexpectedCharacterException(input[idx])
                    a = buffer
                    idx.inc
                else:
                    b = buffer
                buffer.setLen 0
                allowSpace = true
            of 'n':
                if not a.isNilOrEmpty:
                    raise newUnexpectedCharacterException(input[idx])
                buffer.add "1"
                a = buffer
                idx.inc
                buffer.setLen 0
                allowSpace = true
            of CssWhitespace:
                if allowSpace:
                    idx.inc
                else:
                    raise newUnexpectedCharacterException(input[idx])
            else:
                raise newUnexpectedCharacterException(input[idx])

        if a.isNilOrEmpty: a = "0"
        if b.isNilOrEmpty: b = "0"
        # Should be safe to parse
        return (a.parseInt, b.parseInt)

proc initPseudoToken(str: string): Token =
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
        raise newException(ParseError, "Unknown pseudo: " & str)
    result = initToken(kind)

proc isFinishedSimpleSelector(prev: Token, prevPrev: Token): bool =
    # Checks if the last two tokens represents the end of a simple selector.
    # This is needed to determine if a space is significant or not.
    if prev.kind in { tkBracketEnd, tkParam, tkElement } + PseudoNoParamsKinds:
        return true
    if prev.kind == tkIdentifier and prevPrev.kind in { tkClass, tkId }:
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

    of { '"', '\'' }:
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
        while lexer.input[lexer.pos] in Identifiers and
                lexer.pos < lexer.input.len:
            buffer.add lexer.input[lexer.pos]
            lexer.pos.inc

        token = initPseudoToken(buffer)

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

        if buffer.isNilOrEmpty:
            let rune = lexer.input.runeAt(lexer.pos)
            raise newUnexpectedCharacterException($rune)

        if lexer.next.kind in CombinatorKinds + { tkComma, tkInvalid }:
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
    result.input = input.strip
    result.pos = 0
    result.options = options
    forward(result)
    forward(result)

proc eat(lexer: var Lexer, kind: set[TokenKind]): Token =
    if lexer.next.kind notin kind:
        raise newParseException(lexer.input)
    lexer.forward()
    result = lexer.current

proc eat(lexer: var Lexer, kind: TokenKind): Token {.inline.} =
    lexer.eat({ kind })

proc hasAttr(node: XmlNode, attr: string): bool {. inline .} =
    return not node.attrs.isNil and node.attrs.hasKey(attr)

proc validateNth(a, b, nSiblings: int): bool =
    if a == 0:
        return nSiblings == b - 1
    let n = (nSiblings - (b - 1)) / a
    return n.floor == n and n >= 0

proc satisfies(pair: NodeWithParent, demand: Demand): bool =
    let node = pair.node

    case demand.kind
    of tkAttributeExists:
        return node.hasAttr(demand.attrName)

    of tkAttributeItem:
        return node.hasAttr(demand.attrName) and
            (not demand.attrValue.isNilOrEmpty) and
            demand.attrValue in node.attr(demand.attrName).split(CssWhitespace)

    # Empty attrValue is allowed,
    # and will match any value starting with '-'
    of tkAttributePipe:
        return node.hasAttr(demand.attrName) and
            demand.attrValue == node.attr(demand.attrName).split("-")[0]

    of tkAttributeExact:
        return node.attr(demand.attrName) == demand.attrValue

    of tkAttributeStart:
        return not demand.attrValue.isNilOrEmpty and
            node.attr(demand.attrName).startsWith(demand.attrValue)

    of tkAttributeEnd:
        return not demand.attrValue.isNilOrEmpty and
            node.attr(demand.attrName).endsWith(demand.attrValue)

    of tkAttributeSubstring:
        return not demand.attrValue.isNilOrEmpty and
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
        raise newException(ParseError, "Invalid demand: " & $demand)

proc satisfies(pair: NodeWithParent, demands: seq[Demand]): bool =
    for demand in demands:
        if not pair.satisfies(demand):
            return false

    return true

iterator searchDescendants(queryRoot: PartialQuery,
                           position: NodeWithParent): NodeWithParent =
    var queue = initDeque[NodeWithParent]()
    for nodeData in position.node.children:
        queue.addLast((parent: position.node, index: nodeData.index,
            elementIndex: nodeData.elementIndex))

    while queue.len > 0:
        let pair = queue.popFirst()
        if pair.satisfies queryRoot.demands:
            yield pair

        for nodeData in pair.node.children:
            queue.addLast((parent: pair.node, index: nodeData.index,
                elementIndex: nodeData.elementIndex))

iterator searchChildren(queryRoot: PartialQuery,
                        position: NodeWithParent): NodeWithParent =
    for pair in position.node.children:
        if pair.satisfies queryRoot.demands:
            yield pair

iterator searchSiblings(queryRoot: PartialQuery,
                        position: NodeWithParent): NodeWithParent =
    for pair in position.parent.children(offset = position):
        if pair.satisfies queryRoot.demands:
            yield pair

iterator searchNextSibling(queryRoot: PartialQuery,
                           position: NodeWithParent): NodeWithParent =
    # It's a bit silly to have an iterator which will only yield 0 or 1
    # element, but it's nice for consistency with how the other
    # combinators are implemented.

    for pair in position.parent.children(offset = position):
        if pair.satisfies queryRoot.demands:
            yield pair
        break # by definition, there can only be one next sibling

type SearchIterator = iterator(q: PartialQuery,
                               p: NodeWithParent): NodeWithParent {.inline.}

proc execRecursive(queryRoot: PartialQuery, context: SearchContext,
                   output: var seq[XmlNode]) =
    var position = context.position

    template search(itr: SearchIterator): typed =
        for next in itr(queryRoot, position):
            if queryRoot.nextQueries.len == 0:
                output.add next.node
            else:
                for subquery in queryRoot.nextQueries:
                    let newContext = context.forward(next, queryRoot.combinator)
                    subquery.execRecursive(newContext, output)

            if context.single and output.len > 0:
                break
            # TODO: This should only be checked once?
            if not queryRoot.canFindMultiple(context.combinator,
                    context.options):
                break

    case context.combinator
    of cmDescendants: search(searchDescendants)
    of cmChildren:    search(searchChildren)
    of cmSiblings:    search(searchSiblings)
    of cmNextSibling: search(searchNextSibling)
    of cmLeaf: discard

template DQO: untyped = DefaultQueryOptions

proc exec*(query: Query, root: XmlNode,
        single: bool): seq[XmlNode] =
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
    for queryRoot in query.roots:
        let context = initSearchContext(wRoot, cmDescendants, single,
            query.options)
        queryRoot.execRecursive(context, result)

proc parseHtmlQuery*(queryString: string,
        options: set[QueryOption] = DQO): Query =
    ## Parses a query for later use.
    ## Raises `ParseError` if parsing of `queryString` fails.
    result.roots = @[]
    var queryRoot: PartialQuery = nil
    var demandBuffer = newSeq[Demand]()
    var lexer = initLexer(queryString, options)

    while true:
        case lexer.current.kind

        of tkClass:
            demandBuffer.add initAttributeDemand(tkAttributeItem, "class",
                lexer.eat(tkIdentifier).value)

        of tkId:
            demandBuffer.add initAttributeDemand(tkAttributeExact, "id",
                lexer.eat(tkIdentifier).value)

        of tkElement:
            demandBuffer.add initDemand(tkElement, lexer.current.value)

        of tkBracketStart:
            let f = lexer.eat(tkIdentifier)
            let nkind = lexer.next.kind
            case nkind
            of AttributeKinds - { tkAttributeExists }:
                discard lexer.eat(nkind)
                let v = lexer.eat({ tkIdentifier, tkString })
                demandBuffer.add initAttributeDemand(nkind, f.value, v.value)
                discard lexer.eat(tkBracketEnd)
            of tkBracketEnd:
                demandBuffer.add initAttributeDemand(tkAttributeExists,
                    f.value, "")
                discard lexer.eat(tkBracketEnd)
            else:
                raise newParseException(queryString)

        of PseudoNoParamsKinds:
            demandBuffer.add initPseudoDemand(lexer.current.kind)

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

                demandBuffer.add initDemand(tkPseudoNot, notQuery.roots[0])
            of NthKinds:
                let (a, b) = parsePseudoNthArguments(params.value)
                demandBuffer.add initPseudoDemand(pseudoKind, a, b)
            else: doAssert(false) # can't happen

        of CombinatorKinds:
            queryRoot.append demandBuffer, lexer.current.kind.Combinator
            demandBuffer = @[]

        of tkComma:
            queryRoot.append demandBuffer, cmLeaf
            result.roots.add queryRoot
            demandBuffer = @[]
            queryRoot = nil

        of tkIdentifier, tkString, tkBracketEnd,
                tkParam, tkInvalid, AttributeKinds:
            raise newParseException(queryString)

        of tkEoi:
            break

        lexer.forward()

    queryRoot.append demandBuffer, cmLeaf
    result.roots.add queryRoot
    result.optimize
    result.options = options

    log "\ninput: \n" & queryString
    log "\nquery: \n" & result.debugToString

proc querySelector*(root: XmlNode, queryString: string,
        options: set[QueryOption] = DQO): XmlNode =
    ## Get the first element matching `queryString`,
    ## or `nil` if no such element exists.
    ## Raises `ParseError` if parsing of `queryString` fails.
    let query = parseHtmlQuery(queryString, options)
    let lst = query.exec(root, single = true)
    if lst.len > 0:
        lst[0]
    else:
        nil

proc querySelectorAll*(root: XmlNode, queryString: string,
        options: set[QueryOption] = DQO): seq[XmlNode] =
    ## Get all elements matching `queryString`.
    ## Raises `ParseError` if parsing of `queryString` fails.
    let query = parseHtmlQuery(queryString, options)
    result = query.exec(root, single = false)
