# Spec: https://www.w3.org/TR/css3-selectors/

# TODO:
#  - If I change PartialQuery.Combinator to indicate combinator of PartialQuery,
#    it will simplify a lot of things, including making the comma optimizations better.

import xmltree
import strutils
import strtabs
import unicode
import math

const DEBUG = false

type
    ParseError* = object of Exception

    TokenKind = enum
        tkBracketStart, tkBracketEnd
        tkParam
        tkComma

        # NOTE: These are handled the same in some contexts, but they are different.
        #       `tkIdentifier` can only contain a very specific subset of characters,
        #       but tkString can contain anything.
        #       This means that both `#foo%` and `[id=foo%]` is invalid, but not `[id="foo%"]` or `#foo\%`.
        tkIdentifier, tkString

        tkClass, tkId, tkElement, tkUniversal

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

    Token = ref object
        kind: TokenKind
        value: string

const attributeKinds = {
    tkAttributeExact, tkAttributeItem,
    tkAttributePipe, tkAttributeExists,
    tkAttributeStart, tkAttributeEnd,
    tkAttributeSubstring
}

const nthKinds = {
    tkPseudoNthChild, tkPseudoNthLastChild,
    tkPseudoNthOfType, tkPseudoNthLastOfType
}

type
    NilableDemand = ref object
        case kind: Tokenkind
        of attributeKinds:
            attrName, attrValue: string
        of nthKinds:
            a, b: int
        of tkPseudoNot:
            notQuery: PartialQuery
        of tkElement:
            element: string
        else: discard

    Demand = NilableDemand not nil

    NodeWithParent = tuple
        parent: XmlNode
        # Index is the index used by `xmltree`,
        # elementIndex is the index when only counting elements (not text nodes etc).
        index, elementIndex: int

    Combinator = enum
        cmDescendants = tkCombinatorDescendents,
        cmChildren = tkCombinatorChildren,
        cmNextSibling = tkCombinatorNextSibling,
        cmSiblings = tkCombinatorSiblings,
        cmLeaf # Special case for the last query
    
    NimqueryOption* = enum
        # Assume unique id's or not
        optUniqueIds 
        # Allow non-ascii in identifiers (e.g `#exämple`)
        optUnicodeIdentifiers 
        # Disallow more complex :not selectors. Annoying but that's the spec.   
        # Combinators/comma are not allowed even if true.
        optSimpleNot

    # Because of the comma operator, a query can consist of multiple complete queries.
    Query* = ref object
        roots: seq[PartialQuery]
        options: set[NimqueryOption]
    
    PartialQuery = ref object
        # `nextQueries` will never contain more than one element while parsing.
        # It's used in `optimize(q: Query)` to partialy merge similiar queries.
        nextQueries: seq[PartialQuery]
        # Elements which satiesfies these demands are matched by the query.
        demands: seq[Demand]
        # Indicates what type of search should be used for `nextQueries`.
        combinator: Combinator

    SearchContext[single: static[bool]] = object
        options: set[NimqueryOption]
        position: NodeWithParent
        combinator: Combinator

const nimqueryDefaultOptions* = { optUniqueIds, optUnicodeIdentifiers, optSimpleNot }

const beginingIdentifiers = Letters + { '-', '\\' }
const identifiers = Letters + Digits + { '-', '_', '\\' }
const combinators = { ' ', '+', '~', '>' }

const pseudoNoParamsKinds = {
    tkPseudoFirstOfType, tkPseudoLastOfType,
    tkPseudoOnlyChild, tkPseudoOnlyOfType,
    tkPseudoEmpty, tkPseudoFirstChild,
    tkPseudoLastChild
}

const combinatorKinds = {
    tkCombinatorChildren, tkCombinatorDescendents,
    tkCombinatorNextSibling, tkCombinatorSiblings
}

proc satisfies(pair: NodeWithParent, demands: seq[Demand]): bool
proc `$`(q: PartialQuery): string {. noSideEffect .}
proc parseHtmlQuery*(queryString: string, options: set[NimqueryOption]): Query

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

proc newDemand(kind: static[TokenKind], notQuery: PartialQuery): Demand =
    return Demand(kind: kind, notQuery: notQuery)

proc newDemand(kind: static[TokenKind], element: string): Demand =
    return Demand(kind: kind, element: element)

proc newPseudoDemand(kind: TokenKind): Demand =
    result = Demand(kind: tkPseudoFirstOfType)
    result.kind = kind

proc newAttributeDemand(kind: TokenKind, attrName, attrValue: string): Demand =
    result = Demand(kind: tkAttributeExists, attrName: attrName, attrValue: attrValue)
    result.kind = kind

proc newPseudoDemand(kind: TokenKind, a, b: int): Demand =
    result = Demand(kind: tkPseudoNthChild, a: a, b: b)
    result.kind = kind

proc `$`(demand: Demand): string =
    case demand.kind:
    of attributeKinds:
        if demand.kind == tkAttributeExists:
            result = "[" & demand.attrName & "]"
        else:
            result = "[" & demand.attrName & demand.kind.attrComparerString & "'" & demand.attrValue & "']"
    of tkPseudoNot:
        result = ":" & $demand.kind & "(" & $demand.notQuery & ")"
    of nthKinds:
        result =  ":" & $demand.kind & "(" & $demand.a & "n, " & $demand.b & ")"
    of pseudoNoParamsKinds:
        result  = ":" & $demand.kind
    of tkElement:
        result = demand.element
    else:
        result = $demand.kind

proc `==`(d1, d2: Demand): bool =
    if d1.kind != d2.kind: return false
    case d1.kind
    of attributeKinds:
        return d1.attrName == d2.attrName and d1.attrValue == d2.attrValue
    of nthKinds:
        return d1.a == d2.b
    of tkPseudoNot:
        return d1.notQuery == d2.notQuery
    of tkElement:
        return d1.element == d2.element
    else:
        raise newException(Exception, "Invalid demand kind: " & $d1.kind)

iterator children(node: XmlNode, offset: NodeWithParent = (nil, -1, -1)): NodeWithParent =
    var idx = offset.index + 1
    var elIdx = offset.elementIndex + 1
    while idx < node.len:
        let el = node[idx]
        if el.kind == xnElement:
            yield (parent: node, index: idx, elementIndex: elIdx).NodeWithParent
            elIdx.inc
        idx.inc

proc newToken(kind: static[TokenKind], value: string = ""): Token =
    return Token(kind: kind, value: value)

proc `$`(token: Token): string =
    if token.isNil:
        return "[nil]"

    result = "[" & $token.kind
    if not token.value.isNilOrEmpty:
        result.add " : " & token.value & "]"
    else:
        result.add "]"

proc newPartialQuery(demands: seq[Demand], combinator: Combinator): PartialQuery =
    return PartialQuery(demands: demands, nextQueries: nil, combinator: combinator)

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

proc canFindMultiple(q: PartialQuery, comb: Combinator, options: set[NimqueryOption]): bool = 
    # Returns true if the current queries demands can be satisfied by multiple elements.
    # This is used to check if the search should stop after the first element has been found.
    for demand in q.demands:
        if optUniqueIds in options and demand.kind in attributeKinds and demand.attrName == "id":
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

proc optimize(query: Query) =
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

proc isValidNotQuery(q: Query, options: set[NimqueryOption]): bool =
    return
        q.roots.len == 1 and
        q.roots[0].nextQueries.len == 0 and
        (q.roots[0].demands.len == 1 or not (optSimpleNot in options))

proc initSearchContext(pos: NodeWithParent, comb: Combinator, single: static[bool], opts: set[NimqueryOption]): SearchContext[single] =
    SearchContext[single](position: pos, combinator: comb, options: opts)

proc forward[single: static[bool]](ctx: SearchContext[single], pos: NodeWithParent, comb: Combinator): SearchContext[single] =
    # Create the next context state, going forward in the search
    SearchContext[single](position: pos, combinator: comb, options: ctx.options)

proc readNumerics(input: string, idx: var int, buffer: var string) =
    while input[idx] in Digits:
        buffer.add input[idx]
        idx.inc

proc readEscape(input: string, idx: var int, buffer: var string) =
    const hexInput = HexDigits + { ' ' }
    var codePointStr = ""
    idx.inc
    while input[idx] in hexInput and codePointStr.len < 6:
        if input[idx] != ' ':
            codePointStr.add input[idx]
            idx.inc
        else:
            idx.inc
            break

    if codePointStr.isNilOrEmpty:
        buffer.add input[idx]
        idx.inc
    else:
        let unicodeCharacter = codePointStr.parseHexInt.Rune.toUTF8
        buffer.add unicodeCharacter

proc readStringLiteral(input: string, idx: var int, buffer: var string) =
    if input[idx] notin { '\'', '"' }: return 

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
            (input[idx] == '-' and input.safeCharCompare(idx + 1, { '-' } + Digits)):
        raise newUnexpectedCharacterException(input[idx + 1])

    proc isValidIdentifier(rune: Rune): bool =
        if rune.int32 in intIdentifiers:
            return true
        # Spec: https://www.w3.org/TR/CSS21/syndata.html#value-def-identifier
        return rune >=% 160.Rune

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

    while input[idx] in identifiers and idx < input.len:
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

    while input[idx] != ')' or paramContextCount > 0 or dblQuoteStringContext or sglQuoteStringContext:
        if input[idx] == '"' and not sglQuoteStringContext:
            dblQuoteStringContext = not dblQuoteStringContext

        if input[idx] == '\'' and not dblQuoteStringContext:
            sglQuoteStringContext = not sglQuoteStringContext

        if input[idx] == '(' and not dblQuoteStringContext and not sglQuoteStringContext:
            paramContextCount.inc

        if input[idx] == ')' and not dblQuoteStringContext and not sglQuoteStringContext:
            paramContextCount.dec

        if input[idx] == '\\':
            buffer.add input[idx]
            idx.inc

        buffer.add input[idx]
        idx.inc

        if idx > high(input):
            raise newException(ParseError, "Non-terminated pseudo argument list")

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
                # NOTE: Spaces is allowed around second sign, but not around first.
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
            of ' ':
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
        
proc newPseudoToken(str: string): Token =
    case str
    of ":empty":
        return newToken(tkPseudoEmpty)
    of ":only-child":
        return newToken(tkPseudoOnlyChild)
    of ":only-of-type":
        return newToken(tkPseudoOnlyOfType)
    of ":first-child":
        return newToken(tkPseudoFirstChild)
    of ":last-child":
        return newToken(tkPseudoLastChild)
    of ":last-of-type":
        return newToken(tkPseudoLastOfType)
    of ":first-of-type":
        return newToken(tkPseudoFirstOfType)
    of ":not":
        return newToken(tkPseudoNot)
    of ":nth-child":
        return newToken(tkPseudoNthChild)
    of ":nth-last-child":
        return newToken(tkPseudoNthLastChild)
    of ":nth-of-type":
        return newToken(tkPseudoNthOfType)
    of ":nth-last-of-type":
        return newToken(tkPseudoNthLastOfType)
    else:
        raise newException(ParseError, "Unknown pseudo: " & str)

proc reduce(stack: var seq[Token], demandBuffer: var seq[Demand], queryRoot: var PartialQuery, query: Query, options: set[NimqueryOption]) =
    if stack.len == 0:
        return

    let peek = high(stack)
    let prev = stack[peek]

    case prev.kind

    of tkIdentifier:
        if stack.len == 1:
            raise newException(ParseError, "Unexpected identifier: " & prev.value)

        elif stack[^2].kind == tkClass:
            let demand = newAttributeDemand(tkAttributeItem, "class", prev.value)
            demandBuffer.add demand
            stack.setLen stack.len - 2

        elif stack[^2].kind == tkId:
            let demand = newAttributeDemand(tkAttributeExact, "id", prev.value)
            demandBuffer.add demand
            stack.setLen stack.len - 2

    of tkElement:
        let demand = newDemand(tkElement, prev.value)
        demandBuffer.add demand
        stack.setLen stack.len - 1

    of tkBracketEnd:
        if stack[^3].kind in attributeKinds - { tkAttributeExists }:
            let demand = newAttributeDemand(stack[^3].kind, stack[^4].value, stack[^2].value)
            demandBuffer.add demand
            stack.setLen stack.len - 5

        else:
            let demand = newAttributeDemand(tkAttributeExists, stack[^2].value, "")
            demandBuffer.add demand
            stack.setLen stack.len - 3

    of pseudoNoParamsKinds:
        let demand = newPseudoDemand(prev.kind)
        demandBuffer.add demand
        stack.setLen stack.len - 1

    of tkParam:
        # It's bit inelegant to do parsing of pseudo arguments here,
        # but since there are only two types of pseudo arguments,
        # I think it's fine.

        case stack[^2].kind

        of tkPseudoNot:
            # Not the cleanest way to this, but eh
            let notQuery = parseHtmlQuery(prev.value, options)

            if not notQuery.isValidNotQuery(options):
                raise newException(ParseError,
                    ":not argument must be a simple selector. Was: " & repr(notQuery))
            
            let demand = newDemand(tkPseudoNot, notQuery.roots[0])
            demandBuffer.add demand
            stack.setLen stack.len - 2

        of nthKinds:
            let (a, b) = parsePseudoNthArguments(prev.value)
            let demand = newPseudoDemand(stack[^2].kind, a, b)
            demandBuffer.add demand
            stack.setLen stack.len - 2

        else:
            raise newException(ParseError, "Unexpected params")

    of combinatorKinds:
        if stack.len != 1:
            raise newException(ParseError,
                "Invalid parser state. Expected stack length to be 1. Stack: " & repr(stack))

        let combinator = prev.kind.Combinator
        queryRoot.append demandBuffer, combinator
        stack = @[]
        demandBuffer = @[]

    of tkComma:
        if stack.len != 1:
            raise newUnexpectedCharacterException(',')
        queryRoot.append demandBuffer, cmLeaf
        query.roots.add queryRoot
        stack = @[]
        demandBuffer = @[]
        queryRoot = nil

    else: discard

proc isFinishedSimpleSelector(prev: Token, prevPrev: Token): bool =
    # Checks if the last two tokens represents the end of a simple selector. 
    # This is needed to determine if a space is significant or not.
    if prev.isNil:
        return false
    if prev.kind in { tkBracketEnd, tkParam, tkElement } + pseudoNoParamsKinds:
        return true
    if prevPrev.isNil:
        return false
    if prev.kind == tkIdentifier and prevPrev.kind in { tkClass, tkId }:
        return true

iterator tokenize(rawInput: string, options: set[NimqueryOption]): tuple[idx: int, token: Token] =
    let input = rawInput.strip
    var idx = 0
    var prevToken : Token
    var prevPrevtoken : Token
    var skip = false

    while idx < input.len:
        let ch = input[idx]
        var token: Token
        log "char: '" & ch & "'"

        case ch:

        of { '"', '\'' }:
            var buffer = ""
            readStringLiteral(input, idx, buffer)
            token = newToken(tkString, buffer)

        of ' ':
            if idx + 1 < input.len and input[idx + 1] notin combinators and
                    isFinishedSimpleSelector(prevToken, prevPrevtoken):
                token = newToken(tkCombinatorDescendents)
            else:
                skip = true

            idx.inc
    
        of '~':
            if input.safeCharCompare(idx + 1, '='):
                token = newToken(tkAttributeItem)
                idx.inc 2
            else:
                token = newToken(tkCombinatorSiblings)
                idx.inc

        of '+':
            token = newToken(tkCombinatorNextSibling)
            idx.inc

        of '>':
            token = newToken(tkCombinatorChildren)
            idx.inc

        of '[':
            token = newToken(tkBracketStart)
            idx.inc

        of ']':
            token = newToken(tkBracketEnd)
            idx.inc

        of ':':
            var buffer = ""
            buffer.add ch
            idx.inc
            while input[idx] in identifiers and idx < input.len:
                buffer.add input[idx]
                idx.inc

            token = newPseudoToken(buffer)

        of '#':
            idx.inc
            token = newToken(tkId)
        
        of '.':
            idx.inc
            token = newToken(tkClass)

        of '*':
            if input.safeCharCompare(idx + 1, '='):
                token = newToken(tkAttributeSubstring)
                idx.inc 2
            else:
                idx.inc
                # No need to emit since tkUniversal matches everything?
                # token = newToken(tkUniversal)
                skip = true

        of '(':
            var buffer = ""
            readParams(input, idx, buffer)
            token = newToken(tkParam, buffer)

        of '=':
            token = newToken(tkAttributeExact)
            idx.inc

        of '|':
            if input.safeCharCompare(idx + 1, '='):
                token = newToken(tkAttributePipe)
                idx.inc 2

        of '^':
            if input.safeCharCompare(idx + 1, '='):
                token = newToken(tkAttributeStart)
                idx.inc 2

        of '$':
            if input.safeCharCompare(idx + 1, '='):
                token = newToken(tkAttributeEnd)
                idx.inc 2

        of ',':
            token = newToken(tkComma)
            idx.inc

        else: 
            var buffer = ""
            if optUnicodeIdentifiers in options:
                readIdentifier(input, idx, buffer)
            else:
                readIdentifierAscii(input, idx, buffer)

            if buffer.isNilOrEmpty:
                raise newUnexpectedCharacterException($input.runeAt(idx))

            if prevToken.isNil or prevToken.kind in combinatorKinds + { tkComma }:
                token = newToken(tkElement, buffer)
            else:
                token = newToken(tkIdentifier, buffer)

        if not skip:
            if token.isNil:
                raise newUnexpectedCharacterException(ch)

            # TODO: It might be wise to perform some validation here.
            #       e.g tkParam is only valid after tkPseudoNot tkPseudoNth*
            prevPrevtoken = prevToken
            prevToken = token
            yield (idx, token)
        else:
            skip = false

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
            demand.attrValue in node.attr(demand.attrName).split(" ")
    
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

iterator searchDescendants(queryRoot: PartialQuery, position: NodeWithParent): NodeWithParent =
    var queue = newSeq[NodeWithParent]()
    for nodeData in position.node.children:
        queue.add((parent: position.node, index: nodeData.index, elementIndex: nodeData.elementIndex))

    while queue.len > 0:
        let pair = queue.pop()
        if pair.satisfies queryRoot.demands:
            yield pair
        for nodeData in pair.node.children:
            queue.insert((parent: pair.node, index: nodeData.index, elementIndex: nodeData.elementIndex), 0)

iterator searchChildren(queryRoot: PartialQuery, position: NodeWithParent): NodeWithParent =
    for pair in position.node.children:
        if pair.satisfies queryRoot.demands:
            yield pair

iterator searchSiblings(queryRoot: PartialQuery, position: NodeWithParent): NodeWithParent = 
    for pair in position.parent.children(offset = position):
        if pair.satisfies queryRoot.demands:
            yield pair

iterator searchNextSibling(queryRoot: PartialQuery, position: NodeWithParent): NodeWithParent =
    # It's a bit silly to have an iterator which will only yield 0 or 1 element,
    # but it's nice for consistency with how the other combinators are implemented.
    
    for pair in position.parent.children(offset = position):
        if pair.satisfies queryRoot.demands:
            yield pair
        break # by definition, there can only be one next sibling

proc execRecursive(queryRoot: PartialQuery, context: SearchContext, output: var seq[XmlNode]) =
    var position = context.position

    template search(itr: iterator(q: PartialQuery, p: NodeWithParent): NodeWithParent {. inline .}): typed =
        for next in itr(queryRoot, position):
            if queryRoot.nextQueries.len == 0:
                output.add next.node
            else:
                for subquery in queryRoot.nextQueries:
                    let nextContext = context.forward(next, queryRoot.combinator)
                    subquery.execRecursive(nextContext, output)

            if not queryRoot.canFindMultiple(context.combinator, context.options): break
            when context.single:
                if output.len > 0:
                    break

    case context.combinator
    of cmDescendants: search(searchDescendants)
    of cmChildren:    search(searchChildren)
    of cmSiblings:    search(searchSiblings)
    of cmNextSibling: search(searchNextSibling)
    of cmLeaf: discard

proc exec*(query: Query, root: XmlNode, single: static[bool]): seq[XmlNode] =
    result = newSeq[XmlNode]()

    # The <wrapper> element is needed due to how execRecursive is implemented.
    # The "current" position is never matched against, only the childs/siblings (depending on combinator).
    # So to make sure that the original root is tested, we need to set the starting position
    # to an imaginary wrapper element.
    # Since `NodeWIthParent` always require a parent, we also add a wrapper-root element.
    let root = (parent: <>"wrapper-root"(<>wrapper(root)), index: 0, elementIndex: 0).NodeWithParent
    for queryRoot in query.roots:
        let context = initSearchContext(root, cmDescendants, single, query.options)
        queryRoot.execRecursive(context, result)

proc parseHtmlQuery*(queryString: string, options: set[NimqueryOption]): Query =
    let query = Query(roots: @[])
    var queryRoot: PartialQuery = nil
    var stack = newSeq[Token]()
    var demandBuffer = newSeq[Demand]()

    template printDebug(token: string) =
        log "token: " & $token
        log "stack: " & $stack
        log "demands: " & $demandBuffer
        log "* * *"

    for idx, token in tokenize(queryString, options):
        printDebug($token)
        stack.add token
        reduce(stack, demandBuffer, queryRoot, query, options)

    printDebug("n/a")

    if stack.len > 0:
        raise newException(ParseError, "Unexpected end of input")

    queryRoot.append demandBuffer, cmLeaf
    query.roots.add queryRoot
    query.optimize
    query.options = options

    log "\ninput: \n" & queryString
    log "\nquery: \n" & query.debugToString

    return query

proc querySelector*(root: XmlNode, queryString: string, options: set[NimqueryOption]): XmlNode =
    let query = parseHtmlQuery(queryString, options)
    let lst = query.exec(root, single = true)
    if lst.len > 0:
        return lst[0]
    return nil

proc querySelectorAll*(root: XmlNode, queryString: string, options: set[NimqueryOption]) : seq[XmlNode] =
    let query = parseHtmlQuery(queryString, options)
    return query.exec(root, single = false)

# Overloads with default options

proc parseHtmlQuery*(queryString: string): Query =
    parseHtmlQuery(queryString, nimqueryDefaultOptions)

proc querySelector*(root: XmlNode, queryString: string): XmlNode =
    querySelector(root, queryString, nimqueryDefaultOptions)

proc querySelectorAll*(root: XmlNode, queryString: string) : seq[XmlNode] =
    querySelectorAll(root, queryString, nimqueryDefaultOptions)
    