# Spec: https://www.w3.org/TR/css3-selectors/

# TODO:
#  - Some selectors are unique in the context of siblings. I can use this for optimizations.
#  - There are some very specific optmization which might be worth it.
#    E.g :only-child can disqualify all siblings at once.
import xmltree
import strutils
import strtabs
import unicode

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
    DemandRef = ref object
        case kind: Tokenkind
        of attributeKinds:
            attrName, attrValue: string
        of nthKinds:
            a, b: int
        of tkPseudoNot:
            notSelector: Demand
        of tkElement:
            element: string
        else: discard

    Demand = DemandRef not nil

    NodeWithParent = tuple
        parent: XmlNode
        index, elementIndex: int

    Combinator = enum
        cmChildren, cmDescendants, cmNextSibling, cmSiblings
        cmLeaf # Special case for the last query
    
    # Because of the comma operator, a query can consist of multiple actual queries.
    Query* = ref object
        roots: seq[QueryRoot]

    QueryRoot = ref object
        # Will never contain more than one element while parsing.
        # When optmizing comma queries, partialy identical queries are merged
        # and stored as a seq.
        # F.ex the query "div p, div a" will generate two queries,
        # but since the left-most subqueries ("div") are identical, they can be merged
        # so that the first subquery is only executed once.
        nextQuery: seq[QueryRoot]
        demands: seq[Demand]
        combinator: Combinator

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

proc indent(str, space: string): string =
    result = str.replace("\n", "\n" & space)

proc safeCharCompare(str: string, idx: int, cs: set[char]): bool {. inline .} =
    if idx > high(str): return false
    if idx < low(str): return false
    return str[idx] in cs

proc safeCharCompare(str: string, idx: int, c: char): bool {. inline .} =
    return str.safeCharCompare(idx, { c })

proc child(pair: NodeWithParent): XmlNode =
    return pair.parent[pair.index]

proc newUnexpectedCharacterException(c: char): ref ParseError =
    return newException(ParseError, "Unexpected character: " & c)

proc newDemand(kind: static[TokenKind]): Demand =
    return Demand(kind: kind)

proc newDemand(kind: static[TokenKind], notSelector: Demand): Demand =
    return Demand(kind: kind, notSelector: notSelector)

proc newDemand(kind: static[TokenKind], attrName, attrValue: string): Demand =
    return Demand(kind: kind, attrName: attrName, attrValue: attrValue)

proc newDemand(kind: static[TokenKind], element: string): Demand =
    return Demand(kind: kind, element: element)

proc newDemand(kind: static[TokenKind], a, b: int): Demand =
    return Demand(kind: kind, a: a, b: b)

proc `$`(demand: Demand): string =
    result = "[" & $demand.kind
    case demand.kind:
    of attributeKinds:
        result.add " " & demand.attrName & "=" & demand.attrValue
    of tkPseudoNot:
        result.add " not: " & $demand.notSelector
    of nthKinds:
        result.add " a = " & $demand.a & " b = " & $demand.b
    of tkElement:
        result.add " " & demand.element
    else: discard
    result.add "]"

proc `==`(d1, d2: Demand): bool =
    if d1.kind != d2.kind: return false
    case d1.kind
    of attributeKinds:
        return d1.attrName == d2.attrName and d1.attrValue == d2.attrValue
    of nthKinds:
        return d1.a == d2.b
    of tkPseudoNot:
        return d1.notSelector == d2.notSelector
    of tkElement:
        return d1.element == d2.element
    else:
        raise newException(Exception, "Invalid demand kind: " & $d1.kind)

iterator elements(node: XmlNode, offset: NodeWithParent = (nil, -1, -1)): NodeWithParent =
    var idx = offset.index + 1
    var elIdx = offset.elementIndex + 1
    while idx < node.len:
        let el = node[idx]
        if el.kind == xnElement:
            yield (parent: node, index: idx, elementIndex: elIdx).NodeWithParent
            elIdx.inc
        idx.inc

proc shallowToString(node: XmlNode): string =
    # Print a single element instead of the entire tree
    if node.isNil:
        return "nil"

    var attrs = newSeq[string]()

    if not node.attrs.isNil:
        for attrName, attrValue in node.attrs:
            attrs.add attrName & "=\"" & attrValue & "\""

    result = "<" & node.tag
    if attrs.len > 0:
        result.add " " & attrs.join(" ")
    result.add " />"

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

proc newQueryRoot(demands: seq[Demand], combinator: Combinator): QueryRoot =
    return QueryRoot(demands: demands, nextQuery: nil, combinator: combinator)

proc `$`(q: QueryRoot): string =
    result = q.demands.join(", ") & " " & $q.combinator

    if q.nextQuery.len > 0:
        result.add "\n\t" & q.nextQuery.join("\n").indent "\t"

proc hasIdenticalDemands(q1, q2: QueryRoot): bool =
    if q1.demands.len != q2.demands.len:
        return false

    for d1 in q1.demands:
        var found = false
        for d2 in q2.demands:
            if d1 == d2:
                found = true

        if not found:
            return false
    return true

proc append(q: var QueryRoot, demands: seq[Demand], combinator: Combinator) =
    if q.isNil:
        q = newQueryRoot(demands, combinator)
    else:
        var itr = q
        while itr.nextQuery.len > 0:
            itr = itr.nextQuery[0]
        itr.nextQuery = @[ newQueryRoot(demands, combinator) ]

proc canFindMultiple(q: QueryRoot, comb: Combinator): bool =
    # Returns true if the current queries demands can be satiesfied by multiple elements.
    # This is used to check if the search should stop after the first element has been found.
    for demand in q.demands:
        if demand.kind in attributeKinds and demand.attrName == "id":
            return false
        if comb in { cmChildren, cmSiblings } and demand.kind in
                { tkPseudoFirstOfType, tkPseudoLastOfType,
                    tkPseudoFirstChild, tkPseudoLastChild, tkPseudoOnlyOfType }:
            return false

    return true

proc `$`(q: Query): string =
    result = q.roots.join "\n"

proc optimize(query: Query) =
    # This implementation is not perfect, but it might be good enough.
    # It prioritizes merging roots to the start of the root list, 
    # which is arbitrary (but predictable).
    # The best optimization is dependent on the structure of HTML
    # document being queried anyway so I don't think it matters.

    if query.roots.len == 1: return

    var mergeToIdx = 0
    var mergeFromIdx = 1

    while mergeToIdx < high(query.roots):
        if hasIdenticalDemands(query.roots[mergeFromIdx], query.roots[mergeToIdx]):
            
            var qFrom = query.roots[mergeFromIdx]
            var qTo = query.roots[mergeToIdx]

            while hasIdenticalDemands(qFrom.nextQuery[0], qTo.nextQuery[0]):
                qFrom = qFrom.nextQuery[0]
                qTo = qTo.nextQuery[0]

            qTo.nextQuery.add qFrom.nextQuery
            query.roots.delete mergeFromIdx

        else:
            mergeFromIdx.inc

        if mergeFromIdx > high(query.roots):
            mergeToIdx.inc
            mergeFromIdx = mergeToIdx + 1

proc isSimpleSelector(q: Query): bool =
    return q.roots.len == 1 and q.roots[0].demands.len == 1 and q.roots[0].nextQuery.len == 0

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
    if input[idx] == '-' and input.safeCharCompare(idx + 1, { '-' } + Digits):
        raise newUnexpectedCharacterException(input[idx + 1])

    while input[idx] in identifiers and idx < input.len:
        if input[idx] == '\\':
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
        else:
            buffer.add input[idx]
            idx.inc

proc parsePseudoNthArguments(raw: string): tuple[a: int, b: int] =
    let input = raw.strip
    if input == "odd":
        return (2, 1)
    elif input == "even":
        return (2, 0)
    else:
        var done = false
        var idx = 0
        var a = ""
        var b = ""

        # NOTE: Spacing between first sign and `a` is not allowed.
        while idx < input.len:
            var buffer = ""
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
                    allowSpace = true
                else:
                    b = buffer
                    done = true
            of 'n':
                if not a.isNilOrEmpty:
                    raise newUnexpectedCharacterException(input[idx])
                a = "1"
                idx.inc
                allowSpace = true
            of ' ':
                if allowSpace:
                    idx.inc
                else:
                    raise newUnexpectedCharacterException(input[idx])
            else:
                raise newUnexpectedCharacterException(input[idx])

        if not input[idx .. ^1].strip.isNilOrEmpty:
            raise newUnexpectedCharacterException(input[idx .. ^1].strip[0])
        
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

proc newPseudoDemand(kind: TokenKind): Demand =
    case kind
    of tkPseudoFirstOfType:
        return newDemand(tkPseudoFirstOfType)
    of tkPseudoLastOfType:
        return newDemand(tkPseudoLastOfType)
    of tkPseudoOnlyChild:
        return newDemand(tkPseudoOnlyChild)
    of tkPseudoOnlyOfType:
        return newDemand(tkPseudoOnlyOfType)
    of tkPseudoEmpty:
        return newDemand(tkPseudoEmpty)
    of tkPseudoFirstChild:
        return newDemand(tkPseudoFirstChild)
    of tkPseudoLastChild:
        return newDemand(tkPseudoLastChild)
    else:
        raise newException(ParseError, "Unknown pseudo: " & $kind)

proc newAttributeDemand(kind: TokenKind, attrName, attrValue: string): Demand =
    case kind
    of tkAttributeExists:
        return newDemand(tkAttributeExists, attrName, attrValue)
    of tkAttributeEnd:
        return newDemand(tkAttributeEnd, attrName, attrValue)
    of tkAttributeExact:
        return newDemand(tkAttributeExact, attrName, attrValue)
    of tkAttributeItem:
        return newDemand(tkAttributeItem, attrName, attrValue)
    of tkAttributePipe:
        return newDemand(tkAttributePipe, attrName, attrValue)
    of tkAttributeStart:
        return newDemand(tkAttributeStart, attrName, attrValue)
    of tkAttributeSubstring:
        return newDemand(tkAttributeSubstring, attrName, attrValue)
    else:
        raise newException(ParseError, "Unknown attribute kind: " & $kind)

proc newPseudoDemand(kind: TokenKind, a, b: int): Demand =
    case kind
    of tkPseudoNthChild:
        return newDemand(tkPseudoNthChild, a, b)
    of tkPseudoNthLastChild:
        return newDemand(tkPseudoNthLastChild, a, b)
    of tkPseudoNthOfType:
        return newDemand(tkPseudoNthOfType, a, b)
    of tkPseudoNthLastOfType:
        return newDemand(tkPseudoNthLastOfType, a, b)
    else:
        raise newException(ParseError, "Unknown pseudo: " & $kind)

proc getCombinator(kind: TokenKind): Combinator =
    case kind:
    of tkCombinatorChildren:
        return cmChildren
    of tkCombinatorDescendents:
        return cmDescendants
    of tkCombinatorNextSibling:
        return cmNextSibling
    of tkCombinatorSiblings:
        return cmSiblings
    else:
        raise newException(ParseError, "Unknown combinator: " & $kind)

proc parseHtmlQuery*(queryString: string): Query # Forward declare for usage in `reduce`
proc reduce(stack: var seq[Token], demandStack: var seq[Demand], queryRoot: var QueryRoot, query: Query) =
    if stack.len == 0:
        return

    let peek = high(stack)
    let prev = stack[peek]

    case prev.kind

    of tkIdentifier:
        if stack[^2].kind == tkClass:
            let demand = newDemand(tkAttributeItem, "class", prev.value)
            demandStack.add demand
            stack.setLen stack.len - 2

        elif stack[^2].kind == tkId:
            let demand = newDemand(tkAttributeExact, "id", prev.value)
            demandStack.add demand
            stack.setLen stack.len - 2

    of tkElement:
        let demand = newDemand(tkElement, prev.value)
        demandStack.add demand
        stack.setLen stack.len - 1

    of tkBracketEnd:
        if stack[^3].kind in attributeKinds - { tkAttributeExists }:
            let demand = newAttributeDemand(stack[^3].kind, stack[^4].value, stack[^2].value)
            demandStack.add demand
            stack.setLen stack.len - 5

        else:
            let demand = newDemand(tkAttributeExists,stack[^2].value, "")
            demandStack.add demand
            stack.setLen stack.len - 3

    of pseudoNoParamsKinds:
        let demand = newPseudoDemand(prev.kind)
        demandStack.add demand
        stack.setLen stack.len - 1

    of tkParam:
        # It's bit inelegant to do parsing of pseudo arguments here,
        # but since there are only two types of pseudo arguments,
        # I think it's fine.

        case stack[^2].kind

        of tkPseudoNot:
            # Not the cleanest way to this, but eh
            let notQuery = parseHtmlQuery(prev.value)

            if not notQuery.isSimpleSelector:
                raise newException(ParseError,
                    ":not argument must be a simple selector. Was: " & repr(notQuery))
            
            var notQueryRoot = notQuery.roots[0]

            # Safe because we know it's a simple selector
            let demand = newDemand(tkPseudoNot, notQueryRoot.demands[0])
            demandStack.add demand
            stack.setLen stack.len - 2

        of nthKinds:
            let (a, b) = parsePseudoNthArguments(prev.value)
            let demand = newPseudoDemand(stack[^2].kind, a, b)
            demandStack.add demand
            stack.setLen stack.len - 2

        else:
            raise newException(ParseError, "Unexpected params")

    of combinatorKinds:
        if stack.len != 1:
            raise newException(ParseError,
                "Invalid parser state. Expected stack length to be 1. Stack: " & repr(stack))

        let combinator = getCombinator(prev.kind)
        queryRoot.append demandStack, combinator
        stack = @[]
        demandStack = @[]

    of tkComma:
        if stack.len != 1:
            raise newUnexpectedCharacterException(',')
        queryRoot.append demandStack, cmLeaf
        query.roots.add queryRoot
        stack = @[]
        demandStack = @[]
        queryRoot = nil

    else: discard

proc isFinishedSimpleSelector(prev: Token, prevPrev: Token): bool =
    if prev.isNil:
        return false
    if prev.kind in { tkBracketEnd, tkParam, tkElement } + pseudoNoParamsKinds:
        return true
    if prevPrev.isNil:
        return false
    if prev.kind == tkIdentifier and prevPrev.kind in { tkClass, tkId }:
        return true

iterator tokenize(rawInput: string): tuple[idx: int, token: Token] =
    let input = rawInput.strip
    let max = high(input)
    var idx = 0
    var prevToken : Token
    var prevPrevtoken : Token
    var skip = false

    while idx < input.len:
        let ch = input[idx]
        var token: Token
        when DEBUG:
            echo "char: '" & ch & "'"

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
                idx.inc
                token = newToken(tkCombinatorSiblings)

        of '+':
            idx.inc
            token = newToken(tkCombinatorNextSibling)

        of '>':
            idx.inc
            token = newToken(tkCombinatorChildren)

        of '[':
            idx.inc
            token = newToken(tkBracketStart)

        of ']':
            idx.inc
            token = newToken(tkBracketEnd)

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
            idx.inc
            while input[idx] != ')':
                buffer.add input[idx]
                idx.inc
                if idx > max:
                    raise newException(ParseError, "Non-terminated pseudo argument list")

            idx.inc
            token = newToken(tkParam, buffer)

        of beginingIdentifiers:
            var buffer = ""
            readIdentifier(input, idx, buffer)

            if prevToken.isNil or prevToken.kind in combinatorKinds + { tkComma }:
                token = newToken(tkElement, buffer)
            else:
                token = newToken(tkIdentifier, buffer)

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

        else: discard

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
    return (nSiblings - (b - 1)) mod a == 0

proc satisfies(pair: NodeWithParent, demand: Demand): bool =
    let node = pair.child

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
        for siblingPair in pair.parent.elements:
            if siblingPair.child != node:
                return false
        return true

    of tkPseudoOnlyOfType:
        for siblingPair in pair.parent.elements:
            if siblingPair.child != node and
                    siblingPair.child.tag == node.tag:
                return false
        return true

    of tkPseudoFirstChild:
        return pair.elementIndex == 0

    of tkPseudoLastChild:
        for siblingPair in pair.parent.elements(offset = pair):
            return false
        return true
    
    of tkPseudoFirstOfType:
        for siblingPair in pair.parent.elements:
            if siblingPair.child.tag == node.tag:
                return siblingPair.child == node

    of tkPseudoLastOfType:
        for siblingPair in pair.parent.elements(offset = pair):
            if siblingPair.child.tag == node.tag:
                return false
        return true

    of tkPseudoNot:
        return not pair.satisfies(demand.notSelector)

    of tkPseudoNthChild:
        return validateNth(demand.a, demand.b, pair.elementIndex)

    of tkPseudoNthLastChild:
        var nSiblingsAfter = 0
        for siblingPair in pair.parent.elements(offset = pair):
            nSiblingsAfter.inc
        return validateNth(demand.a, demand.b, nSiblingsAfter)

    of tkPseudoNthOfType:
        var nSiblingsOfTypeBefore = 0
        for siblingPair in pair.parent.elements:
            if siblingPair.child == node:
                break
            elif siblingPair.child.tag == node.tag:
                nSiblingsOfTypeBefore.inc

        return validateNth(demand.a, demand.b, nSiblingsOfTypeBefore)

    of tkPseudoNthLastOfType:
        var nSiblingsOfTypeAfter = 0
        for siblingPair in pair.parent.elements(offset = pair):
            if siblingPair.child.tag == node.tag:
                nSiblingsOfTypeAfter.inc

            return validateNth(demand.a, demand.b, nSiblingsOfTypeAfter)
    else:
        raise newException(ParseError, "Invalid demand: " & $demand)

proc satisfies(pair: NodeWithParent, demands: seq[Demand]): bool =
    for demand in demands:
        if not pair.satisfies(demand):
            return false

    return true

iterator searchDescendants(queryRoot: QueryRoot, position: NodeWithParent): NodeWithParent =
    var queue = newSeq[NodeWithParent]()
    for nodeData in position.child.elements:
        queue.add((parent: position.child, index: nodeData.index, elementIndex: nodeData.elementIndex))

    while queue.len > 0:
        let pair = queue.pop()
        if pair.satisfies queryRoot.demands:
            yield pair
        for nodeData in pair.child.elements:
            queue.insert((parent: pair.child, index: nodeData.index, elementIndex: nodeData.elementIndex), 0)

iterator searchChildren(queryRoot: QueryRoot, position: NodeWithParent): NodeWithParent =
    for pair in position.child.elements:
        if pair.satisfies queryRoot.demands:
            yield pair

iterator searchSiblings(queryRoot: QueryRoot, position: NodeWithParent): NodeWithParent = 
    for pair in position.parent.elements:
        if pair.child != position.child and pair.satisfies queryRoot.demands:
            yield pair

iterator searchNextSibling(queryRoot: QueryRoot, position: NodeWithParent): NodeWithParent =
    # It's a bit silly to have an iterator which will only yield 0 or 1 element,
    # but it's nice for consistency with how the other combinators are implemented.
    
    for pair in position.parent.elements(offset = position):
        if pair.satisfies queryRoot.demands:
            yield pair
        break # by definition, there can only be one next sibling

proc execRecursive(queryRoot: QueryRoot, root: NodeWithParent, combinator: Combinator,
        single: static[bool], output: var seq[XmlNode]) =
    
    var position = root

    template search(itr: iterator(q: QueryRoot, p: NodeWithParent): NodeWithParent {. inline .}): typed =
        for next in itr(queryRoot, position):
            if queryRoot.nextQuery.len == 0:
                output.add next.child
            else:
                for subquery in queryRoot.nextQuery:
                    subquery.execRecursive(next, queryRoot.combinator, single, output)

            if not queryRoot.canFindMultiple(combinator): break
            when single:
                if output.len > 0:
                    break

    case combinator
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
        queryRoot.execRecursive(root, cmDescendants, single, result)

proc parseHtmlQuery*(queryString: string): Query =
    let query = Query(roots: @[])
    var queryRoot: QueryRoot = nil
    var stack = newSeq[Token]()
    var demandStack = newSeq[Demand]()

    template printDebug(token: string) =
        when DEBUG:
            echo "token: " & $token
            echo "stack: " & $stack
            echo "demands: " & $demandStack
            echo "* * *"

    for idx, token in tokenize(queryString):
        printDebug($token)
        stack.add token
        reduce(stack, demandStack, queryRoot, query)

    when DEBUG:
        printDebug("n/a")

    if stack.len > 0:
        raise newException(ParseError, "Unexpected end of input")

    queryRoot.append demandStack, cmLeaf
    query.roots.add queryRoot
    query.optimize

    when DEBUG:
        echo "\ninput: \n" & queryString
        echo "\nquery: \n" & $query

    return query

proc querySelector*(root: XmlNode, queryString: string): XmlNode =
    let query = parseHtmlQuery(queryString)
    let lst = query.exec(root, single = true)
    if lst.len > 0:
        return lst[0]
    return nil

proc querySelectorAll*(root: XmlNode, queryString: string) : seq[XmlNode] =
    let query = parseHtmlQuery(queryString)
    return query.exec(root, single = false)