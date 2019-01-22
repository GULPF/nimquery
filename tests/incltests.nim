import std / unittest
include ../nimquery.nim

# NOTE: CSS selectprs are case insensitive!

test "parsePseudoNthArguments":
    check parsePseudoNthArguments("odd") == (2, 1)
    check parsePseudoNthArguments("ODD") == (2, 1)
    check parsePseudoNthArguments("  odd  ") == (2, 1)
    check parsePseudoNthArguments("even") == (2, 0)
    check parsePseudoNthArguments("1n + 1") == (1, 1)
    check parsePseudoNthArguments("2n+0") == (2, 0)
    check parsePseudoNthArguments(" 2n  +  0") == (2, 0)
    check parsePseudoNthArguments("2n") == (2, 0)
    check parsePseudoNthArguments("n + 0") == (1, 0)
    check parsePseudoNthArguments("-n + 1") == (-1, 1)
    check parsePseudoNthArguments("1") == (0, 1)
    check parsePseudoNthArguments("0n") == (0, 0)

    expect(ParseError):
        discard parsePseudoNthArguments("1 + 1")
    expect(ParseError):
        discard parsePseudoNthArguments("1 +")