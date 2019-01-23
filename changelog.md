Version 1.2.0 (2019-01-23)
=============
- Improved error handling:
    * `ParseError` now inherits from `ValueError` instead of `Exception`
    * `ParseError` is now the only catchable exception that will be raised by exported Nimquery procs (this was already the documented behavior, but wasn't true for some edge cases).
    * Exception messages now always contain the full query that caused the exception.
- CSS pseudo selectors are now case insensitive.
- Now requires Nim 0.19.2 or later.
- Much more strict and correct parsing for the `:nth-*` family of pseudo selectors.