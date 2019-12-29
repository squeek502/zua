Zua
===

An attempt at a [Lua](https://lua.org) 5.1 implementation in [Zig](https://ziglang.org).

Goals, in order of priority:
1. Learn more about Lua internals
2. Learn more about Zig
3. Anything else

## Status

- [ ] Lexer (llex.c/.h) -> [lex.zig](src/lex.zig)
  + [x] Keywords
  + [x] Identifiers
  + [x] `..`, `...`
  + [x] `==`, `>=`, `<=`, `~=`
  + [x] String literals (single/double quoted and multi-line (`[[`))
  + [x] Comments (`--` and `--[[`)
  + [x] Numbers
  + [x] Improve tests, perhaps use fuzz testing
  + [ ] Cleanup implementation
- [ ] Parser (lparser.c/.h)
- [ ] ...

## Why Lua 5.1?

It's what I'm most familiar with, and I'm also assuming that 5.1 is simpler internally than more recent Lua versions.

## Building / running

- `zig build` to build zua.exe
- `zig build test` to build & run all tests
- `zig build run` to build & run zua.exe (does nothing right now)
