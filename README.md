# Syn Helpers

[![](https://img.shields.io/crates/v/syn-helpers)](https://crates.io/crates/syn-helpers)


[Documentation](https://docs.rs/syn-helpers/latest/syn_helpers/)

Framework for building derive proc macros over structures (`struct` and `enum`).

Handles:

- Getting the expressions referencing fields and building patterns for enums
- Using the same logic for deriving over a enum or struct
- Error handling and generating `compile_error` output
- Generics on trait and structure (including conflict rectification)

Used in / example usage:
- [derive-partial-eq-extras](https://github.com/kaleidawave/derive-partial-eq-extras)
- [derive-debug-extras](https://github.com/kaleidawave/derive-debug-extras)

*Design work in progress*