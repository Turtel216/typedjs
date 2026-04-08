# TypedJS - A Hindley–Milner Typed JavaScript Dialect

TypedJS is a **source-to-source compiler** for a statically typed JavaScript-like language with **Hindley–Milner (HM) type inference**, implemented in Haskell.

It parses TypedJS source code, performs static type checking, and lowers the typed AST into readable JavaScript output.

---

## Why TypedJS?

JavaScript is expressive, but dynamic typing can move certain bugs from compile-time to runtime. TypedJS explores a different design point:

- Familiar JavaScript-like syntax
- Strong static typing
- Type inference (HM style) for reduced annotation burden
- Readable JavaScript code generation

TypedJS is intended for experimentation, education, and as a foundation for more advanced language tooling.

---

## Current Status

Implemented:

- Parser (Megaparsec-based)
- Typed AST
- HM-style type inference/checking
- Type annotations (optional in many places)
- Source-to-source lowering to JavaScript
- Precedence-aware JavaScript pretty-printer

 In progress / simplified semantics in current implementation:

- Object typing is currently closed/exact in unification-heavy paths
- No row polymorphism yet
- Return-flow analysis is basic unless enhanced with explicit return constraints
- Mutation/immutability semantics are currently minimal and evolving

---

## Compiler Pipeline

TypedJS follows a classic frontend + lowering architecture:

1. **Parse** source text into TypedJS AST
   `Parser`
2. **Typecheck / Infer** using HM constraints + unification
   `Typecheck`
3. **Desuger** typed AST to JavaScript AST (type erasure)
   `Desuger`
4. **Emit** JavaScript AST to readable JS source
   `Emit`

---

## Language Overview

### Variable Binding

```ts
let x = 42;
let y: Int = x + 1;

```

### Functions

```ts
function add(x: Int, y: Int): Int {
  return x + y;
}
```

### Lambdas

```ts
let inc = (n: Int): Int => n + 1;
```

### Conditionals

```ts
let v = if (true) 1 else 0;
```

### Arrays and Objects

```ts
let arr = [1, 2, 3];
let obj = { a: 1, b: 2 };
let x: Int = obj.a;
```

### Higher-Order Functions

```ts
let apply = (f, x) => f(x);
let id = (z) => z;
let result = apply(id, 42);
```

### Prelude

```ts 
let x = 5;
print(5); // maps to Javascript's console.log
```

---

## Type System (Current)

TypedJS currently supports:

- Primitive types: `Int`, `Bool`, `String`
- Type inference for let-bound variables and lambda parameters
- Function types and annotations
- Let-polymorphism (`let id = (x) => x`)
- Unification-based checking of expressions and calls

### Notes

- `+`, `-`, `*`, `/`, `%` are currently numeric (`Int`) operators.
- Equality and logical operators are type-checked.

---

## Project Structure

```text
src/
  Parser.hs      -- Lexer+parser (Megaparsec), TypedJS AST definition
  Typecheck.hs   -- HM inference, constraints, unification
  JSAst.hs       -- JavaScript target AST
  Desuger.hs     -- TypedJS AST -> JS AST lowering (type erasure)
  Emit.hs        -- JavaScript code generation
app/
  Main.hs        -- CLI entry point (parse -> typecheck -> emit JS)
```

---

## Build & Run

### Build

```bash
cabal build
```

### Run

```bash
cabal run
```

---

## Example

### Input (`example.tjs`)

```ts
let id = (x) => x;
let a = id(42);

function add(x: Int, y: Int): Int {
  return x + y;
}

let b = add(a, 1);
```

### Output (`out.js`)

```js
const id = (x) => x;
const a = id(42);
function add(x, y) {
  return x + y;
}
const b = add(a, 1);
```

---

## Error Reporting

Type errors are surfaced from the typechecker, for example:

- Unbound variables
- Unification failures (e.g. `Bool` passed where `Int` is required)
- Duplicate bindings in same scope (if enabled via checker rule)
- Missing object fields (based on current object typing model)

---

## Known Limitations

- No modules/import system yet
- No algebraic data types (ADTs) yet
- No pattern matching
- No row polymorphism for structural object typing
- No optimization pipeline yet (just direct lowering)

---

## Future Features

### 1) Immutability by Default (Planned)

TypedJS will move toward a model where bindings are immutable unless explicitly marked mutable.

Potential direction:

- `let` = immutable (default)
- `mut` (or similar) for mutable bindings
- Assignment allowed only for mutable references
- Stronger guarantees for reasoning and optimization
- Better alignment with functional/HM foundations

### 2) Row Polymorphism for Objects

Enable flexible object typing such as passing `{a:Int, b:Int}` where `{a:Int}` is expected, while preserving static safety.

### 3) Richer Type Features

- Parametric data types and aliases
- Exhaustive pattern matching
- Union/intersection-like encodings (carefully designed)
- Better nullability story

### 4) Improved Diagnostics

- Source-span-aware type errors
- Better mismatch explanations and hints
- Suggested fixes

### 5) Safer/Stronger Frontend Semantics

- Explicit block scoping rules
- Return-flow checking improvements
- Duplicate declaration policy controls

### 6) Developer Tooling

- Test suite + golden tests for parser/typechecker/lowering
- LSP features (hover/type info/diagnostics)
- Formatter and linting tools for TypedJS
