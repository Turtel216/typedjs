# TypedJS - A Typed JavaScript Dialect

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
- Polymorphic functions and type aliases
- Immutability by default (`let` = immutable, `let mut` = mutable)
- Mutation checks enforced at type-check time

 In progress / simplified semantics in current implementation:

- Algebraic Data Types(ADTs) and pattern matching
- Object typing is currently closed/exact in unification-heavy paths
- Return-flow analysis is basic unless enhanced with explicit return constraints

---

## Language Overview

### Variable Binding

Bindings are **immutable by default**. Use `let mut` to declare a mutable binding:

```ts
let x = 42;        // immutable — compiles to `const`
let mut y = 0;     // mutable  — compiles to `let`
y = y + 1;         // OK

// x = 10;         // TYPE ERROR: cannot assign to immutable binding `x`
```

Optional type annotations:

```ts
let a: Int = 42;
let mut b: Int = 0;
```

### Functions

```ts
function add(x: Int, y: Int): Int {
  return x + y;
}
```

### Polymorphic functions 

```ts
function id(a): a {
    return a;
  }

let a = id(2);
let b = id(true);
```

### Lambdas

```ts
let inc = (n: Int): Int => n + 1; // The type annotations are optional
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

### User defined Types and Parametric type aliases

```ts 
type Pair<A, B> = { first: A, second: B };

let p: Pair<Int, String> = { first: 42, second: "hello" };
print(p.first);
```

---

## Type System (Current)

TypedJS currently supports:

- Primitive types: `Int`, `Bool`, `String`
- Type inference for let-bound variables and lambda parameters
- Function types and annotations
- Let-polymorphism (`let id = (x) => x`)
- Unification-based checking of expressions and calls
- Polymorphic functions and type aliases

### Notes

- `+`, `-`, `*`, `/`, `%` are currently numeric (`Int`) operators.
- Equality and logical operators are type-checked.

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
let a = id(1);

function add(x: Int, y: Int): Int {
  return x + y;
}

let mut b = add(a, 1);

if (b > 5) {
  print(b);
} else {
  b = 100;
  print(b);
}
```

### Output (`out.js`)

```js
const id = (x) => x;
const a = id(1);
function add(x, y) {
  return x + y;
}
let b = add(a, 1);
if (b > 5) {
  console.log(b);
} else {
  b = 100;
  console.log(b);
}
```

---

## Error Reporting

Type errors are surfaced from the typechecker, for example:

- Unbound variables
- Unification failures (e.g. `Bool` passed where `Int` is required)
- Immutable assignment (e.g. `x = 10` where `x` was declared with `let`)
- Duplicate bindings in same scope
- Missing object fields (based on current object typing model)

---

## Known Limitations

- No modules/import system yet
- No algebraic data types (ADTs) yet
- No pattern matching
- No optimization pipeline yet (just direct lowering)

---

## Future Features

### 1) Row Polymorphism for Objects

Enable flexible object typing such as passing `{a:Int, b:Int}` where `{a:Int}` is expected, while preserving static safety.

### 2) Richer Type Features

- Exhaustive pattern matching
- Union/intersection-like encodings (carefully designed)
- Better nullability

### 3) Improved Diagnostics

- Source-span-aware type errors
- Better mismatch explanations and hints
- Suggested fixes

### 4) Safer/Stronger Frontend Semantics

- Explicit block scoping rules
- Return-flow checking improvements
- Duplicate declaration policy controls

### 5) Developer Tooling

- Test suite + golden tests for parser/typechecker/lowering
- LSP features (hover/type info/diagnostics)
- Formatter and linting tools for TypedJS
