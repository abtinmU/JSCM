# Julia-SCM

A lightweight and extensible Scheme interpreter implemented in Julia. This interpreter supports core Scheme functionality, including arithmetic, conditionals, lambda expressions, macros, and advanced features like continuations. It also provides a simple REPL for interactive programming.

---

## Features

- **Core Scheme Constructs**: Supports `define`, `if`, `lambda`, `begin`, `let`, and more.
- **Arithmetic and Logic**: Implements basic operations like `+`, `-`, `*`, `/`, and comparisons (`<`, `>`, `=`).
- **Lists and Data Structures**: Includes `cons`, `car`, `cdr`, and other list utilities.
- **Macros**: Allows defining and using macros with `define-macro`.
- **Continuations**: Advanced control flow using `call/cc` (call-with-current-continuation).
- **REPL**: Interactive environment for writing and evaluating Scheme code.

---

## Project Structure

```
JSCM
├── Julia-Scheme/
│   ├── Env.jl       # Environment management for variables and scopes
│   ├── Eval.jl      # Evaluation logic for Scheme expressions
│   ├── Parser.jl    # Parsing Scheme code into internal representations
│   ├── Prep.jl      # Preprocessing and macro expansion
│   ├── Repl.jl      # REPL implementation for user interaction
│   ├── Run.jl       # Entry point to start the interpreter
│   └── Utils.jl     # Utility functions and symbol management
└── Tests.ipynb # Tests for various interpreter components
```

