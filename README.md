# Symbolic Differentiation and Simplification in OCaml

This project provides a simple OCaml program to parse, differentiate, and simplify mathematical expressions symbolically. It supports a variety of operations including addition, multiplication, exponentiation, logarithms, sine, cosine, and exponentials.

## Features

- **Parsing**: Convert a string representation of a mathematical expression into an abstract syntax tree (AST).
- **Differentiation**: Symbolically differentiate the parsed expression with respect to a given variable.
- **Simplification**: Simplify the resulting expression to a more readable form.

## Supported Operations

- Constants (`Const`)
- Variables (`Var`)
- Addition (`Sum`)
- Multiplication (`Prod`)
- Exponentiation (`Power`)
- Logarithm (`Log`)
- Sine (`Sin`)
- Cosine (`Cos`)
- Exponential (`Exp`)

## Getting Started

### Prerequisites

Ensure you have [OCaml](https://ocaml.org/) and [Dune](https://dune.build/) installed on your system.

### Installation

Clone the repository:
```bash
git clone https://github.com/your-username/ocaml-symbolic-differentiation.git
cd ocaml-symbolic-differentiation
