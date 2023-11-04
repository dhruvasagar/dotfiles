# Introduction to Rust Programming Language

---

# Agenda

- What is Rust?
- Why Rust?
- Key Features of Rust
- Getting Started with Rust
- Basic Syntax
- Concurrency
- Ecosystem and Community
- Resources and Next Steps

---

# What is Rust?

- Rust is a systems programming language.
- Developed by Mozilla, first released in 2010.
- Designed for performance, reliability, and safety.

---

# Why Rust?

- Rust offers a unique combination of features.
- Ideal for systems programming, embedded systems, and more.
- Key advantages: Memory safety, zero-cost abstractions, and fearless concurrency.

---

# Memory Safety

A memory-safe programming language will prevent a developer from writing code
introducing memory-related bugs.

Rust is designed to make it **impossible** to access unintended memory.
Therefore, the language has several features included in it, such as:

## Mutability

by default, variables are not mutable and must be declared so.
Furthermore, even mutable variables can only have a single reference at
a time.

## Ownership

Set of rules that govern how a Rust program manages memory.
All programs have to manage the way they use a computer’s memory while
running. Some languages have garbage collection that regularly looks for
no-longer-used memory as the program runs; in other languages, the programmer
must explicitly allocate and free the memory. Rust uses a third approach:
memory is managed through a system of ownership with a set of rules that the
compiler checks. If any of the rules are violated, the program won’t compile.
None of the features of ownership will slow down your program while it’s
running.

## Borrowing

eliminates dangling pointers and creates a mechanism like
a read-write lock.

## Bounds checking

verifying that code accesses are within the boundaries of
an array or assigned memory location.

## Lifetimes

Ensures data isn't accessed after it's invalid.

---

# Zero Cost Abstractions

Zero-cost abstractions is a concept where you can use higher-level languages
without incurring additional runtime cost. Essentially, this means that Rust
allows you to write code at a high level of abstraction without sacrificing
performance.

Rust achieves zero-cost abstractions by compiling all code down to machine
instructions, with no interpreter or garbage collector. This way, Rust ensures
that any abstractions do not impose any additional runtime costs.

```rust
fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    let sum_of_squares: i32 = numbers.iter().map(|&x| x * x).sum();
    println!("{}", sum_of_squares);
}
```

In this example, we use the map function, a higher-level abstraction, to
square each number in the vector. The sum function then adds all the squares
together. This code, despite its high level of abstraction, will be compiled
down to machine instructions as efficient as if you had written the loop
yourself.

---

# Fearless Concurrency

Handling concurrent programming safely and efficiently is another of Rust’s
major goals. Concurrent programming, where different parts of a program
execute independently, and parallel programming, where different parts of
a program execute at the same time, are becoming increasingly important as
more computers take advantage of their multiple processors. Historically,
programming in these contexts has been difficult and error prone: Rust hopes
to change that.

Initially, the Rust team thought that ensuring memory safety and preventing
concurrency problems were two separate challenges to be solved with different
methods. Over time, the team discovered that the ownership and type systems
are a powerful set of tools to help manage memory safety and concurrency
problems! By leveraging ownership and type checking, many concurrency errors
are compile-time errors in Rust rather than runtime errors. Therefore, rather
than making you spend lots of time trying to reproduce the exact circumstances
under which a runtime concurrency bug occurs, incorrect code will refuse to
compile and present an error explaining the problem. As a result, you can fix
your code while you’re working on it rather than potentially after it has been
shipped to production. We’ve nicknamed this aspect of Rust fearless
concurrency. Fearless concurrency allows you to write code that is free of
subtle bugs and is easy to refactor without introducing new bugs.

Source: [Fearless Concurrency](https://doc.rust-lang.org/book/ch16-00-concurrency.html)

---

# Getting Started with Rust

- Install Rust by following instructions at [Rust Lang](https://rust-lang.org)
- Get familiar with **cargo**
  - `cargo build`
  - `cargo build --release`
  - `cargo run`
  - `cargo add`
- `rust-analyzer` is a modular compiler frontend for the Rust language. It is
  a part of a larger rls-2.0 effort to create excellent IDE support for Rust.

---

# Basic Syntax

## Variables & Mutability

```rust
let x = 5;
let y: u8 = 5;
let mut z = 10;
z += 1;
```

## Data Types

### Scalar Types

- Integers
- Floating point numbers
- Booleans
- characters

### Compound Types

- Tuples
- Arrays (Slices, Vectors)

## Functions

```rust
fn main() {
    println!("Hello, world!");

    another_function();
}

fn another_function() {
    println!("Another function.");
}
```

```sh
$ cargo run
   Compiling functions v0.1.0 (file:///projects/functions)
    Finished dev [unoptimized + debuginfo] target(s) in 0.28s
     Running `target/debug/functions`
Hello, world!
Another function.
```

## Structs

A struct, or structure, is a custom data type that lets you package together
and name multiple related values that make up a meaningful group. If you’re
familiar with an object-oriented language, a struct is like an object’s data
attributes. In this chapter, we’ll compare and contrast tuples with structs to
build on what you already know and demonstrate when structs are a better way
to group data.

```rust
#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    fn area(&self) -> u32 {
        self.width * self.height
    }
}

fn main() {
    let rect1 = Rectangle {
        width: 30,
        height: 50,
    };

    println!(
        "The area of the rectangle is {} square pixels.",
        rect1.area()
    );
}
```

## Enums & Pattern Matching

Rust has an extremely powerful control flow construct called match that allows
you to compare a value against a series of patterns and then execute code
based on which pattern matches.

```rust
enum Coin {
    Penny,
    Nickel,
    Dime,
    Quarter,
}

fn value_in_cents(coin: Coin) -> u8 {
    match coin {
        Coin::Penny => 1,
        Coin::Nickel => 5,
        Coin::Dime => 10,
        Coin::Quarter => 25,
    }
}
```

---

# Concurrency

Handling concurrent programming safely and efficiently is another of Rust’s
major goals. Concurrent programming, where different parts of a program
execute independently, and parallel programming, where different parts of
a program execute at the same time

## Threads

## Message Passing

## Shared-State Concurrency

## Extensible Concurrency with Sync & Send Traits

---

# Ecosystem and Community

- The Rust Standard Library: A rich collection of pre-built modules.
- Package Management with Cargo: Simplifies dependency management.
- A growing community with numerous open-source projects.

---

# Resources and Next Steps

- Official Rust Documentation [rust-lang.org](https://rust-lang.org)
- [The Rust Programming Language Book](https://doc.rust-lang.org/book)
- Explore real-world Rust projects and contribute.

---

# Questions
