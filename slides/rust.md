# Introduction to **Rust** Programming Language

Rust has topped the chart as `The most admired and desired programming
language` in Stack Overflow’s annual developer survey. And with more than
**80%** of developers reporting that they’d like to use the language again
next year, you have to wonder how a language created less than 20 years ago
has stolen the hearts of developers around the world.

In this presentation, we’ll look at the history of **Rust**, what it’s
commonly used for, why developers love it so much, and some resources to help
you start learning one of the top fastest growing languages on GitHub.

---

# Agenda

- What is **Rust**?
- Why **Rust**?
- Key Features of **Rust**
- Getting Started with **Rust**
- Basic Syntax
- Ecosystem and Community
- Resources and Next Steps

---

# What is **Rust**?

- Developed by Mozilla, first released in 2010 and in May 2015, **Rust** 1.0
  was released
- Designed for performance, reliability, productivity and safety.
- **Rust** is a statically typed systems programming language.
  - Variables & Expression types are determined and checked at compile time,
    which helps enhance memory safety and error detection, resulting in more
    reliable builds.

---

# Why **Rust**?

- **Rust** offers a unique combination of features which help solve some of
  developers’ most frustrating memory management problems commonly associated
  with C and C++
- Ideal for systems programming, embedded systems, and much more.
- Key advantages:
  - Memory Safety
  - Zero-Cost Abstractions
  - Fearless Concurrency

---

# Memory Safety

Memory safety issues are one of the leading causes of security vulnerabilities
in computing systems, including embedded systems. In programming languages
like C/C++, developers are expected to manage memory safely. However, modern
C++ does have features that can help developers manage memory better.

A memory-safe programming language will prevent a developer from writing code
introducing memory-related bugs.

Rust is designed to make it **impossible** to access unintended memory.
Therefore, the language has several features included in it, such as:

- Mutability
- Ownership
- Borrowing
- Bounds Checking
- Lifetimes

---

## Mutability

by default, variables are not mutable and must be declared so. Furthermore,
even mutable variables can only have a single reference at a time.

---

## Ownership

Set of rules that govern how a **Rust** program manages memory. All programs
have to manage the way they use a computer’s memory while running. Some
languages have garbage collection that regularly looks for no-longer-used
memory as the program runs; in other languages, the programmer must explicitly
allocate and free the memory. **Rust** uses a third approach: memory is
managed through a system of ownership with a set of rules that the compiler
checks. If any of the rules are violated, the program won’t compile. None of
the features of ownership will slow down your program while it’s running.

### Ownership Rules

First, let’s take a look at the ownership rules. Keep these rules in mind as
we work through the examples that illustrate them:

- Each value in **Rust** has an owner.
- There can only be one owner at a time.
- When the owner goes out of scope, the value will be dropped.

In languages with a garbage collector (GC), the GC keeps track of and cleans
up memory that isn’t being used anymore, and we don’t need to think about it.
In most languages without a GC, it’s our responsibility to identify when
memory is no longer being used and to call code to explicitly free it, just as
we did to request it. Doing this correctly has historically been a difficult
programming problem. If we forget, we’ll waste memory. If we do it too early,
we’ll have an invalid variable. If we do it twice, that’s a bug too. We need
to pair exactly one **allocate** with exactly one **free**.

Rust takes a different path: the memory is automatically returned once the
variable that owns it goes out of scope.

---

## Borrowing

We call the action of creating a reference borrowing. As in real life, if
a person owns something, you can borrow it from them. When you’re done, you
have to give it back. You don’t own it. This eliminates dangling pointers and
creates a mechanism like a read-write lock.

Just as variables are immutable by default, so are references. We’re not
allowed to modify something we have a reference to.

Even though borrowing errors may be frustrating at times, remember that it’s
the **Rust** compiler pointing out a potential bug early (at compile time
rather than at runtime) and showing you exactly where the problem is. Then you
don’t have to track down why your data isn’t what you thought it was.

### Mutable References

Mutable references have one big restriction: if you have a mutable reference
to a value, you can have no other references to that value.

We also cannot have a mutable reference while we have an immutable one to the
same value.

Users of an immutable reference don’t expect the value to suddenly change out
from under them! However, multiple immutable references are allowed because no
one who is just reading the data has the ability to affect anyone else’s
reading of the data.

### Dangling References

In languages with pointers, it’s easy to erroneously create a dangling
pointer—a pointer that references a location in memory that may have been
given to someone else—by freeing some memory while preserving a pointer to
that memory. In **Rust**, by contrast, the compiler guarantees that references
will never be dangling references: if you have a reference to some data, the
compiler will ensure that the data will not go out of scope before the
reference to the data does.

---

## Bounds checking

verifying that code accesses are within the boundaries of an array or assigned
memory location.

---

## Lifetimes

A lifetime is a construct the compiler (or more specifically, its borrow
checker) uses to ensure all borrows are valid. Specifically, a variable's
lifetime begins when it is created and ends when it is destroyed. While
lifetimes and scopes are often referred to together, they are not the same.

Take, for example, the case where we borrow a variable via `&`. The borrow has
a lifetime that is determined by where it is declared. As a result, the borrow
is valid as long as it ends before the lender is destroyed. However, the scope
of the borrow is determined by where the reference is used.

In the following example and in the rest of this section, we will see how
lifetimes relate to scopes, as well as how the two differ.

```rust
// Lifetimes are annotated below with lines denoting the creation
// and destruction of each variable.
// `i` has the longest lifetime because its scope entirely encloses
// both `borrow1` and `borrow2`. The duration of `borrow1` compared
// to `borrow2` is irrelevant since they are disjoint.
fn main() {
    let i = 3; // Lifetime for `i` starts. ────────────────┐
    //                                                     │
    { //                                                   │
        let borrow1 = &i; // `borrow1` lifetime starts. ──┐│
        //                                                ││
        println!("borrow1: {}", borrow1); //              ││
    } // `borrow1` ends. ─────────────────────────────────┘│
    //                                                     │
    //                                                     │
    { //                                                   │
        let borrow2 = &i; // `borrow2` lifetime starts. ──┐│
        //                                                ││
        println!("borrow2: {}", borrow2); //              ││
    } // `borrow2` ends. ─────────────────────────────────┘│
    //                                                     │
}   // Lifetime ends. ─────────────────────────────────────┘
```

---

# Zero Cost Abstractions

Zero-cost abstractions is a concept where you can use higher-level languages
without incurring additional runtime cost. Essentially, this means that
**Rust** allows you to write code at a high level of abstraction without
sacrificing performance.

Rust achieves zero-cost abstractions by compiling all code down to machine
instructions, with no interpreter or garbage collector. This way, **Rust**
ensures that any abstractions do not impose any additional runtime costs.

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

Handling concurrent programming safely and efficiently is another of
**Rust**’s major goals. Concurrent programming, where different parts of
a program execute independently, and parallel programming, where different
parts of a program execute at the same time, are becoming increasingly
important as more computers take advantage of their multiple processors.
Historically, programming in these contexts has been difficult and error
prone: **Rust** hopes to change that.

Initially, the **Rust** team thought that ensuring memory safety and
preventing concurrency problems were two separate challenges to be solved with
different methods. Over time, the team discovered that the ownership and type
systems are a powerful set of tools to help manage memory safety and
concurrency problems! By leveraging ownership and type checking, many
concurrency errors are compile-time errors in **Rust** rather than runtime
errors. Therefore, rather than making you spend lots of time trying to
reproduce the exact circumstances under which a runtime concurrency bug
occurs, incorrect code will refuse to compile and present an error explaining
the problem. As a result, you can fix your code while you’re working on it
rather than potentially after it has been shipped to production. We’ve
nicknamed this aspect of **Rust** fearless concurrency. Fearless concurrency
allows you to write code that is free of subtle bugs and is easy to refactor
without introducing new bugs.

Source: [Fearless Concurrency](https://doc.rust-lang.org/book/ch16-00-concurrency.html)

- Threads
- Message Passing
- Shared-State Concurrency
- Extensible Concurrency with `Sync` & `Send` Traits

---

## Threads

In most current operating systems, an executed program’s code is run in
a process, and the operating system will manage multiple processes at once.
Within a program, you can also have independent parts that run simultaneously.
The features that run these independent parts are called threads. For example,
a web server could have multiple threads so that it could respond to more than
one request at the same time.

Splitting the computation in your program into multiple threads to run
multiple tasks at the same time can improve performance, but it also adds
complexity. Because threads can run simultaneously, there’s no inherent
guarantee about the order in which parts of your code on different threads
will run. This can lead to problems, such as:

- Race conditions, where threads are accessing data or resources in an
  inconsistent order
- Deadlocks, where two threads are waiting for each other, preventing both
  threads from continuing
- Bugs that happen only in certain situations and are hard to reproduce and
  fix reliably

Rust attempts to mitigate the negative effects of using threads, but
programming in a multithreaded context still takes careful thought and
requires a code structure that is different from that in programs running in
a single thread.

---

## Message Passing

One increasingly popular approach to ensuring safe concurrency is message
passing, where threads or actors communicate by sending each other messages
containing data. Here’s the idea in a slogan from the Go language
documentation: “Do not communicate by sharing memory; instead, share memory by
communicating.”

To accomplish message-sending concurrency, **Rust**'s standard library
provides an implementation of channels. A channel is a general programming
concept by which data is sent from one thread to another.

A channel has two halves: a transmitter and a receiver. The transmitter half
is the upstream location where you put rubber ducks into the river, and the
receiver half is where the rubber duck ends up downstream. One part of your
code calls methods on the transmitter with the data you want to send, and
another part checks the receiving end for arriving messages. A channel is said
to be closed if either the transmitter or receiver half is dropped.

---

## Shared-State Concurrency

Message passing is a fine way of handling concurrency, but it’s not the only
one. Another method would be for multiple threads to access the same shared
data. Consider this part of the slogan from the Go language documentation
again: “do not communicate by sharing memory.”

What would communicating by sharing memory look like? In addition, why would
message-passing enthusiasts caution not to use memory sharing?

In a way, channels in any programming language are similar to single
ownership, because once you transfer a value down a channel, you should no
longer use that value. Shared memory concurrency is like multiple ownership:
multiple threads can access the same memory location at the same time.
**Rust**’s type system and ownership rules greatly assist in getting this
management correct.

**Mutex** is an abbreviation for mutual exclusion, as in, a mutex allows only
one thread to access some data at any given time. To access the data in
a mutex, a thread must first signal that it wants access by asking to acquire
the mutex’s lock. The lock is a data structure that is part of the mutex that
keeps track of who currently has exclusive access to the data. Therefore, the
mutex is described as guarding the data it holds via the locking system.

---

## Extensible Concurrency with Sync & Send Traits

Interestingly, the **Rust** language has very few concurrency features. Almost
every concurrency feature we’ve talked about so far in this chapter has been
part of the standard library, not the language. Your options for handling
concurrency are not limited to the language or the standard library; you can
write your own concurrency features or use those written by others.

However, two concurrency concepts are embedded in the language: the
`std::marker` traits `Sync` and `Send`.

- Allowing Transference of Ownership Between Threads with `Send`
- Allowing Access from Multiple Threads with `Sync`

---

# Getting Started with **Rust**

- Install **Rust** by following instructions at [Rust
  Lang](https://rust-lang.org)
- Get familiar with **cargo**
  - `cargo build`
  - `cargo build --release`
  - `cargo run`
  - `cargo add`
- `rust-analyzer` is a modular compiler frontend for the **Rust** language. It
  is a part of a larger rls-2.0 effort to create excellent IDE support for
  **Rust**.

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

# Ecosystem and Community

- The **Rust** Standard Library: A rich collection of pre-built modules.
- Package Management with Cargo: Simplifies dependency management.
- A growing community with numerous open-source projects.

---

# Resources and Next Steps

- Official **Rust** Documentation [rust-lang.org](https://rust-lang.org)
- [The **Rust** Programming Language Book](https://doc.rust-lang.org/book)
- [Rustlings](https://github.com/rust-lang/rustlings)
- Explore real-world **Rust** projects and contribute.

---

# Questions
