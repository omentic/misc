# lalge (name tbd)

A functional, dead simple linear algebra library written in Nim. Mostly for learning purposes.

This is also serving as my finals review, haha.

If you're looking for an actually robust linear algebra library for Nim, check out [Neo](https://github.com/andreaferretti/neo).

## What's this language?

Nim is a compiled whitespace-sensitive systems language with a strong type system and interesting memory management capabilities.

It doesn't quite beat Python's usability or Rust's features, but I like it quite a lot, and think it's a good complement to other languages I use.

Nim's [UFCS](https://en.wikipedia.org/wiki/Uniform_Function_Call_Syntax) (TL;DR: `a.add(b) == add(a, b)`) and operator overloading support is also a major draw for this type of project.

## Architecture

lalge uses Nim's `seqs` - dynamic arrays, equivalent to Rust's `Vec`, Go's `slice`, or Java's `ArrayList` - to implement matrices. This may have some performance overhead. On the other hand, it has the benefit of making the code much simpler.

This also has the benefit of being able to declare matrices as such:
```nim
example: Matrix = @[
  @[1, 0, 0],
  @[0, 1, 0],
  @[0, 0, 1],
]
```

## Goals
- [ ] Cover the basic operations associated with the MATH 221 / typical introductory linear algebra curriculum
  - [x] Addition / Subtraction
  - [x] (Scalar / Vector) Multiplication
  - [x] Inverse
  - [x] Transpose
  - [ ] Eigenvalues / Eigenvectors
  - [ ] everything that i am forgetting
- [ ] Helpful characteristic functions
  - [ ] Dimension
  - [ ] Rank / Nullity
  - [ ] Spans
  - [ ] Determinant
- [x] Support arbitrary-sized matrices with internal seq representation
- [x] Support specialized "vectors", as opposed to Nx1 matrices

After I get to a reasonable point of completeness, I'd love to go through and compare my library to Eigen (generally praised as a Very Good library)
