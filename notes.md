# Notes for "Can Tensor Programming Be Liberated from the Fortran Data Paradigm?"

Friday October 29, 2021 as part of the Oxford Tensor Computation seminar

What old talks do I have for source material, and what do I want to add?

## Some old talks and papers:

*   "Understanding parallel scan" 2013:
    *   CUDA C implementation that started my journey
*   "Generic Functional Parallel Algorithms---Scan and FFT" Sept 2017 (ICFP):
    *   Several good, updated examples
    *   Omits bush examples
*   "Generic parallel scan" 2016 (updated 2017):
    *   Bush definition and examples
    *   Some applications.
*   "Generic FFT" (2016):
    *   Many example pictures.
    *   "Factor types, not numbers."
*   "From Haskell to Hardware via CCCs" 2015:
    *   Generalized matrices.
    *   Bitonic sort
*   "Deep learning rebooted"


## Miscellaneous thoughts

*   Optimization obscures clarity and composability, so do it late.
*   Possible titles:
    *   Tensor programming for the post-Fortran age
    *   Tensor programming after Fortran
    *   Tensor programming, post-Fortran
    *   Can tensor programming be liberated from the Fortran data paradigm?
    *   Data parallel programming without arrays
    *   Array programming considered harmful
*   Composition is our problem-solving super-power for implementation and its correctness.
    N-dimensional arrays of scalars (i.e., Fortran data) compose poorly, are typically unsafe, and obscure algorithm understanding, and thwart correctness.
    Hence the post-Fortran age of compositional data types.
*   Functional generic programming has revealed a general, elegant, composition-friendly *algebra* of container data types: three operations (+, ×, ∘) and their identities (0, 1, I).
*   2/3 of this algebra *subsumes* (n-dim) arrays and so can coexist.
*   Typical "array algorithms" (afaik) are not naturally array algorithms at all, but rather a composition-resistant enmeshing of three algorithms:
    *   A safe, simpler, and more illuminating algorithm on a natural, non-array type.
    *   Decoding arrays into the natural type.
    *   Encoding the natural type into arrays.
*   A hint ("smell") that something is wrong: witness the "reshaping" operations added to recent array ("tensor") APIs.
    The container algebra greatly simplifies and generalizes reshaping as *natural* (rather than ad hoc) algebraic isomorphisms.
*   The encoding and decoding algorithms are clerical and typically unsafe in theory and error-prone in practice.
*   The natural algorithm can sometimes (often?) be decomposed into very simple, orthogonal pieces, with each piece corresponding to an operator or identity in the standard container algebra.
    These pieces are easier to understand and easier to prove correct, yielding an infinite family of parallel-friendly algorithm, each guaranteed correct by construction.
*   The data algebra can be encoded and decoded into arrays generically and safely.
    We can even generate general proofs of safety and correctness.
*   A general class of algebraic data types (even nested/non-regular) can be systematically (even automatically) converted to and from the standard algebra.
*   Use Agda and commutative diagrams.
*   Parallel scan and FFT as in [*Generic functional parallel algorithms: Scan and FFT*].
    Show pictures from the paper & talk even if I don't have Agda versions.
*   Simpler example: a parallel fold, e.g., `max`.
    Hopefully in Agda.
*   Show how to map natural parallel algorithms (on non-arrays) to parallel array algorithms, systematically and *correctly*.

## A possible outline

*   The Unix philosophy:
    *   How Unix defeated its own philosophy (parser and unparser)
    *   How array programming does the same ("parser and unparser")
*   An efficient array program:
    *   Parallel prefix in CUDA
    *   Realization: not an *array* algorithm
*   Questions:
    *   How to tease out the natural data type & algorithm hiding behind an array program?
    *   Conversely, how to turn natural data types & algorithms into array programs correctly and systematically (even automatically)?
*   Two problems:
    *   Indexing ("word-at-a-time") vs structure
    *   Indexing with numbers
*   Correctness:
    *   Commutative diagram with natural type and algorithm above and array below

*   "Parsing" indices --- index isomorphisms
*   Trie structures (exponentials) and their isomorphisms

Miscellany:

*   What makes a type "natural" for an algorithm is the simplicity of expressing the algorithm and proof in terms of that type.
    The goal is to factor the array function in a standard way as `unparse ∘ f ∘ parse` where `f` is simple and general.
*   Quote from Dijkstra's "The Humble Programmer"
*   Our mental habits are trained by archaic technology
*   Optimization is important, but it obscures clarity and thwarts composability, so do it late.


<!-- References -->

[*Generic functional parallel algorithms: Scan and FFT*]: http://conal.net/papers/generic-parallel-functional "paper by Conal Elliott (2017)"
