## Can Tensor Programming Be Liberated from the Fortran Data Paradigm?

Prepared for the [2021 Oxford Tensor Programming Seminar](https://www.cs.ox.ac.uk/seminars/2418.html).

[Video](https://www.youtube.com/watch?v=oaIMMclGuog)

[Slides](http://conal.net/talks/can-tensor-programming-be-liberated.pdf)

Related paper (and video): [*Generic parallel functional programming: Scan and FFT*](http://conal.net/papers/generic-parallel-functional/) (ICFP 2017)

### Abstract

Classic Fortran programs used "`GO TO`" for control and (multi-dimensional) arrays for data.
While these unstructured building blocks suffice to encode implementations, they fail to yield clear understanding of the essential nature of algorithms and their correctness.
Although "`GO TO`" has largely disappeared from modern programming, arrays are still widely embraced for parallel programming and machine learning.
The resulting programming style suffers in safety and compositionality, leading to code that is unnecessarily difficult to write, read, prove, and reuse.

The main message of this talk is that "array algorithms" are often not naturally array algorithms at all, but rather an error-prone, composition-resistant enmeshing of a safe, simple, and illuminating algorithm on a natural (non-array) data type, together with details of decoding from and encoding to arrays.
By disentangling these natural data types and corresponding algorithms from their array encodings, we can gain deeper understanding of known algorithms and easily discover correct new algorithms, many of which are well-suited for the modern age of parallel hardware.
Where desired, these clear, correct, and compositional algorithms can then be safely, correctly, and systematically (even automatically) converted to operate on arrays by applying a few simple principles in the form of well-known type isomorphisms.

### Video erratum

Starting at 12:05 in [video](https://www.youtube.com/watch?v=oaIMMclGuog), Jeremy conveyed a question about satisfying the key specification (commuting diagram) via degenerate `parseZ`. I'm afraid it was a bit early in the morning for me, and the answer I gave was rambling and incorrect. We really are searching for any `scanZ` that satisfies the specification, but we're *not* allowed arbitrary choices of `parseZ`. Rather, the parsing functions correspond naturally to the essential (non-array) `Z` type. Much later in the talk, I define all of these parsers parametrically. They're always isomorphisms for tries / Naperian functors (i.e., generalized, compositional, post-Fortran tensors). (They needn't isomorphisms for some non-Naperian functors, but even then they're semi-invertible and are defined naturally.)
