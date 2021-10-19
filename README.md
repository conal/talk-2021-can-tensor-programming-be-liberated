## Can Tensor Programming Be Liberated from the Fortran Data Paradigm?

Abstract:
<blockquote>
Classic Fortran programs used "`GO TO`" for control and (multi-dimensional) arrays for data.
While these unstructured building blocks are adequate for encoding implementations, they fail to yield clear understanding of the essential nature of algorithms and their correctness.
Although "`GO TO`" has largely disappeared from modern programming, arrays are still widely embraced for parallel programming and machine learning.
The resulting programming style suffers in safety and compositionality, leading to code that is unnecessarily difficult to write, read, prove, and reuse.

The main message of this talk is that "array algorithms" are often not naturally array algorithms at all, but rather an error-prone, composition-resistant enmeshing of a safe, simple, and illuminating algorithm on a natural (non-array) data type, together with details of decoding from and encoding to arrays.
By disentangling these natural data types and corresponding algorithms from their array encodings, we can gain deeper understanding of known algorithms and easily discover correct new algorithms, many of which are well-suited for the modern age of parallel hardware.
Where desired, these clear, correct, and compositional algorithms can then be safely, correctly, and systematically (even automatically) converted to operate on arrays by applying a few simple principles in the form of well-known type isomorphisms.
</blockquote>
