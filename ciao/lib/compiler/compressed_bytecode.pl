:- module(compressed_bytecode,
        [compressLZ/1,
         copyLZ/1],
         [assertions]).

:- impl_defined(
        [compressLZ/1,
         copyLZ/1]).

:- doc(title, "Predicates for the compression and uncompression of bytecode.").

:- doc(author, "Oscar Portela Arjona").
