# compati

Currently working on C style structs. Would like to add lists, strings, arrays. Would like to add pythonic lists that some python like list operations but lol 

dune exec -- ./compati.exe ../tests/struct-test1.compati > t.ll && /usr/local/opt/llvm/bin/llc -relocation-model=pic t.ll > t.s && gcc -o t.exe t.s && ./t.exe > t.out
