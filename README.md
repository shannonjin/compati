# compati

Currently working on C style structs. Would like to add lists, strings, arrays. Would like to add pythonic lists that some python like list operations but lol 

Run these commands from the src folder after running "dune build"

Tests: 
1. Struct declaration
dune exec -- ./compati.exe ../tests/struct-decl-test.compati > t.ll && /usr/local/opt/llvm/bin/llc -relocation-model=pic struct-decl-test.ll > struct-decl-test.s && gcc -o struct-decl-test.exe struct-decl-test.s && ./struct-decl-test.exe > struct-decl-test.out

2. Struct definition
dune exec -- ./compati.exe ../tests/struct-defn-test.compati > t.ll && /usr/local/opt/llvm/bin/llc -relocation-model=pic struct-defn-test.ll > struct-defn-test.s && gcc -o struct-defn-test.exe struct-defn-test.s && ./struct-defn-test.exe > struct-defn-test.out

3. Struct assignment and access
dune exec -- ./compati.exe ../tests/struct-assign-test.compati > t.ll && /usr/local/opt/llvm/bin/llc -relocation-model=pic struct-assign-test.ll > struct-assign-test.s && gcc -o struct-defn-test.exe struct-assign-test.s && ./struct-assign-test.exe > struct-assign-test.out
