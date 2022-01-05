# compati

Currently working on C style structs. Would like to add lists, strings, arrays. Would like to add pythonic lists that some python like list operations but lol 

Run these commands from the src folder after running ```dune build```

## Tests: 
1. Struct declaration
```
dune exec -- ./compati.exe ../tests/struct-decl-test.compati > struct-decl-test.ll && /usr/local/opt/llvm/bin/llc -relocation-model=pic struct-decl-test.ll > struct-decl-test.s && gcc -o struct-decl-test.exe struct-decl-test.s && ./struct-decl-test.exe > struct-decl-test.out
```

2. Struct definition
```
dune exec -- ./compati.exe ../tests/struct-defn-test.compati > t.ll && /usr/local/opt/llvm/bin/llc -relocation-model=pic struct-defn-test.ll > struct-defn-test.s && gcc -o struct-defn-test.exe struct-defn-test.s && ./struct-defn-test.exe > struct-defn-test.out
```

3. Struct assignment and access
```
dune exec -- ./compati.exe ../tests/struct-assign-test.compati > t.ll && /usr/local/opt/llvm/bin/llc -relocation-model=pic struct-assign-test.ll > struct-assign-test.s && gcc -o struct-defn-test.exe struct-assign-test.s && ./struct-assign-test.exe > struct-assign-test.out
```
5. String test
```
dune exec -- ./compati.exe ../tests/string-test.compati > string-test.ll && /usr/local/opt/llvm/bin/llc -relocation-model=pic string-test.ll > string-test.s && gcc -o string-test.exe string-test.s && ./string-test.exe > string-test.out
```

6. Char test 
```
dune exec -- ./compati.exe ../tests/char-test.compati > char-test.ll && /usr/local/opt/llvm/bin/llc -relocation-model=pic char-test.ll > char-test.s && gcc -o char-test.exe char-test.s && ./char-test.exe > char-test.out
 ```

7. Array test
 ```
dune exec -- ./compati.exe ../tests/array-test-1.compati > array-test-1.ll && /usr/local/opt/llvm/bin/llc -relocation-model=pic array-test-1.ll > array-test-1.s && gcc -o array-test-1.exe array-test-1.s && ./array-test-1.exe > array-test-1.out
 ```
8. Struct fail test (checking semant.ml's ability to detect when someone try's to use an accessor (.) on a nonstruct variable)
```
dune exec -- ./compati.exe ../tests/struct-test-fail.compati > struct-test-fail.ll && /usr/local/opt/llvm/bin/llc -relocation-model=pic array-test-1.ll > struct-test-fail.s && gcc -o array-test-1.exe struct-test-fail.s && ./struct-test-fail.exe > struct-test-fail.out
```
9. Struct fail test 2  (duplicate members in struct)
```
dune exec -- ./compati.exe ../tests/struct-test-fail2.compati > struct-test-fail2.ll && /usr/local/opt/llvm/bin/llc -relocation-model=pic struct-test-fail2.ll > struct-test-fail2.s && gcc -o struct-test-fail2.exe struct-test-fail2.s && ./struct-test-fail2.exe > struct-test-fail2.out
```

10. Struct fail void test (void members)
```
dune exec -- ./compati.exe ../tests/struct-test-void.compati > struct-test-void.ll && /usr/local/opt/llvm/bin/llc -relocation-model=pic struct-test-fail-void.ll > struct-test-void.s && gcc -o struct-test-void.exe struct-test-void.s && ./struct-test-void.exe > struct-test-void.out
```

11. Struct expr test (testing struct assignment with expressions)
```
dune exec -- ./compati.exe ../tests/struct-expr-test.compati > struct-expr-test.ll && /usr/local/opt/llvm/bin/llc -relocation-model=pic struct-expr-test.ll > struct-expr-test.s && gcc -o struct-expr-test.exe struct-expr-test.s && ./struct-expr-test.exe > struct-expr-test.out
```
12. Struct member comparison test 
```
dune exec -- ./compati.exe ../tests/struct-comp.compati > struct-comp.ll && /usr/local/opt/llvm/bin/llc -relocation-model=pic struct-comp.ll > struct-comp.s && gcc -o struct-comp.exe struct-comp.s && ./struct-comp.exe > struct-comp.out
```
Final report: https://docs.google.com/document/d/1-Rf-jKuwo3Hb1Yuon1iwI6_uqQ8DNLo_-8Y9NBVwZtg/edit?usp=sharing
