OCaml-minilisp
====

OCaml-minilisp is a simple subset of lisp.


Installation
----
1. Clone the repo
2. `cd ocaml-minilisp`
3. `make`


Test
----
1. `make test`


Repl
----

    ./minilisp
    minilisp> (define
    (fib n x1 x2)
    (if (<= n 0)
    x1
    (fib (- n 1) x2 (+ x1 x2))))
    Func(fib, [n, x1, x2], Cond(Apply(<=, Var(n), Num(0)), Var(x1), Apply(fib, Apply(-, Var(n), Num(1)), Var(x2), Apply(+, Var(x1), Var(x2)))))

    minilisp> (fib 100 0 1)
    Num(3736710778780434371)


Evaluation of source file
----

    $ cat sample.minilisp 
    (define
      (fib n x1 x2)
      (if (<= n 0)
          x1
          (fib (- n 1) x2 (+ x1 x2))))

    (print (fib 100 0 1))


    $ ./minilisp sample.minilisp 
    Num(3736710778780434371)


License
----
OCaml-minilisp is released under the MIT license.


Author
----
Mitsunori Komatsu  < komamitsu [at] gmail [dot] com >

