diaspec
=======

This is a very small README file. It'll get bigger, promise.

The intention is to create a minimalistic compiler for the core Diaspec language 
presented in [this paper](http://people.bordeaux.inria.fr/pwalt/docs/progfw.pdf). There
are a few examples included in the `examples/` directory, and simple invocations are presented here.

Installation
------

As usual one just needs to clone the repository, then do

```sh
cabal configure
cabal build
```

after which you should be good to go using the `./diaspec` command, which is simply a
symlink into `dist/build/diaspec`, which is where Cabal puts build products by default.

Running
-----

Of course, should you ever feel lost, you can use

```sh
./diaspec --help
```

This will inform you of three modes, `java`, `racket`, and `pretty`. The most interesting
one is `java`, which will generate a Java framework from a specification. By default it will
end up in the `gen/` directory relative to `$PWD`. An invocation might simply be

```sh
./diaspec examples/webcam.spec
```

This would generate the abstract classes needed for the example used in the paper referenced above.
More documentation is, as always, on the TODO list.
