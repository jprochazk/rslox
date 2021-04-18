Rust implementation of [Lox](https://craftinginterpreters.com/).

This implementation is compatible with clox, except for closures, which capture by value instead of by variable.

### Usage

To see all possible flags, run:
```
$ cargo run -- --help
```

The binary supports executing a file and opening a REPL environment.

*File*
```
$ cargo run --release --example cli -- file test.lox
fib(30) = 832040
Calculated in 226.47212982177734ms
```

*REPL*
```
$ cargo run --release --example cli -- repl
> var a = 10;
> print a;
10
```

Running the binary without the `--release` flag will additionally print the disassembly of the program, as well as disassemble each instruction as the program is running.

```
$ cargo run --example cli -- repl
> var a = 10;
== MAIN ==
0000    1 Constant      10
0002    | DefineGlobal  a
0004    | Return
==========
        |
        |
0000    1 Constant      10
        |
        | [10]
0002    | DefineGlobal  a
        |
        |
0004    | Return
```