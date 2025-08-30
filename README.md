# SystemVerilog model for property-based testing

This is a set of descriptions (a model) of semantically correct definitions in the SystemVerilog language
expressed with dependent types in [Idris 2](https://github.com/idris-lang/Idris2) programming language.

This model is designed for property-based testing using the [DepTyCheck library](https://github.com/buzden/deptycheck/),
a library for property-based testing and generation of dependently-typed data.

## The model

This model is not meant to be the full specification of SystemVerilog.
However, it is not strictly required for good property-based testing.

Currently, we test the following property: for every semantically correct SystemVerilog description (from the supported subset),
an instrument taking it should accept it without issue.
For several particular instruments supporting simulation,
we also run this simulation for several ticks in order to check it is feasible.

### Features

We are currently working on extending supported features of SystemVerilog,
and there would be a list of supported features.

TBD

### Found bugs

Currently we have found several bugs in open-source instruments working with SystemVerilog.
We are on the way of reporting them officially.

To see the bugs and issues we have discovered, please visit our [website](https://deptycheck.github.io/verilog-model/).

## Installation

The generator of semantically correct SystemVerilog definitions uses [pack](https://github.com/stefan-hoeck/idris2-pack),
the package manager for the Idris 2 programming language.

You can either:

- **Build manually:**
1. Install `pack` (see [pack installation guide](https://github.com/stefan-hoeck/idris2-pack)).
2. Build the project:
    ```console
    pack build verilog-model
    ```

- **Use the prebuilt Docker container** from the [packages](https://github.com/deptycheck/verilog-model/pkgs/container/verilog-model).

## Running

After building, you have two options to run the generator:

Run directly with `pack`:
```console
pack run verilog-model -h
```

Install once and run as a standalone executable:
```console
pack install verilog-model
verilog-model --help
```

(this is already done inside the Docker container, so you can just run `verilog-model`).

### Usage

The generator produces SystemVerilog test designs.  
Each generated file corresponds to a **separate test**.

- By default, tests are printed to the console, but you can specify a directory to save files using `--to`.
- Every run produces different tests. You can set the seed manually to make results reproducible with `--seed` (the `--seed` option expects two numbers). 
  
  To see which seeds are actually used, add the `--seed-name` flag to include the seed in file names, and the `--seed-content` flag to print the seed inside the file.


For all available options, run:
```console
verilog-model -h
```

Here is a basic usage example. Generate 10 tests in the tests folder with a fixed seed:
```console
verilog-model --to ./tests -n 10 --seed 12345,6789
```
