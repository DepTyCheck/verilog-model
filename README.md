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
Here we would have a list of reported bugs.

TBD

## Running

You can run generator of semantically correct SystemVerilog definitions.
The easiest way is to use [pack](https://github.com/stefan-hoeck/idris2-pack), the package manager for Idris 2 programming language:

```console
$ pack run verilog-model
```

The program supports several options, which you can view by using the `--help` option.
