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
There is a list of reported bugs:

- iverilog
  - [Assertion 'net->type_ == IVL_SIT_REG'](https://github.com/steveicarus/iverilog/issues/1213)
  - [Unable to elaborate r-value](https://github.com/steveicarus/iverilog/issues/1217)
  - [Unable to resolve label v0x55abf7187e10_0](https://github.com/steveicarus/iverilog/issues/1218)
  - [Expression width does not match width of logic gate](https://github.com/steveicarus/iverilog/issues/1221)
  - [18vvp_arith_cast_int: recv_vec4 not implemented](https://github.com/steveicarus/iverilog/issues/1222)
  - [11vvp_fun_not: recv_real(0.000000) not implemented](https://github.com/steveicarus/iverilog/issues/1223)
  - [Packed vs unpacked dimension confusion](https://github.com/steveicarus/iverilog/issues/1224)
  - [elaborate.cc:1661: failed assertion prts[0]->unpacked_dimensions()==0](https://github.com/steveicarus/iverilog/issues/1231)
- verilator
  - [Real arrays with opposite index ranges](https://github.com/verilator/verilator/issues/5877)

### Known bugs

We also found some already known bugs:

- [Missing MULTIDRIVEN warning for variable with multiple drivers](https://github.com/verilator/verilator/issues/5887)

## Running

You can run generator of semantically correct SystemVerilog definitions.
The easiest way is to use [pack](https://github.com/stefan-hoeck/idris2-pack), the package manager for Idris 2 programming language:

```console
$ pack run verilog-model
```

The program supports several options, which you can view by using the `--help` option.
