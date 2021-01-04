# LambdaCalculusInterpreter

A lambda calculus interpreter with type checking.

## Installation

This project requires the [ATS2](http://www.ats-lang.org/) compiler to be installed already.

```bash
git clone https://github.com/rahul-arasikere/LambdaCalculusInterpreter.git
cd LambdaCalculusInterpreter
make
```

You can optionally specify `make -j#` to run '#' jobs in parallel, if you have a multicore cpu.

To clean the build files run:

```bash
make clean
```

## Usage

To generate the `filename.c` file:

```bash
./lambda filename
```

To compile it to an executable:

```bash
gcc -o filename filename.c
```

## License

[MIT](https://choosealicense.com/licenses/mit/)
