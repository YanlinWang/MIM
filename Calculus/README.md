# FHJ: A Formal Model for Hierarchical Dispatching and Overriding

This folder contains a Scala project that implements FHJ as a simple interpreter.

- [./src/framework/FHJ.scala](https://github.com/YanlinWang/MIM/blob/master/Calculus/src/framework/FHJ.scala): the abstract syntax of FHJ, typing and semantic rules.
- [./src/framework/Parser.scala](https://github.com/YanlinWang/MIM/blob/master/Calculus/src/framework/Parser.scala): the parser implemented by Scala Packrat Parsing.
- [./src/framework/Main.scala](https://github.com/YanlinWang/MIM/blob/master/Calculus/src/framework/Main.scala): the main program to run the examples.

## How to run
The project is developed using Scala Eclipse IDE on Windows. Please install Scala Eclipse, go back to the [parent directory](https://github.com/YanlinWang/MIM) and import the entire fold "Calculus/" into the IDE as a Scala project. Run Main.scala as a Scala application.

## Examples
[./examples/] has some examples introduced in the paper. You can also write your own programs and run them in Main.
