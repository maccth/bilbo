# Bilbo
Bilbo is a domain-specific language for programming with graphs at a high level of abstraction.

Bilbo is completely cross-platform and runs on linux, macOS and windows.

To use Bilbo you'll need [.NET Core](https://dotnet.microsoft.com/download).


## Running Bilbo
To run Bilbo, download the [release](https://github.com/maccth/bilbo/releases/tag/v1.0) and unzip `bilbo.zip`.

To start the REPL:
```
dotnet bilbo/Bilbo.dll
```

To run a program use the `--bilbofile` option:
```
dotnet bilbo/Bilbo.dll --bilbofile examples/shortest-path.bb
```

There are number of [example programs](/examples) ready to run.

Finally, the option `--debug` can be used to run, either the REPL or the normal interpreter, in debug mode.

## Building and Running from Source
To build or run from source, clone the repository and resolve [Paket](https://fsprojects.github.io/Paket/index.html) dependencies.

To build and run the REPL:
```
dotnet run --project src/Bilbo
```

The same options for running programs (`--bilbofile`) and debug mode (`--debug`) can be used.

## Running the Tests
To run the tests, first ensure you can build from source.

To run the evaluator tests:
```
dotnet run --project test/EvaluatorTests
```
The evaluator tests are one of three test suites. The others are `test/GraphTests` and `test/ParserTests` which can be run similarly.
