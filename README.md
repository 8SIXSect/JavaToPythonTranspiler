# JavaToPythonTranspiler
Transpiles Java source code into Python source code

## Installation
```
pip install java-to-python-transpiler
```

## Functions

### java_to_python_from_string
`java_to_python_from_string(user_input: str) -> str | TranspilerFailure`

This is the primary function provided by this library.

Takes in a `user_input` and transpiles it into Python source code. It will return
a String if successful. Otherwise, it will return a `TranspilerFailure`

## Data

### TranspilerFailure

This class represents a failure that occurred in the Transpiler.

`error_message: str` -> a String that provides informaion about the error that
occurred 

