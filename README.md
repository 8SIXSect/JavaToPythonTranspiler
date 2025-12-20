# JavaToPythonTranspiler
Transpiles Java source code into Python source code

## Installation
```
pip install java-to-python-transpiler
```

## Example Usage
```python
import java_to_python_transpiler as jtp

python_code: str = jtp.transpile_methods.java_to_python_from_string("""
public class Main {
    public static void main(String[] args) {
        System.out.println("Merry Christmas");
    }
}
""")
print(python_code)
```

### Output in Terminal
```python
class Main:
    def main(args):
        print("Merry Christmas)
```

## Functions & Dataclasses

### java_to_python_from_string
`java_to_python_from_string(user_input: str) -> str | TranspilerFailure`

This is the primary function provided by this library.

Takes in a `user_input` and transpiles it into Python source code. It will return
a String if successful. Otherwise, it will return a `TranspilerFailure`

### TranspilerFailure

This class represents a failure that occurred in the Transpiler.

`error_message: str` -> a String that provides informaion about the error that
occurred 

## Supported Features
- Arithmetic Expressions (+, -, *, /)
- While Loops, If-Else Conditionals
- Method Declarations and Calls
- Class Declarations
- Variable Initialization and Declaration

## License

[MIT](./LICENSE) © 8SIXSect
