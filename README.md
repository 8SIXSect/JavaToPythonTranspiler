# JavaToPythonTranspiler
Transpiles Java source code into Python source code

## Installation
`NotImplemented`

## Functions

### java_to_python_from_string
`java_to_python_from_string(user_input: str) -> str | TranspilerFailure`

This is the primary function provided by this library.

This function takes in a String (`user_input`) and transpiles it into a
result.

This function will return either a String (which is the python source), or
it may fail, and return a `TranspilerFailure` object. This object contains a
singular field, `error_message`, which is a String representing the error
that occurred.

### java_to_python_from_file
`java_to_python_from_file(
    file_path: Optional[str] = None,
    file_object: Optional[TextIOWrapper] = None
    ) -> str | TranspilerFailure`

This function is the secondary purpose of this library; it allows one to
transpile the contents of a file (of java source code) to Python source.

`file_path` (optional) is a String that defaults to None; if set to None,
then `file_object` must have a value.

`file_object` (optional) is a TextIOWrapper that defaults to None; if set
to None, then `file_path` must have a value.

if both `file_path` and `file_object` each have a value of None, then a
ValueError will be thrown.

## Data

### TranspilerFailure

This class represents a failure that occurred in the Transpiler.

`error_message: str`

`error_message` represents the error that occurred; it is a String of
information that informs the user what went wrong.

