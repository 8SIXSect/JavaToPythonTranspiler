# Todo List
Stuff I need to get Done


## TODO
- [x] Add Lexer support for '.'
- [x] Add Parser support for 'void' Token
- [x] Add Parser support for using identifiers as a variable type
- [x] Add Parser support for using arrays in a variable type
- [x] Add Parser & Emitter support for using String Literals in factors
- [] Everything todo w/ expr stops at EOF but it really should stop at SEMICOLON; however, that may only apply to statements but no if expr reaches semi, it stops
- [x] Add Parser support for using arrays in a return type
- [] Enhance the documentation of the functions in the transpiler file
- [x] Enhance error information; track rows and columns (possibly more)
- [] Create test(s) for usings arrays in variable type
- [] Create test(s) for using identifiers as a variable type (may not be nes.)
- [] Add Parser & Emitter support for using Parenthesis in factors
- [] Add Parser & Emitter support for exponentation
- [] Add Parser & Emitter support for modulus operator
- [] Create tests (more) comprehensive tests for variable types
- [] Implement error checking in the Emitter (so checking that certain var types exist)
- [] Add Full Support for Float Literals (possibly combine w/ decimal and call it number?)
- [] Implement tests for ArithmeticOperator and ComparisonOperator
- [] Writes tests for java_to_python_* functions
- [] reformat `indent_level` to have a default value of 0
- [] Fix `method_call` case in format_ast
- [] Create a function that returns a list of the supported features of the transpiler
- [] Add full support for comments
- [] Restructure code in parse_tokens_for_block_statement to be more elegant
- [] (Emitter) Create a test for if-elif-else clauses altogether.


All you need to do is add error handling for now and remove all the TODO comments
in this project (or like move them here). Then update your version, and push this
to pypi. Update heroku, and after that, remake the website :)

