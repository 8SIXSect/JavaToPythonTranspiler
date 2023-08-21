"""
This module contains methods for transpiling source to source
"""

from java_to_python_transpiler.java_lexer import tokenize_code


PROMPT = ">>> "


def lexer_shell() -> None:
    """
    This function is a REPL-like program that allows one interact
    with the Lexer using a small shell

    :return: None
    """

    while True:
        user_input = input(PROMPT)
        lexer_result = tokenize_code(user_input)
        print(lexer_result)

