"""
This module contains methods for transpiling source to source
"""

from java_to_python_transpiler.scan_and_lex_input import tokenize_input_string


PROMPT = ">>> "


def lexer_shell() -> None:
    """
    This function is a REPL-like program that allows one interact
    with the Lexer using a small shell

    :return: None
    """

    while True:
        user_input = input(PROMPT)
        lexer_result = tokenize_input_string(user_input)
        print(lexer_result)

