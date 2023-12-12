"""
This module contains methods for transpiling source to source
"""

from java_to_python_transpiler.scan_and_lex_input import LexerResult, scan_and_tokenize_input 


PROMPT = ">>> "


def lexer_shell() -> None:
    """
    This function is a REPL-like program that allows one interact
    with the Lexer using a small shell

    :return: None
    """

    while True:
        user_input: str= input(PROMPT)
        lexer_result: LexerResult = scan_and_tokenize_input(user_input)
        
        if not lexer_result.was_successful:
            print(lexer_result.error_message)
        else:
            print(lexer_result.tokens)

