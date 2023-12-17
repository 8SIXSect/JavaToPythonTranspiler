"""
This module contains methods for transpiling source to source
"""

from java_to_python_transpiler import java_to_python


PROMPT = ">>> "


def test_parser(): 
    user_input = "86"

    lexer_result: java_to_python.LexerResult = \
            java_to_python.scan_and_tokenize_input(user_input)

    if not lexer_result.was_successful:
        print(lexer_result.error_message)
        return

    parser_result: java_to_python.ParserResult = \
            java_to_python.parse_list_of_tokens(lexer_result.tokens)

    if (not parser_result.was_successful) or (parser_result.syntax_tree is None):
        print(parser_result.error_message)
        return

    print(parser_result.syntax_tree)

