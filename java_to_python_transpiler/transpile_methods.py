"""
This module contains methods for transpiling source to source
"""

from typing import List, Union
from java_to_python_transpiler.java_to_python import (Token,
                                                      LexerFailure,
                                                      ParserResult,
                                                      scan_and_tokenize_input,
                                                      parse_list_of_tokens
                                                      )


PROMPT = ">>> "


def test_parser(): 
    user_input = "86"

    lexer_result: Union[List[Token], LexerFailure] = \
            scan_and_tokenize_input(user_input)

    if isinstance(lexer_result, LexerFailure):
        print(lexer_result.error_message)
        return

    assert isinstance(lexer_result, list)

    parser_result: ParserResult = parse_list_of_tokens(lexer_result)

    if (not parser_result.was_successful) or (parser_result.syntax_tree is None):
        print(parser_result.error_message)
        return

    print(parser_result.syntax_tree)

