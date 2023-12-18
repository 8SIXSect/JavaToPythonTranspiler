"""
This module contains methods for transpiling source to source
"""

from typing import List, Union
from java_to_python_transpiler.java_to_python import (ExpressionNode,
                                                      ParserFailure,
                                                      Token,
                                                      LexerFailure,
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

    parser_result: Union[ExpressionNode, ParserFailure] = \
            parse_list_of_tokens(lexer_result)

    if isinstance(parser_result, ParserFailure):
        print(parser_result.error_messasge)
        return

    print(parser_result)

