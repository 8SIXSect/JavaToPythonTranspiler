"""
This module's purpose is to tokenize user input (some Java source code string)
"""

from dataclasses import dataclass
import re
from typing import List, Optional
from java_to_python_transpiler.token_objects import TokenType, Token


def tokenize_input_string(input: str) -> List[Token]:
    """
    This function tokenizes java source code

    This function has a singular parameter: input of type string

    This function returns a list tokens

    :return a list of tokens
    """

    @dataclass
    class LexerResult:
        """ This dataclass provides information from the Lexer """
        
        was_successful: bool
        match_object: Optional[re.Match[str]]
        token: Optional[Token]

    tokens: List[Token] = []
    position = 0

    while position < len(input):

        for token_type_key in TokenType.keys():
            print(token_type_key)
        break


