"""
This module's purpose is to tokenize user input (some Java source code string)
"""

import re
from typing import List
from java_to_python_transpiler.token_classes import TokenType, Token


# This string is used in formatting
ERROR_MESSAGE = "Invalid character: {0}"


def tokenize_code(java_source: str) -> List[Token]:
    """
    This functions tokenizes java source code

    This function has a singular parameter: java_source of type string

    This function returns a list tokens

    :return a list of tokens
    """

    token_list = []
    current_index = 0

    while current_index < len(java_source):
        regex_match = None

        # TokenType is an Enum and they are iterable
        for token_type in TokenType:
            regex_pattern = re.compile(token_type.value)
            regex_match = regex_pattern.match(java_source, current_index)

            if regex_match is not None:
                token_value = regex_match.group(0)

                # Whitespace is ignored in Java
                if token_type is not TokenType.WHITESPACE:
                    token = Token(token_type, token_value)
                    token_list.append(token)

                current_index = regex_match.end()
                break

        if regex_match is None:
            raise ValueError(ERROR_MESSAGE.format(java_source[current_index]))

    return token_list
