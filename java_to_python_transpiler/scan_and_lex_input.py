"""
This module's purpose is to tokenize user input (some Java source code string)
"""

import re
from typing import List
from java_to_python_transpiler.token_objects import TokenType, Token


# This string is used in formatting
ERROR_MESSAGE = "Invalid character: {0}"


def tokenize_input_string(input: str) -> List[Token]:
    """
    This function tokenizes java source code

    This function has a singular parameter: input of type string

    This function returns a list tokens

    :return a list of tokens
    """

    token_list: List[Token] = []
    current_index = 0
    length_of_input = len(input)

    while current_index < length_of_input:
        match = None

        # TokenType is an Enum and they are iterable
        for token_type in TokenType:
            
            # TokenType.value stores the regex pattern.
            pattern = re.compile(token_type.value)
            match = pattern.match(input, current_index)

            if match is not None:
                token_value = match.group(0)

                # Whitespace is ignored in Java
                if token_type is not TokenType.WHITESPACE:
                    token = Token(token_type, token_value)
                    token_list.append(token)

                current_index = match.end()
                break

        if match is None:
            input_at_current_index = input[current_index]
            error_message = ERROR_MESSAGE.format(input_at_current_index)
            raise ValueError(error_message)

    return token_list

