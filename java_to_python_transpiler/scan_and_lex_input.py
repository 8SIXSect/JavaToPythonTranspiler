"""
This module's purpose is to tokenize user input (some Java source code string)
"""

from dataclasses import dataclass, field
import re
from typing import List, Optional, Union


TokenType = str


# todo: convert these strings into an enumeration
SINGLE_LINE_COMMENT_TOKEN_TYPE: str = "SINGLE_LINE_COMMENT"
LEFT_PARENTHESIS_TOKEN_TYPE: str = "LEFT_PARENTHESIS"
RIGHT_PARENTHESIS_TOKEN_TYPE: str = "RIGHT_PARENTHESIS"
LEFT_CURLY_BRACE_TOKEN_TYPE: str = "LEFT_CURLY_BRACE"
RIGHT_CURLY_BRACE_TOKEN_TYPE: str = "RIGHT_CURLY_BRACE"
LEFT_BRACKET_TOKEN_TYPE: str = "LEFT_BRACKET"
RIGHT_BRACKET_TOKEN_TYPE: str = "RIGHT_BRACKET"
SEMI_COLON_TOKEN_TYPE: str = "SEMI_COLON"
COMMA_TOKEN_TYPE: str = "COMMA"
EQUALS_TOKEN_TYPE: str = "EQUALS"
LESS_THAN_TOKEN_TYPE: str = "LESS_THAN"
GREATER_THAN_TOKEN_TYPE: str = "GREATER_THAN"
PLUS_TOKEN_TYPE: str = "PLUS"
MINUS_TOKEN_TYPE: str = "MINUS"
MULTIPLY_TOKEN_TYPE: str = "MULTIPLY"
DIVIDE_TOKEN_TYPE: str = "DIVIDE"
FLOAT_LITERAL_TOKEN_TYPE: str = "FLOAT_LITERAL"
DECIMAL_LITERAL_TOKEN_TYPE: str = "DECIMAL_LITERAL"
STRING_LITERAL_TOKEN_TYPE: str = "STRING_LITERAL"
WHITESPACE_TOKEN_TYPE: str = "WHITESPACE"
IDENTIFIER_TOKEN_TYPE: str = "IDENTIFIER"


TOKEN_PATTERNS = {
    r"//.*": SINGLE_LINE_COMMENT_TOKEN_TYPE,
    r"\(": LEFT_PARENTHESIS_TOKEN_TYPE,
    r"\)": RIGHT_PARENTHESIS_TOKEN_TYPE,
    r"\{": LEFT_CURLY_BRACE_TOKEN_TYPE,
    r"\}": RIGHT_CURLY_BRACE_TOKEN_TYPE,
    r"\[": LEFT_BRACKET_TOKEN_TYPE,
    r"\]": RIGHT_BRACKET_TOKEN_TYPE,
    r";": SEMI_COLON_TOKEN_TYPE,
    r",": COMMA_TOKEN_TYPE,
    r"=": EQUALS_TOKEN_TYPE,
    r"\<": LESS_THAN_TOKEN_TYPE,
    r"\>": GREATER_THAN_TOKEN_TYPE,
    r"\+": PLUS_TOKEN_TYPE,
    r"-": MINUS_TOKEN_TYPE,
    r"\*": MULTIPLY_TOKEN_TYPE,
    r"\/": DIVIDE_TOKEN_TYPE,
    r"\d+\.\d+": FLOAT_LITERAL_TOKEN_TYPE,
    r"\d+": DECIMAL_LITERAL_TOKEN_TYPE,
    r"\"([^\"\\],|\\[btnfr\"\'\\])*\"": STRING_LITERAL_TOKEN_TYPE,
    r"\s+": WHITESPACE_TOKEN_TYPE,
    r"[a-zA-Z_$][\da-zA-Z_]*": IDENTIFIER_TOKEN_TYPE,
}


_ = {
    "TRUE": r"true",
    "FALSE": r"false",
    "NULL": r"null",
    "PUBLIC": r"public",
    "PRIVATE": r"private",
    "VOID": r"void",
    "STATIC": r"static",
    "BYTE": r"byte",
    "SHORT": r"short",
    "CHAR": r"char",
    "INT": r"int",
    "LONG": r"long",
    "FLOAT": r"float",
    "DOUBLE": r"double",
    "BOOLEAN": r"boolean",
    "CLASS": r"class",
    "RETURN": r"return",
    "NEW": r"new",
    "PACKAGE": r"package",
    "IMPORT": r"import",
    "EXTENDS": r"extends",
    "IF": r"if",
    "ELSE": r"else",
    "WHILE": r"while"
}


@dataclass
class Token:
    """
    This class represents tokens.

    It's two fields are token_type and value which a self-explanatory
    """

    token_type: str
    value: str


@dataclass
class LexerResult:
    """
    This class represents the output of the lexer.
    'tokens' and 'error_message' cannot both have no value.
    By this, I mean that 'tokens' is an empty list, then an error occurred, and
    'error_message' is not an empty string.
    The opposite is also true: if 'tokens' is not an empty list then
    'error_message' is an empty string.
    """

    was_successful: bool
    tokens: List[Token] = field(default_factory=list) 
    error_message: str = ""


def scan_and_tokenize_input(user_input: str) -> LexerResult:
    """ This function's purpose is to be the entrypoint for the lexer. """


    def report_error(unknown_character: str) -> LexerResult:
        """
        This function's purpose is to report an error that occurred in the
        lexer.
        """

        ERROR_MESSAGE = "Found an unknown character, '{0}'"

        error_message: str = ERROR_MESSAGE.format(unknown_character)

        lexer_result = LexerResult(False, error_message=error_message)
        return lexer_result 
    

    tokens: List[Token] = []
    position = 0
    length_of_input: int = len(user_input)
    
    while position < length_of_input:
        match: Union[re.Match[str], None] = None

        # Incase you don't remember hunter, this gives you everything after
        # position
        sliced_input = user_input[position:]
        
        pattern: str

        for pattern in TOKEN_PATTERNS.keys():

            match = re.match(pattern,sliced_input)

            if match is not None:

                token_type: TokenType = TOKEN_PATTERNS[pattern]
                if token_type == WHITESPACE_TOKEN_TYPE:
                    break

                token_value: str = match.group(0)
                token = Token(token_type, token_value)
                
                tokens.append(token)
                break
        
        if match is None:

            unknown_character = user_input[position]
            unsuccessful_result: LexerResult = report_error(unknown_character)
            
            return unsuccessful_result

        position += match.end()

    was_successful = True
    return LexerResult(was_successful, tokens)

