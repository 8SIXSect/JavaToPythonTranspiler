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

TRUE_TOKEN_TYPE: str = r"TRUE"
FALSE_TOKEN_TYPE: str = r"FALSE"
NULL_TOKEN_TYPE: str = r"NULL"
PUBLIC_TOKEN_TYPE: str = r"PUBLIC"
PRIVATE_TOKEN_TYPE: str = r"PRIVATE"
VOID_TOKEN_TYPE: str = r"VOID"
STATIC_TOKEN_TYPE: str = r"STATIC"
BYTE_TOKEN_TYPE: str = r"BYTE"
SHORT_TOKEN_TYPE: str = r"SHORT"
CHAR_TOKEN_TYPE: str = r"CHAR"
INT_TOKEN_TYPE: str = r"INT"
LONG_TOKEN_TYPE: str = r"LONG"
FLOAT_TOKEN_TYPE: str = r"FLOAT"
DOUBLE_TOKEN_TYPE: str = r"DOUBLE"
BOOLEAN_TOKEN_TYPE: str = r"BOOLEAN"
CLASS_TOKEN_TYPE: str = r"CLASS"
RETURN_TOKEN_TYPE: str = r"RETURN"
NEW_TOKEN_TYPE: str = r"NEW"
PACKAGE_TOKEN_TYPE: str = r"PACKAGE"
IMPORT_TOKEN_TYPE: str = r"IMPORT"
IF_TOKEN_TYPE: str = r"IF"
ELSE_TOKEN_TYPE: str = r"ELSE"
WHILE_TOKEN_TYPE: str = r"WHILE"


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


    def change_identifier_to_keyword(token: Token) -> Token:
        """
        The purpose of this function is to take in a Token.

        If the Token is not an identifier then the Token is returned as it

        If the Token is of type identifier, it will check if that identifier is
        a keyword. If it is a keyword, then it will create a new keyword Token
        """
        
        KEYWORDS = {
            r"true": TRUE_TOKEN_TYPE,
            r"false": FALSE_TOKEN_TYPE,
            r"null": NULL_TOKEN_TYPE,
            r"public": PUBLIC_TOKEN_TYPE,
            r"private": PRIVATE_TOKEN_TYPE,
            r"void": VOID_TOKEN_TYPE,
            r"static": STATIC_TOKEN_TYPE,
            r"byte": BYTE_TOKEN_TYPE,
            r"short": SHORT_TOKEN_TYPE,
            r"char": CHAR_TOKEN_TYPE,
            r"int": INT_TOKEN_TYPE,
            r"long": LONG_TOKEN_TYPE,
            r"float": FLOAT_TOKEN_TYPE,
            r"double": DOUBLE_TOKEN_TYPE,
            r"boolean": BOOLEAN_TOKEN_TYPE,
            r"class": CLASS_TOKEN_TYPE,
            r"return": RETURN_TOKEN_TYPE,
            r"new": NEW_TOKEN_TYPE,
            r"package": PACKAGE_TOKEN_TYPE,
            r"import": IMPORT_TOKEN_TYPE,
            r"if": IF_TOKEN_TYPE,
            r"else": ELSE_TOKEN_TYPE,
            r"while": WHILE_TOKEN_TYPE,
        }
        
        if token.token_type != IDENTIFIER_TOKEN_TYPE:
            return token

        if token.value not in KEYWORDS.keys():
            return token

        keyword_token_type: str = KEYWORDS[token.value]

        # The value is kind of redundant
        keyword_value: str = token.value
        keyword_token: Token = Token(keyword_token_type, keyword_value)

        return keyword_token


    tokens_with_keywords: map[Token] = map(change_identifier_to_keyword, tokens)
    tokens_with_keywords_as_list: List[Token] = list(tokens_with_keywords)


    was_successful = True
    return LexerResult(was_successful, tokens_with_keywords_as_list)


