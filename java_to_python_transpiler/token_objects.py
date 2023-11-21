"""
This module contains useful token classes used inside the Lexer
"""

from dataclasses import dataclass
from enum import Enum


class TokenType(Enum):
    """
    This class is an enumeration containing token types
    """

    SINGLE_LINE_COMMENT = r"//.*"
    MULTI_LINE_COMMENT = r"/\*[\s\S]*?\*/"

    TRUE = r"true"
    FALSE = r"false"
    NULL = r"null"
    PUBLIC = r"public"
    PRIVATE = r"private"
    VOID = r"void"
    STATIC = r"static"
    BYTE = r"byte"
    SHORT = r"short"
    CHAR = r"char"
    INT = r"int"
    LONG = r"long"
    FLOAT = r"float"
    DOUBLE = r"double"
    BOOLEAN = r"boolean"
    CLASS = r"class"
    RETURN = r"return"
    NEW = r"new"
    PACKAGE = r"package"
    IMPORT = r"import"
    EXTENDS = r"extends"
    IF = r"if"
    ELSE = r"else"
    WHILE = r"while"

    LEFT_PARENTHESIS = r"\("
    RIGHT_PARENTHESIS = r"\)"
    LEFT_CURLY_BRACE = r"\{"
    RIGHT_CURLY_BRACE = r"\}"
    LEFT_BRACKET = r"\["
    RIGHT_BRACKET = r"\]"
    SEMI_COLON = r";"
    PERIOD = r"\."
    COMMA = r","
    EQUALS = r"="
    LESS_THAN = r"\<"
    GREATER_THAN = r"\>"
    QUESTION = r"\?"
    COLON = r":"
    PLUS = r"\+"
    MINUS = r"-"
    MULTIPLY = r"\*"
    DIVIDE = r"\/"
    MODULO = r"%"

    IDENTIFIER = r"[a-zA-Z_$][\da-zA-Z_]*"
    FLOAT_LITERAL = \
        r"(\d[_\d]*\d|\d)\.(\d[_\d]*\d|\d)?([eE][+-]?(\d[_\d]*\d|\d))?[fFdD]?"
    DECIMAL_LITERAL = r"([1-9]([_\d]*\d+)?|0)[lL]?"
    STRING_LITERAL = r"\"([^\"\\]|\\[btnfr\"\'\\])*\""

    WHITESPACE = r"\s+"


@dataclass
class Token:
    """
    This class represents tokens.

    It's two fields are token_type and value which a self-explanatory
    """

    token_type: TokenType
    value: str

