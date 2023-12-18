from typing import List
from java_to_python_transpiler.java_to_python import (
    COMMA_TOKEN_TYPE, DECIMAL_LITERAL_TOKEN_TYPE, DIVIDE_TOKEN_TYPE, END_OF_FILE_TOKEN_TYPE, EQUALS_TOKEN_TYPE,
    ERROR_MESSAGE_FOR_LEXER, FLOAT_LITERAL_TOKEN_TYPE, GREATER_THAN_TOKEN_TYPE, LEFT_BRACKET_TOKEN_TYPE,
    LEFT_CURLY_BRACE_TOKEN_TYPE, LEFT_PARENTHESIS_TOKEN_TYPE, LESS_THAN_TOKEN_TYPE, MINUS_TOKEN_TYPE, MULTIPLY_TOKEN_TYPE, PLUS_TOKEN_TYPE,
    RIGHT_BRACKET_TOKEN_TYPE, RIGHT_CURLY_BRACE_TOKEN_TYPE, RIGHT_PARENTHESIS_TOKEN_TYPE,
    SEMI_COLON_TOKEN_TYPE, SINGLE_LINE_COMMENT_TOKEN_TYPE, STRING_LITERAL_TOKEN_TYPE,
    LexerFailure, Token, LexerResult,
    report_error_for_lexer, scan_and_tokenize_input
)


# This token may be reused throughout the tests in this file
end_of_file_token: Token = Token(END_OF_FILE_TOKEN_TYPE, "")


def test_report_error_for_lexer_returns_proper_error_object():
    """
    This test checks if the function `report_error_for_lexer` returns
    the correct LexerFailure object.
    """

    UNKNOWN_CHARACTER: str = "~"

    error_message: str = ERROR_MESSAGE_FOR_LEXER.format(UNKNOWN_CHARACTER)
    expected_output: LexerFailure = LexerFailure(error_message)

    report_error_output: LexerFailure = report_error_for_lexer(UNKNOWN_CHARACTER)

    assert expected_output == report_error_output


def test_lexer_can_generate_token_for_single_line_comment():
    """
    This test checks if the lexer can successfully generate a Token object
    with a token type of SINGLE LINE COMMMENT.
    """

    LEXER_INPUT: str = "// this is in a `comment`"

    expected_output_token: Token = Token(SINGLE_LINE_COMMENT_TOKEN_TYPE, LEXER_INPUT)
    expected_output: List[Token] = [expected_output_token, end_of_file_token] 
 
    lexer_output: LexerResult = scan_and_tokenize_input(LEXER_INPUT)

    assert isinstance(lexer_output, list) and expected_output == lexer_output


def test_lexer_can_generate_token_for_grouping_characters():
    """
    This test checks if the lexer can successfully generate a Token objects
    for parenthesis, curly braces, and brackets
    """

    LEXER_INPUT: str = "(){}[]"

    left_parenthesis_token: Token = Token(LEFT_PARENTHESIS_TOKEN_TYPE, "(")
    right_parenthesis_token: Token = Token(RIGHT_PARENTHESIS_TOKEN_TYPE, ")")

    left_curly_braces_token: Token = Token(LEFT_CURLY_BRACE_TOKEN_TYPE, "{")
    right_curly_braces_token: Token = Token(RIGHT_CURLY_BRACE_TOKEN_TYPE, "}")

    left_bracket_token: Token = Token(LEFT_BRACKET_TOKEN_TYPE, "[")
    right_bracket_token: Token = Token(RIGHT_BRACKET_TOKEN_TYPE, "]")

    expected_output: List[Token] = [
        left_parenthesis_token, right_parenthesis_token,
        left_curly_braces_token, right_curly_braces_token,
        left_bracket_token, right_bracket_token,
        end_of_file_token
    ]

    lexer_output: LexerResult = scan_and_tokenize_input(LEXER_INPUT)

    assert isinstance(lexer_output, list) and expected_output == lexer_output


def test_lexer_can_generate_token_for_punctuation_characters():
    """
    This test checks if the lexer can successfully generate Token objects for
    semicolons and commas.
    """

    LEXER_INPUT: str = ";,,;"

    semi_colon_token: Token = Token(SEMI_COLON_TOKEN_TYPE, ";")
    comma_token: Token = Token(COMMA_TOKEN_TYPE, ",")

    expected_output: List[Token] = [
        semi_colon_token, comma_token, comma_token, semi_colon_token,
        end_of_file_token
    ]

    lexer_output: LexerResult = scan_and_tokenize_input(LEXER_INPUT)

    assert isinstance(lexer_output, list) and expected_output == lexer_output 


def test_lexer_can_generate_token_for_comparison_operators():
    """
    This test checks if the lexer can successfully generate Token objects for
    less than, greater than, and the equals sign
    """

    LEXER_INPUT: str = "=>=<="

    less_than_token: Token = Token(LESS_THAN_TOKEN_TYPE, "<")
    greater_than_token: Token = Token(GREATER_THAN_TOKEN_TYPE, ">")
    equals_token: Token = Token(EQUALS_TOKEN_TYPE, "=")

    expected_output: List[Token] = [
        equals_token, greater_than_token, equals_token, less_than_token,
        equals_token,
        end_of_file_token
    ]

    lexer_output: LexerResult = scan_and_tokenize_input(LEXER_INPUT)

    assert isinstance(lexer_output, list) and expected_output == lexer_output 


def test_lexer_can_generate_token_for_arithmetic_operators():
    """
    This test checks if the lexer can successfully generate Token objects for
    plus, minus, multiply, and divide
    """

    LEXER_INPUT: str = "++/*-/"

    plus_token: Token = Token(PLUS_TOKEN_TYPE, "+")
    minus_token: Token = Token(MINUS_TOKEN_TYPE, "-")
    multiply_token: Token = Token(MULTIPLY_TOKEN_TYPE, "*")
    divide_token: Token = Token(DIVIDE_TOKEN_TYPE, "/")

    expected_output: List[Token] = [
        plus_token, plus_token, divide_token, multiply_token, minus_token,
        divide_token,
        end_of_file_token
    ]

    lexer_output: LexerResult = scan_and_tokenize_input(LEXER_INPUT)

    assert isinstance(lexer_output, list) and expected_output == lexer_output


def test_lexer_can_generate_token_for_float_literals():
    """
    This test checks if the lexer can successfully generate Token objects for
    float literals.
    """

    LEXER_INPUT: str = "86.86"

    float_literal_token: Token = Token(FLOAT_LITERAL_TOKEN_TYPE, LEXER_INPUT)
    expected_output: List[Token] = [float_literal_token, end_of_file_token]

    lexer_output: LexerResult = scan_and_tokenize_input(LEXER_INPUT)

    assert isinstance(lexer_output, list) and expected_output == lexer_output


def test_lexer_can_generate_token_for_decimal_literals():
    """
    This test checks if the lexer can successfully generate Token objects for
    decimal literals.
    """

    LEXER_INPUT: str = "86868686"

    decimal_literal_token: Token = Token(DECIMAL_LITERAL_TOKEN_TYPE, LEXER_INPUT)
    expected_output: List[Token] = [decimal_literal_token, end_of_file_token]

    lexer_output: LexerResult = scan_and_tokenize_input(LEXER_INPUT)

    assert isinstance(lexer_output, list) and expected_output == lexer_output


# TODO: rename tests. every single test generates a token list. not a singular token
def test_lexer_can_generate_token_for_string_literals():
    """
    This test checks if the lexer can successfully generate Tokeno objects for
    string literals.
    """

    LEXER_INPUT: str = '"hello"'

    first_string_literal_token: Token = Token(STRING_LITERAL_TOKEN_TYPE, '"hello"')

    expected_output: List[Token] = [
        first_string_literal_token,
        end_of_file_token
    ]

    lexer_output: LexerResult = scan_and_tokenize_input(LEXER_INPUT)

    assert isinstance(lexer_output, list) and expected_output == lexer_output

