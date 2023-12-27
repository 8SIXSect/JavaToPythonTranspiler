import random
from typing import List, Tuple

from java_to_python_transpiler.java_to_python import (
    CHAR_TOKEN_TYPE, COMMA_TOKEN_TYPE, DECIMAL_LITERAL_TOKEN_TYPE, DIVIDE_TOKEN_TYPE, DOUBLE_TOKEN_TYPE,
    END_OF_FILE_TOKEN_TYPE, EQUALS_TOKEN_TYPE, ERROR_MESSAGE_FOR_LEXER, ERROR_MESSAGE_FOR_PARSER,
    FLOAT_LITERAL_TOKEN_TYPE, GREATER_THAN_TOKEN_TYPE, IDENTIFIER_TOKEN_TYPE, INT_TOKEN_TYPE,
    LEFT_BRACKET_TOKEN_TYPE, LEFT_CURLY_BRACE_TOKEN_TYPE,
    LEFT_PARENTHESIS_TOKEN_TYPE, LESS_THAN_TOKEN_TYPE, LONG_TOKEN_TYPE, MINUS_TOKEN_TYPE,
    MULTIPLY_TOKEN_TYPE, PLUS_TOKEN_TYPE, RETURN_TOKEN_TYPE, RIGHT_BRACKET_TOKEN_TYPE,
    RIGHT_CURLY_BRACE_TOKEN_TYPE, RIGHT_PARENTHESIS_TOKEN_TYPE,
    SEMI_COLON_TOKEN_TYPE, SHORT_TOKEN_TYPE, SINGLE_LINE_COMMENT_TOKEN_TYPE,
    STRING_LITERAL_TOKEN_TYPE, TRUE_TOKEN_TYPE, WHILE_TOKEN_TYPE, ArgumentList, ArithmeticOperator,
    ExpressionNode, FactorNode, InlineStatement, LexerFailure, MethodCall, NodeFailure, NodeResult, NodeSuccess,
    ParserFailure, ReturnStatement, TermNode, Token, LexerResult, VariableIncrement, VariableInitialization, parse_tokens,
    parse_tokens_for_argument_list, parse_tokens_for_expression, parse_tokens_for_factor, parse_tokens_for_inline_statement,
    parse_tokens_for_method_call, parse_tokens_for_return_statement, parse_tokens_for_term, parse_tokens_for_variable_increment, parse_tokens_for_variable_initialization,
    report_error_for_lexer, scan_and_tokenize_input, ParserResult
)


# These tokens may be reused throughout the tests in this file
end_of_file_token = Token(END_OF_FILE_TOKEN_TYPE, "")
plus_token = Token(PLUS_TOKEN_TYPE, "+")
minus_token = Token(MINUS_TOKEN_TYPE, "-")
multiply_token = Token(MULTIPLY_TOKEN_TYPE, "*")
divide_token = Token(DIVIDE_TOKEN_TYPE, "/")

left_parenthesis_token = Token(LEFT_PARENTHESIS_TOKEN_TYPE, "(")
right_parenthesis_token = Token(RIGHT_PARENTHESIS_TOKEN_TYPE, ")")
comma_token = Token(COMMA_TOKEN_TYPE, ",")
semi_colon_token = Token(SEMI_COLON_TOKEN_TYPE, ";")
equals_token = Token(EQUALS_TOKEN_TYPE, "equals")


def test_report_error_for_lexer_returns_proper_error_object():
    """
    This test checks if the function `report_error_for_lexer` returns
    the correct LexerFailure object.
    """

    UNKNOWN_CHARACTER = "~"

    error_message: str = ERROR_MESSAGE_FOR_LEXER.format(UNKNOWN_CHARACTER)
    expected_output = LexerFailure(error_message)

    report_error_output: LexerFailure = report_error_for_lexer(UNKNOWN_CHARACTER)

    assert expected_output == report_error_output


def test_lexer_can_generate_tokens_for_single_line_comment():
    """
    This test checks if the lexer can successfully generate a Token object
    with a token type of SINGLE LINE COMMMENT.
    """

    INPUT = "// this is in a `comment`"

    expected_output_token = Token(SINGLE_LINE_COMMENT_TOKEN_TYPE, INPUT)
    expected_output: Tuple[Token, Token] = (expected_output_token, end_of_file_token)
 
    lexer_result: LexerResult = scan_and_tokenize_input(INPUT)

    assert expected_output == lexer_result


def test_lexer_can_generate_token_for_grouping_characters():
    """
    This test checks if the lexer can successfully generate a Token objects
    for parenthesis, curly braces, and brackets
    """

    INPUT = "(){}[]"

    left_parenthesis_token = Token(LEFT_PARENTHESIS_TOKEN_TYPE, "(")
    right_parenthesis_token = Token(RIGHT_PARENTHESIS_TOKEN_TYPE, ")")

    left_curly_braces_token = Token(LEFT_CURLY_BRACE_TOKEN_TYPE, "{")
    right_curly_braces_token = Token(RIGHT_CURLY_BRACE_TOKEN_TYPE, "}")

    left_bracket_token = Token(LEFT_BRACKET_TOKEN_TYPE, "[")
    right_bracket_token = Token(RIGHT_BRACKET_TOKEN_TYPE, "]")

    expected_output: Tuple[Token, ...] = (
        left_parenthesis_token, right_parenthesis_token,
        left_curly_braces_token, right_curly_braces_token,
        left_bracket_token, right_bracket_token,
        end_of_file_token
    )

    lexer_result: LexerResult = scan_and_tokenize_input(INPUT)

    assert expected_output == lexer_result


def test_lexer_can_generate_tokens_for_punctuation_characters():
    """
    This test checks if the lexer can successfully generate Token objects for
    semicolons and commas.
    """

    INPUT = ";,,;"

    semi_colon_token = Token(SEMI_COLON_TOKEN_TYPE, ";")
    comma_token = Token(COMMA_TOKEN_TYPE, ",")

    expected_output: Tuple[Token, ...] = (
        semi_colon_token, comma_token, comma_token, semi_colon_token,
        end_of_file_token
    )

    lexer_result: LexerResult = scan_and_tokenize_input(INPUT)

    assert expected_output == lexer_result 


def test_lexer_can_generate_tokens_for_comparison_operators():
    """
    This test checks if the lexer can successfully generate Token objects for
    less than, greater than, and the equals sign
    """

    LEXER_INPUT = "=>=<="

    less_than_token = Token(LESS_THAN_TOKEN_TYPE, "<")
    greater_than_token = Token(GREATER_THAN_TOKEN_TYPE, ">")
    equals_token = Token(EQUALS_TOKEN_TYPE, "=")

    expected_output: Tuple[Token, ...] = (
        equals_token, greater_than_token, equals_token, less_than_token,
        equals_token,
        end_of_file_token
    )

    lexer_result: LexerResult = scan_and_tokenize_input(LEXER_INPUT)

    assert expected_output == lexer_result 


def test_lexer_can_generate_tokens_for_arithmetic_operators():
    """
    This test checks if the lexer can successfully generate Token objects for
    plus, minus, multiply, and divide
    """

    INPUT = "++/*-/"

    plus_token = Token(PLUS_TOKEN_TYPE, "+")
    minus_token = Token(MINUS_TOKEN_TYPE, "-")
    multiply_token = Token(MULTIPLY_TOKEN_TYPE, "*")
    divide_token = Token(DIVIDE_TOKEN_TYPE, "/")

    expected_output: Tuple[Token, ...] = (
        plus_token, plus_token, divide_token, multiply_token, minus_token,
        divide_token,
        end_of_file_token
        )

    lexer_result: LexerResult = scan_and_tokenize_input(INPUT)

    assert expected_output == lexer_result


def test_lexer_can_generate_tokens_for_float_literals():
    """
    This test checks if the lexer can successfully generate Token objects for
    float literals.
    """

    INPUT = "86.86"

    float_literal_token = Token(FLOAT_LITERAL_TOKEN_TYPE, INPUT)
    expected_output: Tuple[Token, Token] = (float_literal_token, end_of_file_token)

    lexer_result: LexerResult = scan_and_tokenize_input(INPUT)
    
    assert expected_output == lexer_result


def test_lexer_can_generate_tokens_for_decimal_literals():
    """
    This test checks if the lexer can successfully generate Token objects for
    decimal literals.
    """

    INPUT = "86868686"

    decimal_literal_token = Token(DECIMAL_LITERAL_TOKEN_TYPE, INPUT)
    expected_output: Tuple[Token, Token] = (decimal_literal_token, end_of_file_token)

    lexer_result: LexerResult = scan_and_tokenize_input(INPUT)

    assert expected_output == lexer_result


def test_lexer_can_generate_tokens_for_string_literals():
    """
    This test checks if the lexer can successfully generate Tokens objects for
    string literals.
    """

    INPUT = '"hello"'

    first_string_literal_token = Token(STRING_LITERAL_TOKEN_TYPE, '"hello"')

    expected_output: Tuple[Token, Token] = (
        first_string_literal_token,
        end_of_file_token
    )

    lexer_result: LexerResult = scan_and_tokenize_input(INPUT)

    assert expected_output == lexer_result


def test_lexer_can_generate_tokens_for_identifiers():
    """
    This test checks if the lexer can successfully generate Token objects for
    identifiers.
    """

    INPUT = "eighty sixth sector"

    first_identifier_token = Token(IDENTIFIER_TOKEN_TYPE, "eighty")
    second_identifier_token = Token(IDENTIFIER_TOKEN_TYPE, "sixth")
    third_identifier_token = Token(IDENTIFIER_TOKEN_TYPE, "sector")

    expected_output: Tuple[Token, ...] = (
        first_identifier_token, second_identifier_token, third_identifier_token,
        end_of_file_token
    )

    lexer_result: LexerResult = scan_and_tokenize_input(INPUT)

    assert expected_output == lexer_result


def test_lexer_can_generate_only_end_of_file_token_when_given_no_input():
    """
    This test checks if the lexer can successfully generate a list of one
    Token (an end of file token) when given no input
    """

    INPUT = ""

    expected_output: Tuple[Token] = (end_of_file_token,)

    lexer_result: LexerResult = scan_and_tokenize_input(INPUT)
    
    assert expected_output == lexer_result


def test_lexer_can_generate_only_end_of_file_token_when_given_only_whitespace():
    """
    This test checks if the lexer can successfully generate a list of one
    Token (an end of file token) when given only whitespace in the input
    """

    INPUT = "               \t     \n \n   "

    expected_output: Tuple[Token] = (end_of_file_token,)

    lexer_result: LexerResult = scan_and_tokenize_input(INPUT)

    assert expected_output == lexer_result


def test_lexer_can_generate_proper_keyword_tokens():
    """
    This test checks if the lexer can successfully generate a list of Tokens
    with keywords when given keywords. Pretty much, if given the input: "true",
    it will return a Token(true, "true") object and not a
    Token(identifier, "true")
    """

    INPUT = "true short while"

    true_token = Token(TRUE_TOKEN_TYPE, "true")
    short_token = Token(SHORT_TOKEN_TYPE, "short")
    while_token = Token(WHILE_TOKEN_TYPE, "while")

    expected_output: Tuple[Token, ...] = (
        true_token, short_token, while_token,
        end_of_file_token
    )

    lexer_result: LexerResult = scan_and_tokenize_input(INPUT)

    assert expected_output == lexer_result


def generate_number_token_with_random_value() -> Token:
    """
    This is a helper function that generates a Token object with a random int
    value
    """

    random_number: int = random.randint(0, 100)
    token_value = str(random_number)

    return Token(DECIMAL_LITERAL_TOKEN_TYPE, token_value)


def test_parser_can_generate_correct_ast_for_argument_list_with_no_arguments():
    """
    This test checks that the parser can correctly generate an ArgumentList object
    using the `parse_tokens_for_argument_list` function when given an input
    without an arguments supplied.
    """

    tokens: Tuple[Token, Token] = (right_parenthesis_token, end_of_file_token)

    argument_list = ArgumentList() 
    expected_output = NodeSuccess(tokens, argument_list)

    node_result: NodeResult = parse_tokens_for_argument_list(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_single_argument_list():
    """
    This test checks that the parser can correctly generate an ArgumentList object
    using the `parse_tokens_for_argument_list` function when given an input
    of a singular argument.
    """

    decimal_literal_token: Token = generate_number_token_with_random_value()

    tokens: Tuple[Token, Token, Token] = (
        decimal_literal_token,
        right_parenthesis_token,
        end_of_file_token
    )

    factor = FactorNode(decimal_literal_token.value)
    term = TermNode(factor)
    expression = ExpressionNode(term)

    argument_list = ArgumentList(expression)
    
    expected_output_tokens: Tuple[Token, Token] = (
        right_parenthesis_token, end_of_file_token
    )
    expected_output = NodeSuccess(expected_output_tokens, argument_list)

    node_result: NodeResult = parse_tokens_for_argument_list(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_multiple_argument_list():
    """
    This test checks that the parser can correctly generate an ArgumentList object
    using the `parse_tokens_for_argument_list` function when given an input
    of multiple arguments.
    """

    decimal_literal_token: Token = generate_number_token_with_random_value()

    tokens: Tuple[Token, ...] = (
        decimal_literal_token, comma_token, decimal_literal_token,
        right_parenthesis_token,
        end_of_file_token
    )

    factor = FactorNode(decimal_literal_token.value)
    term = TermNode(factor)
    expression = ExpressionNode(term)

    additional_argument_list = ArgumentList(expression)
    argument_list = ArgumentList(expression, additional_argument_list)
    
    expected_output_tokens: Tuple[Token, Token] = (
        right_parenthesis_token, end_of_file_token
    )
    expected_output = NodeSuccess(expected_output_tokens, argument_list)

    node_result: NodeResult = parse_tokens_for_argument_list(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_error_when_argument_list_expects_parenthesis_or_comma():
    """
    This test checks that the parser can correctly generate a NodeFailure object
    when supplied a faulty input that omits either a comma or a parenthesis.
    """

    decimal_literal_token: Token = generate_number_token_with_random_value()

    tokens: Tuple[Token, ...] = (
        decimal_literal_token, comma_token, decimal_literal_token,
        decimal_literal_token, right_parenthesis_token,
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(DECIMAL_LITERAL_TOKEN_TYPE)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_argument_list(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_no_argument_method_call():
    """
    This test checks that the parser can correctly generate a MethodCall object
    using the `parse_tokens_for_method_call` function.
    """
 
    identifier_token = Token(IDENTIFIER_TOKEN_TYPE, "func")

    tokens: Tuple[Token, Token, Token, Token] = (
        identifier_token, left_parenthesis_token, right_parenthesis_token,
        end_of_file_token
    )

    argument_list = ArgumentList()
    method_call = MethodCall(identifier_token.value, argument_list)

    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, method_call)

    node_result: NodeResult = parse_tokens_for_method_call(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_error_for_method_call_when_expecting_parenthesis():
    """
    This test checks if the function `parse_tokens_for_method_call` returns the
    correct NodeFailure object when given an input that has a syntax error.
    """

    identifier_token = Token(IDENTIFIER_TOKEN_TYPE, "hlelo")

    tokens: Tuple[Token, Token, Token, Token] = (
        identifier_token, left_parenthesis_token, plus_token,
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(PLUS_TOKEN_TYPE)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_method_call(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_single_argument_method_call():
    """
    This test checks if the function `parse_tokens_for_method_call` returns the
    correct MethodCall object when given a single argument.
    """

    identifier_token = Token(IDENTIFIER_TOKEN_TYPE, "raiden")
    decimal_literal_token: Token = generate_number_token_with_random_value()

    tokens: Tuple[Token, Token, Token, Token, Token] = (
        identifier_token, left_parenthesis_token, decimal_literal_token,
        right_parenthesis_token,
        end_of_file_token
    )

    factor = FactorNode(decimal_literal_token.value)
    term = TermNode(factor)
    expression = ExpressionNode(term)

    argument_list = ArgumentList(expression)
    method_call = MethodCall(identifier_token.value, argument_list)

    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, method_call)

    node_result: NodeResult = parse_tokens_for_method_call(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_multiple_arguments_for_method_call():
    """
    This test checks if the function `parse_tokens_for_method_call` returns the
    correct MethodCall object when given a tuple of multiple arguments as input.
    """

    identifier_token = Token(IDENTIFIER_TOKEN_TYPE, "raiden")
    decimal_literal_token: Token = generate_number_token_with_random_value()

    tokens: Tuple[Token, ...] = (
        identifier_token, left_parenthesis_token, decimal_literal_token,
        comma_token, decimal_literal_token, right_parenthesis_token,
        end_of_file_token
    )

    factor = FactorNode(decimal_literal_token.value)
    term = TermNode(factor)
    expression = ExpressionNode(term)

    additional_argument_list = ArgumentList(expression)
    argument_list = ArgumentList(expression, additional_argument_list)
    method_call = MethodCall(identifier_token.value, argument_list)

    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, method_call)

    node_result: NodeResult = parse_tokens_for_method_call(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_single_factor():
    """
    This test checks if the function `parse_tokens_for_factor` returns the
    correct FactorNode object.
    """

    decimal_literal_token: Token = generate_number_token_with_random_value()

    tokens: Tuple[Token, Token] = (decimal_literal_token, end_of_file_token)

    factor_node = FactorNode(decimal_literal_token.value)
    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens,
                                               factor_node)

    node_output: NodeResult = parse_tokens_for_factor(tokens)

    assert expected_output == node_output 


def test_parser_can_generate_correct_error_for_factor():
    """
    This test checks if the function `parse_tokens_for_factor` returns the
    correct NodeFailure object.
    """


    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(PLUS_TOKEN_TYPE)
    expected_output = NodeFailure(error_message)

    tokens: Tuple[Token, Token] = (plus_token, end_of_file_token)

    node_result: NodeResult = parse_tokens_for_factor(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_simple_term():
    """
    This test checks if the parser can successfully generate an ast when given
    a single term. This test specifically checks the `parse_tokens_for_term`
    function.
    """


    decimal_literal_token: Token = generate_number_token_with_random_value()
    tokens: Tuple[Token, Token] = (decimal_literal_token, end_of_file_token)

    factor_node = FactorNode(decimal_literal_token.value)
    term_node = TermNode(factor_node)

    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, term_node)

    node_output: NodeResult = parse_tokens_for_term(tokens)

    assert expected_output == node_output 


def test_parser_can_generate_correct_ast_for_multiply_term():
    """
    This test checks if the parser can successfully generate an ast when given
    a term like "86*2" or "12*9"; this test specifically checks the
    `parse_tokens_for_term` function, 
    """

    first_decimal_literal_token: Token = generate_number_token_with_random_value()
    second_decimal_literal_token: Token = generate_number_token_with_random_value() 

    tokens: Tuple[Token, Token, Token, Token] = (
        first_decimal_literal_token, multiply_token, second_decimal_literal_token,
        end_of_file_token
    )

    single_factor = FactorNode(first_decimal_literal_token.value)
    
    factor_for_additional_term = FactorNode(second_decimal_literal_token.value)
    additional_term = TermNode(factor_for_additional_term)

    term_node = TermNode(single_factor, ArithmeticOperator.MULTIPLY, additional_term)

    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, term_node)

    node_output: NodeResult = parse_tokens_for_term(tokens)

    assert expected_output == node_output


def test_parser_can_generate_correct_ast_for_divide_term():
    """
    This test checks if the parser can successfully generate an ast when given
    a term like "86/2" or "12/9"; this test specifically checks the
    `parse_tokens_for_term` function, 
    """

    first_decimal_literal_token: Token = generate_number_token_with_random_value() 
    second_decimal_literal_token: Token = generate_number_token_with_random_value() 

    tokens: Tuple[Token, Token, Token, Token] = (
        first_decimal_literal_token, divide_token, second_decimal_literal_token,
        end_of_file_token
    )

    single_factor = FactorNode(first_decimal_literal_token.value)
    
    factor_for_additional_term = FactorNode(second_decimal_literal_token.value)
    additional_term = TermNode(factor_for_additional_term)

    term_node = TermNode(single_factor, ArithmeticOperator.DIVIDE,
                                   additional_term)

    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, term_node)

    node_output: NodeResult = parse_tokens_for_term(tokens)

    assert expected_output == node_output


def test_parser_can_generate_correct_error_for_complex_term():
    """
    This test checks if the parser can successfully generate an error when given
    a syntax error kind of term like "5//" or "5*"
    """

    first_decimal_literal_token: Token = generate_number_token_with_random_value()

    tokens: Tuple[Token, Token, Token, Token] = (
        first_decimal_literal_token, divide_token, divide_token,
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(DIVIDE_TOKEN_TYPE)
    expected_output = NodeFailure(error_message)

    node_output: NodeResult = parse_tokens_for_term(tokens)

    assert expected_output == node_output 


def test_parser_can_generate_correct_ast_for_multiple_terms():
    """
    This test checks that the parser can properly generate an AST for multiple
    terms at once
    """
 
    decimal_literal_token: Token = generate_number_token_with_random_value() 

    tokens: Tuple[Token, ...] = (
        decimal_literal_token, multiply_token, decimal_literal_token,
        divide_token, decimal_literal_token,
        end_of_file_token
    )

    first_factor = FactorNode(decimal_literal_token.value)
    second_factor = FactorNode(decimal_literal_token.value)
    third_factor = FactorNode(decimal_literal_token.value)
    
    additional_term_of_additional_term = TermNode(third_factor)
    additional_term = TermNode(second_factor, ArithmeticOperator.DIVIDE,
                               additional_term_of_additional_term)
    
    term = TermNode(first_factor, ArithmeticOperator.MULTIPLY,
                              additional_term)


    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, term)

    node_result: NodeResult = parse_tokens_for_term(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_simple_expression():
    """
    This test checks if the parser can properly generate an AST for a simple
    expression with a singular term and that term has a singular factor.
    """

    decimal_literal_token: Token = generate_number_token_with_random_value() 
    
    tokens: Tuple[Token, Token] = (decimal_literal_token, end_of_file_token)

    factor_node = FactorNode(decimal_literal_token.value)
    term_node = TermNode(factor_node)
    expression_node = ExpressionNode(term_node)

    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, expression_node)

    node_result: NodeResult = parse_tokens_for_expression(tokens)

    assert expected_output == node_result 


def test_parser_can_generate_correct_ast_for_expression_with_one_term():
    """
    This test checks if the parser can properly generate an AST for a simple
    expression with a singular term but multiple factors
    """

    decimal_literal_token: Token = generate_number_token_with_random_value()

    tokens: Tuple[Token, Token, Token, Token] = (
        decimal_literal_token, multiply_token, decimal_literal_token,
        end_of_file_token
    )

    single_factor = FactorNode(decimal_literal_token.value)
    multiply_operator = ArithmeticOperator.MULTIPLY
    
    factor_for_additional_term = FactorNode(decimal_literal_token.value)
    additional_term = TermNode(factor_for_additional_term)

    term_node = TermNode(single_factor, multiply_operator, additional_term)

    expression_node = ExpressionNode(term_node)
    
    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, expression_node)

    node_result: NodeResult = parse_tokens_for_expression(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_complex_expression():
    """
    This test checks that the parser can correctly generate an AST for a
    complex expression like "5+5" or "3-3"
    """

    decimal_literal_token: Token = generate_number_token_with_random_value()

    tokens: Tuple[Token, Token, Token, Token] = (
        decimal_literal_token, plus_token, decimal_literal_token,
        end_of_file_token
    )

    factor = FactorNode(decimal_literal_token.value)
    term = TermNode(factor)
    additional_expression = ExpressionNode(term)

    expression = ExpressionNode(term, ArithmeticOperator.PLUS, additional_expression)

    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, expression)

    node_result: NodeResult = parse_tokens_for_expression(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_error_for_complex_expression():
    """
    This test checks that the parser can correctly generate an error for a
    complex expression with an error.
    """

    decimal_literal_token: Token = generate_number_token_with_random_value()
    tokens: Tuple[Token, Token, Token, Token] = (
        decimal_literal_token, plus_token, minus_token,
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(MINUS_TOKEN_TYPE)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_expression(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_return_statement_with_semicolon():
    """
    This checks if the function `parse_tokens_for_inline_statement` can correctly
    parse the tokens and construct a ReturnStatement object given an input that
    ends in a semicolon.

    Note: I don't think it's necessary to test both an empty and non-empty
    return statements because there are already tests for that. All this test
    cares about is if you have a semicolon after it.
    """

    return_token = Token(RETURN_TOKEN_TYPE, "RETURN")
    tokens: Tuple[Token, Token, Token] = (
        return_token, semi_colon_token,
        end_of_file_token
    )

    return_statement = ReturnStatement()
    inline_statement = InlineStatement(return_statement)
    
    expected_output_tokens: Tuple[Token] = tokens[2:]
    expected_output = NodeSuccess(expected_output_tokens, inline_statement)

    node_result: NodeResult = parse_tokens_for_inline_statement(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_initialization_with_semicolon():
    """
    This test checks that the parser can generate an ast for an InlineStatement
    with a VariableInitialization object. Pretty much, it makes sure that you
    can get this to work: "int x = 86;" (with the semicolon).
    """

    variable_type_token = Token(CHAR_TOKEN_TYPE, "CHAR")
    identifier_token = Token(IDENTIFIER_TOKEN_TYPE, "yolo")
    decimal_literal_token: Token = generate_number_token_with_random_value()

    tokens: Tuple[Token, ...] = (
        variable_type_token, identifier_token, equals_token,
        decimal_literal_token, semi_colon_token,
        end_of_file_token
    )

    factor = FactorNode(decimal_literal_token.value)
    term = TermNode(factor)
    expression = ExpressionNode(term)

    variable_initialization = VariableInitialization(identifier_token.value,
                                                     expression)

    inline_statement = InlineStatement(variable_initialization)
    
    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, inline_statement)

    node_result: NodeResult = parse_tokens_for_inline_statement(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_error_for_statement_that_expects_semicolon():
    """
    This test checks that the parser can generate an error when it expects a
    semicolon for an InlineStatement but doesn't get it.
    """

    variable_type_token = Token(LONG_TOKEN_TYPE, "LONG")
    identifier_token = Token(IDENTIFIER_TOKEN_TYPE, "yolo")
    decimal_literal_token: Token = generate_number_token_with_random_value()

    tokens: Tuple[Token, ...] = (
        variable_type_token, identifier_token, equals_token,
        decimal_literal_token, divide_token, decimal_literal_token,
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(END_OF_FILE_TOKEN_TYPE)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_inline_statement(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_variable_initialization():
    """
    This test checks that the parser can correctly generate a
    VariableInitialization object using the
    `parse_tokens_for_variable_initilization` function
    """

    variable_type_token = Token(INT_TOKEN_TYPE, "INT")
    identifier_token = Token(IDENTIFIER_TOKEN_TYPE, "my_var")
    decimal_literal_token: Token = generate_number_token_with_random_value()

    tokens: Tuple[Token, ...] = (
        variable_type_token, identifier_token, equals_token,
        decimal_literal_token,
        end_of_file_token
    )

    factor = FactorNode(decimal_literal_token.value)
    term = TermNode(factor)
    expression = ExpressionNode(term)
    
    variable_intialization = VariableInitialization(identifier_token.value,
                                                    expression)

    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, variable_intialization)

    node_result: NodeResult = parse_tokens_for_variable_initialization(tokens)

    assert expected_output == node_result


def test_parser_can_return_correct_error_for_initialization_when_expects_equals():
    """
    This test checks that the parser's function
    `parse_tokens_for_variable_initialization` can return the correct NodeFailure
    object when given a tuple of tokens that omits the equals sign. 
    """

    variable_type_token = Token(INT_TOKEN_TYPE, "INT")
    identifier_token = Token(IDENTIFIER_TOKEN_TYPE, "my_error_prone_var")

    tokens: Tuple[Token, Token, Token] = (
        variable_type_token, identifier_token,
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(END_OF_FILE_TOKEN_TYPE)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_variable_initialization(tokens)

    assert expected_output == node_result


def test_parser_can_return_correct_error_for_initialization_when_expects_expression():
    """
    This test checks the function `parse_tokens_for_variable_initialization`
    can return the correct NodeFailure object when given a list of tokens
    that omits an expression (which is required).
    """

    variable_type_token = Token(DOUBLE_TOKEN_TYPE, "DOUBLE")
    identifier_token = Token(IDENTIFIER_TOKEN_TYPE, "anju")

    tokens: Tuple[Token, Token, Token, Token] = (
        variable_type_token, identifier_token, equals_token,
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(END_OF_FILE_TOKEN_TYPE)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_variable_initialization(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_empty_return_statement():
    """
    This test checks that the function `parse_tokens_for_return_statement`
    can return the correct ReturnStatement object when given an empty return
    statement (i.e. returning without an expression provided)
    """

    return_statement_token = Token(RETURN_TOKEN_TYPE, "RETURN")
    tokens: Tuple[Token, Token, Token] = (
        return_statement_token, semi_colon_token,
        end_of_file_token
    )

    return_statement = ReturnStatement()
    expected_output_tokens: Tuple[Token, Token] = tokens[1:]
    expected_output = NodeSuccess(expected_output_tokens, return_statement)

    node_result: NodeResult = parse_tokens_for_return_statement(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_non_empty_return_statement():
    """
    This test checks that the function `parse_tokens_for_return_Statement`
    can return the correct ReturnStatement object when given an expression that
    is not empty for the return statement
    """

    return_statement_token = Token(RETURN_TOKEN_TYPE, "RETURN")
    decimal_literal_token: Token = generate_number_token_with_random_value()

    tokens: Tuple[Token, ...] = (
        return_statement_token, decimal_literal_token, plus_token,
        decimal_literal_token, semi_colon_token,
        end_of_file_token
    )

    factor = FactorNode(decimal_literal_token.value)
    term = TermNode(factor)

    additional_expression = ExpressionNode(term)
    expression = ExpressionNode(term, ArithmeticOperator.PLUS,
                                additional_expression)

    return_statement = ReturnStatement(expression)

    expected_output_tokens: Tuple[Token, Token] = (semi_colon_token,
                                                   end_of_file_token)
    expected_output = NodeSuccess(expected_output_tokens, return_statement)

    node_result: NodeResult = parse_tokens_for_return_statement(tokens)

    assert expected_output == node_result


def test_parse_can_generate_correct_error_given_invalid_expression_to_return():
    """
    This test cehcks that the function `parse_tokens_for_return_statement` can
    return the correct error when given an invalid expression to return.
    """
    
    return_statement_token = Token(RETURN_TOKEN_TYPE, "RETURN")

    tokens: Tuple[Token, Token, Token] = (
        return_statement_token, return_statement_token,
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(RETURN_TOKEN_TYPE)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_return_statement(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_error_given_faulty_input():
    """
    This test checks the parser's entrypoint function `parse_tokens`
    to see if it can generate the correct error output given a faulty input
    """

    tokens: Tuple[Token, Token] = (plus_token, end_of_file_token)

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(PLUS_TOKEN_TYPE)
    expected_output = ParserFailure(error_message)

    parser_result: ParserResult = parse_tokens(tokens)

    assert expected_output == parser_result


def test_parser_can_generate_correct_for_variable_increment_for_plus_plus():
    """
    This test checks that the parser can correctly generate an ast for
    incrementing a variable.

    This test checks for the ++ operator
    """

    identifier_token = Token(IDENTIFIER_TOKEN_TYPE, "yoworld")
    tokens: Tuple[Token, ...] = (
        identifier_token, plus_token, plus_token,
        end_of_file_token
    )

    DEFAULT_INCREMENT = "1"

    factor = FactorNode(DEFAULT_INCREMENT)
    term = TermNode(factor)
    expression = ExpressionNode(term)

    variable_increment = VariableIncrement(identifier_token.value, expression)
    
    expected_output_tokens: Tuple[Token, ...] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, variable_increment)

    node_result: NodeResult = parse_tokens_for_variable_increment(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_increment_with_expression():
    """
    This test checks that the parser can generate the correct VariableIncrement
    object when supplied with an expression like "x += 1" opposed to "x++"
    """

    identifier_token = Token(IDENTIFIER_TOKEN_TYPE, "yoworld")
    decimal_literal_token: Token = generate_number_token_with_random_value()
    tokens: Tuple[Token, ...] = (
        identifier_token, plus_token, equals_token, decimal_literal_token,
        end_of_file_token
    )

    factor = FactorNode(decimal_literal_token.value)
    term = TermNode(factor)
    expression = ExpressionNode(term)

    variable_increment = VariableIncrement(identifier_token.value, expression)
    
    expected_output_tokens: Tuple[Token, ...] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, variable_increment)

    node_result: NodeResult = parse_tokens_for_variable_increment(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_error_given_bad_expression_to_increment():
    """
    This test checks that the parser can correctly generate an error using the
    `parse_tokens_for_variable_increment` function when given an expression that
    contains a syntax error like "x += /9".
    """

    identifier_token = Token(IDENTIFIER_TOKEN_TYPE, "x")
    decimal_literal_token: Token =  generate_number_token_with_random_value()

    tokens: Tuple[Token, ...] = (
        identifier_token, plus_token, equals_token, decimal_literal_token,
        plus_token,
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(END_OF_FILE_TOKEN_TYPE)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_variable_increment(tokens)
    
    assert expected_output == node_result


def test_parser_can_generate_correct_ast():
    """
    This test checks the parser's entrypoint function `parse_tokens'
    to see if it can generate the correct AST
    """

    decimal_literal_token: Token = generate_number_token_with_random_value()

    tokens: Tuple[Token, Token, Token, Token, Token] = (
        decimal_literal_token, plus_token, decimal_literal_token, semi_colon_token,
        end_of_file_token
    )

    factor = FactorNode(decimal_literal_token.value)
    term = TermNode(factor)
    additional_expression = ExpressionNode(term)

    expression = ExpressionNode(term, ArithmeticOperator.PLUS, additional_expression)
    expected_output = InlineStatement(expression)

    parser_result: ParserResult = parse_tokens(tokens)

    assert expected_output == parser_result

