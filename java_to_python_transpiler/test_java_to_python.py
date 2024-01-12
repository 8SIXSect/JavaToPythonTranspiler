from typing import Tuple

from java_to_python_transpiler.java_to_python import (
    VARIABLE_TYPES, ArgumentList, ArithmeticOperator, BlockStatement, ClassDeclaration,
    ComparisonExpression, ComparisonOperator, ExpressionNode, FactorNode,
    IfStatement, InlineStatement, MethodDeclaration, MethodDeclarationList,
    NoNode, ParameterList, QualifiedIdentifier, StatementList, LexerFailure, MethodCall, NodeFailure,
    NodeResult, NodeSuccess, ParserFailure, ReturnStatement, TermNode, Token,
    LexerResult, Tokens, VariableIncrement, VariableInitialization,
    WhileStatement, TokenType, emit_ast_into_output,
    parse_tokens, parse_tokens_for_access_modifier_list,
    parse_tokens_for_argument_list, parse_tokens_for_block_statement,
    parse_tokens_for_block_statement_body, parse_tokens_for_class_declaration,
    parse_tokens_for_comparison_expression, parse_tokens_for_complete_variable_type,
    parse_tokens_for_expression, parse_tokens_for_expression_in_paren,
    parse_tokens_for_factor, parse_tokens_for_if_statement, parse_tokens_for_inline_statement,
    parse_tokens_for_method_declaration, parse_tokens_for_method_declaration_list,
    parse_tokens_for_parameter_list, parse_tokens_for_qualified_identifier, parse_tokens_for_statement_list,
    parse_tokens_for_method_call, parse_tokens_for_return_statement,
    parse_tokens_for_term, parse_tokens_for_variable_increment,
    parse_tokens_for_variable_initialization, parse_tokens_for_while_statement,
    report_error_for_lexer, scan_and_tokenize_input, ParserResult,
    ERROR_MESSAGE_FOR_LEXER, ERROR_MESSAGE_FOR_PARSER
)


# These tokens may be reused throughout the tests in this file
end_of_file_token = Token(TokenType.END_OF_FILE, "", -1, -1)


def plus_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.PLUS, "+", line_number, column_start)


def minus_token (line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.MINUS, "-", line_number, column_start)


def multiply_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.MULTIPLY, "*", line_number, column_start)


def divide_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.DIVIDE, "/", line_number, column_start)


def left_curly_brace_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.LEFT_CURLY_BRACE, "{", line_number, column_start)


def right_curly_brace_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.RIGHT_CURLY_BRACE, "}", line_number, column_start)

def left_parenthesis_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.LEFT_PARENTHESIS, "(", line_number, column_start)


def right_parenthesis_token (line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.RIGHT_PARENTHESIS, ")", line_number, column_start)


def left_bracket_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.LEFT_BRACKET, "[", line_number, column_start)


def right_bracket_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.RIGHT_BRACKET, "]", line_number, column_start)


def exclamation_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.EXCLAMATION, "!", line_number, column_start)


def comma_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.COMMA, ",", line_number, column_start)


def period_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.PERIOD, ".", line_number, column_start)


def semi_colon_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.SEMI_COLON, ";", line_number, column_start)


def less_than_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.LESS_THAN, "<", line_number, column_start)


def greater_than_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.GREATER_THAN, ">", line_number, column_start)


def equals_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.EQUALS, "=", line_number, column_start)


def true_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.TRUE, "TRUE", line_number, column_start)


def false_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.FALSE, "FALSE", line_number, column_start)


def return_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.RETURN, "RETURN", line_number, column_start)


def while_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.WHILE, "WHILE", line_number, column_start)


def if_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.IF, "IF", line_number, column_start)


def else_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.ELSE, "ELSE", line_number, column_start)


def class_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.CLASS, "CLASS", line_number, column_start)


def public_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.PUBLIC, "PUBLIC", line_number, column_start)


def private_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.PRIVATE, "PRIVATE", line_number, column_start)


def static_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.STATIC, "STATIC", line_number, column_start)


def void_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.VOID, "VOID", line_number, column_start)


def char_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.CHAR, "CHAR", line_number, column_start)


def int_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.INT, "INT", line_number, column_start)


def short_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.SHORT, "SHORT", line_number, column_start)


def double_token(line_number: int = 1, column_start: int = 1) -> Token:
    return Token(TokenType.DOUBLE, "DOUBLE", line_number, column_start)


def decimal_literal_token(value: str, line_number: int = 1,
                          column_start: int = 1) -> Token:
    return Token(TokenType.DECIMAL_LITERAL, value, line_number, column_start)


def float_literal_token(value: str, line_number: int = 1,
                        column_start: int = 1) -> Token:
    return Token(TokenType.FLOAT_LITERAL, value, line_number, column_start)


def string_literal_token(value: str, line_number: int = 1,
                         column_start: int = 1) -> Token:
    return Token(TokenType.STRING_LITERAL, value, line_number, column_start)


def identifier_token(value: str, line_number: int = 1,
                     column_start: int = 1) -> Token:
    return Token(TokenType.IDENTIFIER, value, line_number, column_start)


def test_report_error_for_lexer_returns_proper_error_object():
    """
    This test checks if the function `report_error_for_lexer` returns
    the correct LexerFailure object.
    """

    UNKNOWN_CHARACTER = "~"
    LINE_NUMBER = 1

    error_message: str = ERROR_MESSAGE_FOR_LEXER.format(UNKNOWN_CHARACTER,
                                                        LINE_NUMBER)
    expected_output = LexerFailure(error_message)

    report_error_output: LexerFailure = report_error_for_lexer(UNKNOWN_CHARACTER,
                                                               LINE_NUMBER)

    assert expected_output == report_error_output


def test_lexer_can_generate_tokens_for_single_line_comment():
    """
    This test checks if the lexer can successfully generate a Token object
    with a token type of SINGLE LINE COMMMENT.
    """

    INPUT = "// this is in a `comment`"

    expected_output_token = Token(TokenType.SINGLE_LINE_COMMENT, INPUT, 1, 1)
    expected_output: Tuple[Token, Token] = (expected_output_token, end_of_file_token)
 
    lexer_result: LexerResult = scan_and_tokenize_input(INPUT)

    assert expected_output == lexer_result


def test_lexer_can_generate_token_for_grouping_characters():
    """
    This test checks if the lexer can successfully generate a Token objects
    for parenthesis, curly braces, and brackets
    """

    INPUT = "(){}[]"

    expected_output: Tokens = (
        left_parenthesis_token(1, 1),
        right_parenthesis_token(1, 2),
        left_curly_brace_token(1, 3),
        right_curly_brace_token(1, 4),
        left_bracket_token(1, 5),
        right_bracket_token(1, 6),
        end_of_file_token
    )

    lexer_result: LexerResult = scan_and_tokenize_input(INPUT)

    assert expected_output == lexer_result


def test_lexer_can_generate_tokens_for_punctuation_characters():
    """
    This test checks if the lexer can successfully generate Token objects for
    semicolons, commas, and periods.
    """

    INPUT = ";.,,.;.."

    # Not the prettiest solution, but it saves lines so I like it as is
    expected_output: Tokens = (
        semi_colon_token(1, 1),
        period_token(1, 2),
        comma_token(1, 3),
        comma_token(1, 4),
        period_token(1, 5),
        semi_colon_token(1, 6),
        period_token(1, 7),
        period_token(1, 8),
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


    expected_output: Tokens = (
        equals_token(1, 1),
        greater_than_token(1, 2),
        equals_token(1, 3),
        less_than_token(1, 4),
        equals_token(1, 5),
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

    expected_output: Tokens = (
        plus_token(1, 1),
        plus_token(1, 2),
        divide_token(1, 3),
        multiply_token(1, 4),
        minus_token(1, 5),
        divide_token(1, 6),
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

    expected_output: Tuple[Token, Token] = (
        float_literal_token(INPUT, 1, 1),
        end_of_file_token
    )

    lexer_result: LexerResult = scan_and_tokenize_input(INPUT)
    
    assert expected_output == lexer_result


def test_lexer_can_generate_tokens_for_decimal_literals():
    """
    This test checks if the lexer can successfully generate Token objects for
    decimal literals.
    """

    INPUT = "86868686"

    expected_output: Tuple[Token, Token] = (
        decimal_literal_token(INPUT, 1, 1),
        end_of_file_token
    )

    lexer_result: LexerResult = scan_and_tokenize_input(INPUT)

    assert expected_output == lexer_result


def test_lexer_can_generate_tokens_for_string_literals():
    """
    This test checks if the lexer can successfully generate Tokens objects for
    string literals.
    """

    INPUT = '"hello"'

    expected_output: Tuple[Token, Token] = (
        string_literal_token(INPUT, 1, 1),
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

    expected_output: Tokens = (
        identifier_token("eighty", 1, 1),
        identifier_token("sixth", 1, 8),
        identifier_token("sector", 1, 14),
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

    expected_output: Tokens = (
        Token(TokenType.TRUE, "true", 1, 1),
        Token(TokenType.SHORT, "short", 1, 6),
        Token(TokenType.WHILE, "while", 1, 12),
        end_of_file_token
    )

    lexer_result: LexerResult = scan_and_tokenize_input(INPUT)

    assert expected_output == lexer_result


def generate_single_comp_expression(number_or_id: str) -> ComparisonExpression:
    """
    This is a helper function that generates a ComparisonExpression object with
    a single term, and a single expression using `number_or_id.`

    `number_or_id` is an abbreviation for number_or_identifier found in
    FactorNode's parameters.
    """

    factor = FactorNode(number_or_id)
    term = TermNode(factor)
    expression = ExpressionNode(term)
    
    return ComparisonExpression(expression)


def generate_limited_comp_expression(first_number_or_id: str,
                                     operator: ComparisonOperator,
                                     second_number_or_id: str):
    """
    This is a helper function that generates a ComparisonExpression object with
    two single-term-single-factor expressions.

    `first_number_or_id` represents the first number/identifier

    `operator` represents the comparison operatar (<, >, <=, >=, ==, !=)

    `second_number_or_id` represents the second number/identifier
    """

    first_factor = FactorNode(first_number_or_id)
    first_term = TermNode(first_factor)
    first_expression = ExpressionNode(first_term)

    second_factor = FactorNode(second_number_or_id)
    second_term = TermNode(second_factor)
    second_expression = ExpressionNode(second_term)

    return ComparisonExpression(first_expression, operator, second_expression) 


def test_parser_can_generate_correct_error_given_faulty_input():
    """
    This test checks the parser's entrypoint function `parse_tokens`
    to see if it can generate the correct error output given a faulty input
    """

    tokens: Tuple[Token, Token] = (
        plus_token(),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.PLUS, 1)
    expected_output = ParserFailure(error_message)

    parser_result: ParserResult = parse_tokens(tokens)

    assert expected_output == parser_result


def test_parser_can_generate_correct_ast_for_class_declaration():
    """
    This test checks if the function `parse_tokens_for_class_declaration`
    can correctly parse the tokens when provided with a valid input
    """

    CLASS_IDENTIFIER = "abra"
    METHOD_IDENTIFIER = "apl"
    DECIMAL_LITERAL_VALUE = "86"

    # The col/row values dont really matter
    tokens: Tokens = (
        private_token(),
        class_token(),
        identifier_token(CLASS_IDENTIFIER),
        left_curly_brace_token(),
        public_token(),
        int_token(),
        identifier_token(METHOD_IDENTIFIER),
        left_parenthesis_token(),
        right_parenthesis_token(),
        left_curly_brace_token(),
        return_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        semi_colon_token(),
        right_curly_brace_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    return_comp_expression: ComparisonExpression = generate_single_comp_expression(
        DECIMAL_LITERAL_VALUE
    )

    return_statement = ReturnStatement(return_comp_expression)
    inline_statement = InlineStatement(return_statement)
    statement_list = StatementList(inline_statement)

    parameter_list = ParameterList()

    method_declaration = MethodDeclaration(
        METHOD_IDENTIFIER, parameter_list, statement_list
    )
    method_declaration_list = MethodDeclarationList(method_declaration)
    class_declaration = ClassDeclaration(
        CLASS_IDENTIFIER, method_declaration_list
    )

    expected_output_tokens: Tokens = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, class_declaration)

    node_result: NodeResult = parse_tokens_for_class_declaration(tokens)

    assert expected_output == node_result


def test_parser_can_produce_error_for_class_dec_when_access_modifier_omitted():
    """
    This test checks if the function `parse_tokens_for_class_declaration`
    can produce an error when the access modifier(s) of a class are omitted.
    """

    CLASS_IDENTIFIER = "myid"

    tokens: Tokens = (
        class_token(1, 1),
        identifier_token(CLASS_IDENTIFIER),
        left_curly_brace_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.CLASS, 1)
    expected_output = NodeFailure(error_message)
    
    node_result: NodeResult = parse_tokens_for_class_declaration(tokens)

    assert expected_output == node_result


def test_parser_can_produce_error_for_class_dec_when_class_keyword_omitted():
    """
    This test checks if the function `parse_tokens_for_class_declaration`
    can produce an error when the class keyword is omitted.
    """

    CLASS_IDENTIFIER = "shinnei"

    tokens: Tokens = (
        public_token(1, 1),
        identifier_token(CLASS_IDENTIFIER, 1, 8),
        left_curly_brace_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    # All that matters here it the line number -- the '8' is kind of irrev.
    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.IDENTIFIER, 1)
    expected_output = NodeFailure(error_message)
    
    node_result: NodeResult = parse_tokens_for_class_declaration(tokens)

    assert expected_output == node_result


def test_parser_can_produce_error_for_class_dec_when_identifier_omitted():
    """
    This test checks if the function `parse_tokens_for_class_declaration`
    can produce an error when the identifier is omitted.
    """

    tokens: Tokens = (
        public_token(1, 1),
        class_token(1),
        left_curly_brace_token(1),
        right_curly_brace_token(),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.LEFT_CURLY_BRACE, 1)
    expected_output = NodeFailure(error_message)
    
    node_result: NodeResult = parse_tokens_for_class_declaration(tokens)

    assert expected_output == node_result


def test_parser_can_produce_error_for_class_dec_when_left_brace_omitted():
    """
    This test checks if the function `parse_tokens_for_class_declaration`
    can produce an error when the left/opening curly brace is omitted.
    """

    CLASS_IDENTIFIER = "pascal"

    tokens: Tokens = (
        public_token(1),
        class_token(1),
        identifier_token(CLASS_IDENTIFIER, 1),
        right_curly_brace_token(1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(
        TokenType.RIGHT_CURLY_BRACE, 1
    )
    expected_output = NodeFailure(error_message)
    
    node_result: NodeResult = parse_tokens_for_class_declaration(tokens)

    assert expected_output == node_result


def test_parser_can_produce_error_for_class_dec_when_error_occurs_method_dec_list():
    """
    This test checks if the function `parse_tokens_for_class_declaration`
    can produce an error when an error occurs in method_declaration_list
    """

    CLASS_IDENTIFIER = "shiden"

    tokens: Tokens = (
        public_token(1),
        class_token(1),
        identifier_token(CLASS_IDENTIFIER, 1),
        left_curly_brace_token(1),
        while_token(2),
        right_curly_brace_token(3),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.WHILE, 2)
    expected_output = NodeFailure(error_message)
    
    node_result: NodeResult = parse_tokens_for_class_declaration(tokens)

    assert expected_output == node_result


def test_parser_can_produce_error_for_class_dec_when_right_brace_omitted():
    """
    This test checks if the function `parse_tokens_for_class_declaration`
    can produce an error when the right/closing curly brace is omitted.
    """

    CLASS_IDENTIFIER = "vladilena"

    tokens: Tokens = (
        public_token(1),
        class_token(1),
        identifier_token(CLASS_IDENTIFIER, 1),
        left_curly_brace_token(1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.END_OF_FILE, -1)
    expected_output = NodeFailure(error_message)
    
    node_result: NodeResult = parse_tokens_for_class_declaration(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_method_dec_list_with_no_methods():
    """
    This test checks if the function `parse_tokens_for_method_declaration_list`
    can correctly parse the tokens when provided with no methods.
    """

    tokens: Tuple[Token, Token] = (
        right_curly_brace_token(),
        end_of_file_token
    )
    
    method_declaration_list = MethodDeclarationList()
    expected_output = NodeSuccess(tokens, method_declaration_list)

    node_result: NodeResult = parse_tokens_for_method_declaration_list(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_method_dec_list_with_one_method():
    """
    This test checks if the function `parse_tokens_for_method_declaration_list`
    can corretly parse the tokens when provided with a singular method.
    """

    IDENTIFIER = "annette"

    tokens: Tokens = (
        private_token(),
        int_token(),
        identifier_token(IDENTIFIER),
        left_parenthesis_token(),
        right_parenthesis_token(),
        left_curly_brace_token(),
        right_curly_brace_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    parameter_list = ParameterList()
    statement_list = StatementList()

    method_declaration = MethodDeclaration(
        IDENTIFIER,
        parameter_list,
        statement_list
    )

    method_declaration_list = MethodDeclarationList(method_declaration)

    expected_output_tokens: Tuple[Token, Token] = (
        right_curly_brace_token(),
        end_of_file_token,
    )
    expected_output = NodeSuccess(expected_output_tokens, method_declaration_list)

    node_result: NodeResult = parse_tokens_for_method_declaration_list(tokens)

    assert expected_output == node_result


def test_parser_can_produce_error_for_method_dec_list_that_fails_initial_method():
    """
    This test checks if the function `parse_tokens_for_method_declaration_list`
    can produce an error because of failure of the initial method.
    """

    tokens: Tokens = (
        private_token(1),
        int_token(1),
        left_parenthesis_token(1),
        right_parenthesis_token(),
        left_curly_brace_token(),
        right_curly_brace_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.LEFT_PARENTHESIS, 1)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_method_declaration_list(tokens)

    assert expected_output == node_result


def test_parser_can_produce_ast_for_method_dec_list_with_multiple_methods():
    """
    This test checks if the function `parse_tokens_for_method_dec_list`
    can produce the correct ast for a method declaration list with multiple
    methods.
    """

    METHOD_IDENTIFIER = "vagabond"
    ADDITIONAL_IDENTIFIER = "vinland"
    RETURN_VALUE = "86"

    tokens: Tokens = (
        static_token(),
        int_token(),
        identifier_token(METHOD_IDENTIFIER),
        left_parenthesis_token(),
        right_parenthesis_token(),
        left_curly_brace_token(),
        return_token(),
        decimal_literal_token(RETURN_VALUE),
        semi_colon_token(),
        right_curly_brace_token(),
        public_token(),
        int_token(),
        identifier_token(ADDITIONAL_IDENTIFIER),
        left_parenthesis_token(),
        right_parenthesis_token(),
        left_curly_brace_token(),
        right_curly_brace_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    parameter_list = ParameterList()

    return_comparison_expression: ComparisonExpression
    return_comparison_expression = generate_single_comp_expression(
        RETURN_VALUE
    )
    return_statement = ReturnStatement(return_comparison_expression)

    inline_statement = InlineStatement(return_statement)

    first_statement_list = StatementList(inline_statement)
    second_statement_list = StatementList()

    additional_method_declaration = MethodDeclaration(
        ADDITIONAL_IDENTIFIER,
        parameter_list, second_statement_list
    )
    additional_method_declaration_list = MethodDeclarationList(
        additional_method_declaration
    )

    method_declaration = MethodDeclaration(
        METHOD_IDENTIFIER,
        parameter_list,
        first_statement_list
    )

    method_declaration_list = MethodDeclarationList(
        method_declaration,
        additional_method_declaration_list
    )

    expected_output_tokens = (
        right_curly_brace_token(),
        end_of_file_token,
    )
    expected_output = NodeSuccess(expected_output_tokens, method_declaration_list)

    node_result: NodeResult = parse_tokens_for_method_declaration_list(tokens)
    
    assert expected_output == node_result


def test_parser_can_produce_error_for_method_dec_list_that_fails_additional_method():
    """
    This test checks if the function `parse_tokens_for_method_dec_list`
    can produce an error because of failure of the additional method.
    """

    METHOD_IDENTIFIER = "sat"
    ADDITIONAL_METHOD_IDENTIFIER = "Jhu"
    RETURN_VALUE = "85"

    tokens: Tokens = (
        static_token(1),
        short_token(1),
        identifier_token(METHOD_IDENTIFIER, 1),
        left_parenthesis_token(1),
        right_parenthesis_token(1),
        left_curly_brace_token(1),
        return_token(2),
        decimal_literal_token(RETURN_VALUE, 2),
        semi_colon_token(2),
        right_curly_brace_token(3),
        public_token(4),
        int_token(4),
        identifier_token(ADDITIONAL_METHOD_IDENTIFIER, 4),
        left_parenthesis_token(4),
        left_curly_brace_token(4),
        right_curly_brace_token(5),
        right_curly_brace_token(6),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.LEFT_CURLY_BRACE, 4)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_method_declaration_list(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_method_declaration():
    """
    This test checks if the function `parse_tokens_for_method_declaration`
    can generate the correct ast when given a valid input for a method
    declaration.
    """

    METHOD_IDENTIFIER = "pascal"
    PARAMETER_IDENTIFIER = "cpp"
    DECIMAL_LITERAL_VALUE = "1"

    tokens: Tokens = (
        public_token(),
        static_token(),
        short_token(),
        identifier_token(METHOD_IDENTIFIER),
        left_parenthesis_token(),
        int_token(),
        identifier_token(PARAMETER_IDENTIFIER),
        right_parenthesis_token(),
        left_curly_brace_token(),
        return_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        semi_colon_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    parameter_list = ParameterList(PARAMETER_IDENTIFIER)
    
    return_comp_expression: ComparisonExpression 
    return_comp_expression = generate_single_comp_expression(
        DECIMAL_LITERAL_VALUE
    )

    return_statement = ReturnStatement(return_comp_expression)
    inline_statement = InlineStatement(return_statement)
    statement_list = StatementList(inline_statement)

    method_declaration = MethodDeclaration(
        METHOD_IDENTIFIER,
        parameter_list,
        statement_list
    )

    expected_output_tokens: Tokens = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, method_declaration)

    node_result: NodeResult = parse_tokens_for_method_declaration(tokens)

    assert expected_output == node_result


def test_parser_can_produce_error_for_no_access_modifiers_in_method_declaration():
    """
    This test checks if the function `parse_tokens_for_method_declaration`
    can generate the correct error when given no access modifier when one is
    expected for a method declaration
    """

    METHOD_IDENTIFIER = "pascal"
    PARAMETER_IDENTIFIER = "cpp" 
    DECIMAL_LITERAL_VALUE = "999"

    tokens: Tokens = (
        int_token(1),
        return_token(),
        identifier_token(METHOD_IDENTIFIER),
        left_parenthesis_token(),
        short_token(),
        identifier_token(PARAMETER_IDENTIFIER),
        right_parenthesis_token(),
        left_curly_brace_token(),
        return_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        semi_colon_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(
        TokenType.INT, 1
    )
    expected_output = NodeFailure(error_message)
    
    node_result: NodeResult = parse_tokens_for_method_declaration(tokens)

    assert expected_output == node_result


def test_parser_can_produce_correct_output_for_void_return_type_in_method_declaration():
    """
    This test checks if the function `parse_tokens_for_method_declaration`
    can generate the correct output when given a void return type in a method
    declaration.
    """

    METHOD_IDENTIFIER = "csharp"
    DECIMAL_LITERAL_VALUE = "85" 

    tokens: Tokens = (
        public_token(),
        static_token(),
        void_token(),
        identifier_token(METHOD_IDENTIFIER),
        left_parenthesis_token(),
        right_parenthesis_token(),
        left_curly_brace_token(),
        return_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        semi_colon_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    parameter_list = ParameterList()
    
    return_comp_expression: ComparisonExpression
    return_comp_expression = generate_single_comp_expression(
        DECIMAL_LITERAL_VALUE
    )

    return_statement = ReturnStatement(return_comp_expression)
    inline_statement = InlineStatement(return_statement)
    statement_list = StatementList(inline_statement)

    method_declaration = MethodDeclaration(
        METHOD_IDENTIFIER,
        parameter_list,
        statement_list
    )

    expected_output_tokens: Tokens = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, method_declaration)

    node_result: NodeResult = parse_tokens_for_method_declaration(tokens)

    assert expected_output == node_result


def test_parser_can_produce_error_for_no_return_type_in_method_declaration():
    """
    This test checks if the function `parse_tokens_for_method_declaration`
    can generate the correct error when given no return type in a method
    declaration.
    """

    METHOD_IDENTIFIER = "pascal"
    PARAMETER_IDENTIFIER = "cpp"
    DECIMAL_LITERAL_VALUE = "1"

    tokens: Tokens = (
        public_token(1),
        static_token(1),
        identifier_token(METHOD_IDENTIFIER, 1),
        left_parenthesis_token(1),
        double_token(),
        identifier_token(PARAMETER_IDENTIFIER),
        right_parenthesis_token(),
        left_curly_brace_token(),
        return_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        semi_colon_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    # Explanation: IDENTIFIER Counts as a VARIABLE TYPE so it sees that and
    # hits LEFT PARENTHESIS
    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(
        TokenType.LEFT_PARENTHESIS, 1
    )
    expected_output = NodeFailure(error_message)
    
    node_result: NodeResult = parse_tokens_for_method_declaration(tokens)

    assert expected_output == node_result


def test_parser_can_produce_error_for_no_identifier_in_method_declaration():
    """
    This test checks if the function `parse_tokens_for_method_declaration`
    can generate the correct error when given no identifier in a method
    declaration.
    """

    PARAMETER_IDENTIFIER = "perl"
    DECIMAL_LITERAL_VALUE = "8686"

    tokens: Tokens = (
        public_token(1),
        static_token(1),
        double_token(1),
        left_parenthesis_token(1),
        int_token(),
        identifier_token(PARAMETER_IDENTIFIER),
        right_parenthesis_token(),
        left_curly_brace_token(),
        return_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        semi_colon_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(
        TokenType.LEFT_PARENTHESIS, 1
    )
    expected_output = NodeFailure(error_message)
    
    node_result: NodeResult = parse_tokens_for_method_declaration(tokens)

    assert expected_output == node_result



def test_parser_can_produce_error_for_no_left_paren_in_method_declaration():
    """
    This test checks if the function `parse_tokens_for_method_declaration`
    can generate the correct error when given left/opening parenthesis in a
    method declaration.
    """

    METHOD_IDENTIFIER = "walterWhite"
    PARAMETER_IDENTIFIER = "mihal"
    DECIMAL_LITERAL_VALUE = "991"

    tokens: Tokens = (
        public_token(1),
        static_token(1),
        int_token(1),
        identifier_token(METHOD_IDENTIFIER, 1),
        short_token(1),
        identifier_token(PARAMETER_IDENTIFIER),
        right_parenthesis_token(),
        left_curly_brace_token(),
        return_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        semi_colon_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(
            TokenType.SHORT, 1
    )
    expected_output = NodeFailure(error_message)
    
    node_result: NodeResult = parse_tokens_for_method_declaration(tokens)

    assert expected_output == node_result


def test_parser_can_produce_error_for_invalid_parameter_input_in_method_declaration():
    """
    This test checks if the function `parse_tokens_for_method_declaration`
    can generate the correct error when parameter list fails.
    """

    METHOD_IDENTIFIER = "datalog"
    PARAMETER_IDENTIFIER = "prolog"
    DECIMAL_LITERAL_VALUE = "90"

    tokens: Tokens = (
        public_token(1),
        static_token(1),
        double_token(1),
        identifier_token(METHOD_IDENTIFIER, 1),
        left_parenthesis_token(1),
        identifier_token(PARAMETER_IDENTIFIER, 1),
        right_parenthesis_token(),
        left_curly_brace_token(),
        return_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        semi_colon_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    # Explanation: IDENTIFIER counts as a VARIABLE TYPE so it sees that but not
    # a second identifier for actual name of the parameter; instead its RPAREN
    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(
        TokenType.RIGHT_PARENTHESIS, 1
    )
    expected_output = NodeFailure(error_message)
    
    node_result: NodeResult = parse_tokens_for_method_declaration(tokens)

    assert expected_output == node_result


# I don't think you have to check for right paren; parameters should check 4 it
def test_parser_can_produce_error_for_no_block_statement_body_in_method_declaration():
    """
    This test checks if the function `parse_tokens_for_method_declaration`
    can generate the correct error when given no block statement body in a
    method declaration.
    """

    METHOD_IDENTIFIER = "java"
    PARAMETER_IDENTIFIER = "recursion"
    DECIMAL_LITERAL_VALUE = "101"

    tokens: Tokens = (
        public_token(1),
        static_token(1),
        double_token(1),
        identifier_token(METHOD_IDENTIFIER, 1),
        left_parenthesis_token(1),
        double_token(1),
        identifier_token(PARAMETER_IDENTIFIER, 1),
        right_parenthesis_token(1),
        left_curly_brace_token(1),
        return_token(1),
        decimal_literal_token(DECIMAL_LITERAL_VALUE, 1),
        semi_colon_token(1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.END_OF_FILE, -1)
    expected_output = NodeFailure(error_message)
    
    node_result: NodeResult = parse_tokens_for_method_declaration(tokens)

    assert expected_output == node_result


def test_parser_can_produce_error_for_no_access_modifiers_given():
    """
    This test checks if the function `parse_tokens_for_access_modifier_list`
    can generate the correct error when given no access modifier when one is
    expected.
    """

    IDENTIFIER = "turing"

    tokens: Tokens = (
        int_token(1),
        identifier_token(IDENTIFIER, 1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.INT, 1)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_access_modifier_list(tokens)
    
    assert expected_output == node_result


def test_parser_can_generate_correct_output_for_a_single_access_modifier():
    """
    This test checks if the function `parse_tokens_for_access_modifier_list`
    can generate the correct output when given one access modifer.
    """

    IDENTIFIER = "factor"

    tokens: Tokens = (
        public_token(),
        int_token(),
        identifier_token(IDENTIFIER),
        end_of_file_token
    )

    no_node = NoNode()
    expected_output_tokens: Tokens = tokens[1:]
    expected_output = NodeSuccess(expected_output_tokens, no_node)

    node_result: NodeResult = parse_tokens_for_access_modifier_list(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_output_for_multiple_access_modifiers():
    """
    This test checks if the function `parse_tokens_for_access_modifier` can
    generate the correct output when given multiple access modifiers
    """

    IDENTIFIER = "porth"

    tokens: Tokens = (
        public_token(),
        static_token(),
        char_token(),
        identifier_token(IDENTIFIER),
        end_of_file_token
    )

    no_node = NoNode()
    expected_output_tokens: Tokens = (
        char_token(),
        identifier_token(IDENTIFIER),
        end_of_file_token
    )
    expected_output = NodeSuccess(expected_output_tokens, no_node)

    node_result: NodeResult = parse_tokens_for_access_modifier_list(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_simple_variable_type():
    """
    This test checks if the function `parse_tokens_for_complete_variable_type`
    can correctly parse the tokens when provided with a simple variable type
    (i.e. one without brackets that signify that is an Array).
    """

    tokens: Tokens = (
        double_token(),
        end_of_file_token
    )
    
    no_node = NoNode()

    expected_output_tokens: Tokens = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, no_node)

    node_result: NodeResult = parse_tokens_for_complete_variable_type(
        tokens, VARIABLE_TYPES
    )

    assert expected_output == node_result


def test_parser_can_produce_correct_error_for_variable_type_that_omits_right_bracket():
    """
    This test checks if the function `parse_tokens_for_complete_variable_type`
    can produce the correct error provided with a variable that has a left
    bracket but does not have closing right bracket.
    """

    tokens: Tokens = (
        int_token(1),
        left_bracket_token(1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.END_OF_FILE, -1)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_complete_variable_type(
        tokens, VARIABLE_TYPES
    )

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_complete_variable_type():
    """
    This test checks if the function `parse_tokens_for_complete_variable_type`
    can generate the correct ast when provided with a variable type with a
    set of brackets appended to it.

    Example: int[]
    """

    tokens: Tokens = (
        char_token(),
        left_bracket_token(),
        right_bracket_token(),
        end_of_file_token
    )

    no_node = NoNode()

    expected_output_tokens: Tokens = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, no_node)

    node_result: NodeResult = parse_tokens_for_complete_variable_type(
        tokens, VARIABLE_TYPES
    )

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_parameter_list_with_no_parameters():
    """
    This test checks if the function `parse_tokens_for_parameter_list` can
    correctly parse the tokens when provided with no parameters.
    """

    tokens: Tokens = (
        right_parenthesis_token(),
        end_of_file_token
    )

    parameter_list = ParameterList()
    expected_output = NodeSuccess(tokens, parameter_list)

    node_result: NodeResult = parse_tokens_for_parameter_list(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_single_parameter_list():
    """
    This test checks that the parser can correctly generate an ParameterList
    object using the `parse_tokens_for_parameter_list` function when given an
    input of a singular parameter.
    """

    IDENTIFIER = "ada"
    
    tokens: Tokens = (
        double_token(),
        identifier_token(IDENTIFIER),
        right_parenthesis_token(),
        end_of_file_token
    )

    parameter_list = ParameterList(IDENTIFIER)

    expected_output_tokens: Tokens = (
        right_parenthesis_token(),
        end_of_file_token
    )
    expected_output = NodeSuccess(expected_output_tokens, parameter_list)

    node_result: NodeResult = parse_tokens_for_parameter_list(tokens)

    assert expected_output == node_result


def test_parser_can_produce_error_when_parameter_without_type():
    """
    This test checks that the parser can correctly generate a NodeFailure
    object using the `parse_tokens_for_parameter_list` function when given
    an input the omits the variable type for a parameter
    """

    IDENTIFIER = "apl"
    tokens: Tokens = (
        identifier_token(IDENTIFIER, 1),
        right_parenthesis_token(1),
        end_of_file_token
    )
    
    # Explanation: IDENTIFIER counts as a VARIABLE_TYPE so.........
    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(
        TokenType.RIGHT_PARENTHESIS, 1
    )
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_parameter_list(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_multiple_parameter_list():   
    """
    This test checks that the parser can correctly generate an ParameterList
    object using the `parse_tokens_for_parameter_list` function when given an
    input of multiple parameters.
    """

    FIRST_IDENTIFIER = "logtalk"
    SECOND_IDENTIFIER = "datalog"

    tokens: Tokens = (
        int_token(),
        identifier_token(FIRST_IDENTIFIER),
        comma_token(),
        int_token(),
        identifier_token(SECOND_IDENTIFIER),
        right_parenthesis_token(),
        end_of_file_token
    )

    additional_parameter_list = ParameterList(SECOND_IDENTIFIER)
    parameter_list = ParameterList(FIRST_IDENTIFIER,
                                   additional_parameter_list)

    expected_output_tokens: Tokens = (
        right_parenthesis_token(),
        end_of_file_token
    )
    expected_output = NodeSuccess(expected_output_tokens, parameter_list)

    node_result: NodeResult = parse_tokens_for_parameter_list(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_error_when_parameter_list_expects_parenthesis_or_comma():
    """
    This test checks that the parser can correctly generate a NodeFailure object
    when supplied a faulty input that omits either a comma or a parenthesis.
    """

    IDENTIFIER = "cabal"

    tokens: Tokens = (
        char_token(1),
        identifier_token(IDENTIFIER, 1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.END_OF_FILE, -1)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_parameter_list(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_statement_list_with_no_statements():
    """
    This test checks if the function `parse_tokens_for_inline_statement_list`
    can correctly parse the tokens when provided with no statements.
    """

    tokens: Tuple[Token, Token] = (
        right_curly_brace_token(),
        end_of_file_token
    )
    
    inline_statement_list = StatementList()
    expected_output = NodeSuccess(tokens, inline_statement_list)

    node_result: NodeResult = parse_tokens_for_statement_list(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_statement_list_with_one_statement():
    """
    This test checks if the function `parse_tokens_for_inline_statement_list`
    can corretly parse the tokens when provided with a singular statement.
    """

    DECIMAL_LITERAL_VALUE = "01"

    tokens: Tokens = (
        return_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        semi_colon_token(),
        end_of_file_token
    )

    comparison_expression: ComparisonExpression = generate_single_comp_expression(
        DECIMAL_LITERAL_VALUE
    )
    return_statement = ReturnStatement(comparison_expression)

    inline_statement = InlineStatement(return_statement)
    inline_statement_list = StatementList(inline_statement)

    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, inline_statement_list)

    node_result: NodeResult = parse_tokens_for_statement_list(tokens)

    assert expected_output == node_result


def test_parser_can_produce_error_for_statement_list_that_fails_initial_statement():
    """
    This test checks if the function `parse_tokens_for_inline_statement_list`
    can produce an error because of failure of the initial statement.
    """

    tokens: Tokens = (
        return_token(1),
        return_token(1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.RETURN, 1)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_statement_list(tokens)

    assert expected_output == node_result


def test_parser_can_produce_ast_for_statement_list_with_multiple_statements():
    """
    This test checks if the function `parse_tokens_for_inline_statement_list`
    can produce the correct ast for a statement list with multiple statements.
    """

    IDENTIFIER = "sat"
    ADDITIONAL_IDENTIFIER = "mof"
    DECIMAL_LITERAL_VALUE = "7"

    tokens: Tokens = (
        double_token(),
        identifier_token(IDENTIFIER),
        equals_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        semi_colon_token(),
        return_token(),
        identifier_token(ADDITIONAL_IDENTIFIER),
        semi_colon_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    comparison_expression: ComparisonExpression = generate_single_comp_expression(
        DECIMAL_LITERAL_VALUE
    )
    variable_initialization = VariableInitialization(IDENTIFIER,
                                                     comparison_expression)

    qualified_identifier = QualifiedIdentifier(ADDITIONAL_IDENTIFIER)
    factor = FactorNode(qualified_identifier=qualified_identifier)
    term = TermNode(factor)
    expression = ExpressionNode(term)
    return_comp_expression = ComparisonExpression(expression)

    return_statement = ReturnStatement(return_comp_expression)

    initial_inline_statement = InlineStatement(variable_initialization)
    additional_inline_statement = InlineStatement(return_statement)
    additional_statement_list = StatementList(additional_inline_statement)

    inline_statement_list = StatementList(initial_inline_statement,
                                                additional_statement_list)

    expected_output_tokens = (
        right_curly_brace_token(),
        end_of_file_token,
    )
    expected_output = NodeSuccess(expected_output_tokens, inline_statement_list)

    node_result: NodeResult = parse_tokens_for_statement_list(tokens)
    
    assert expected_output == node_result


def test_parser_can_produce_error_for_statement_list_that_fails_additional_statement():
    """
    This test checks if the function `parse_tokens_for_inline_statement_list`
    can produce an error because of failure of the additional statement.
    """

    IDENTIFIER = "giannis"
    DECIMAL_LITERAL_VALUE = "40"

    tokens: Tokens = (
        char_token(1),
        identifier_token(IDENTIFIER, 1),
        equals_token(1),
        decimal_literal_token(DECIMAL_LITERAL_VALUE, 1),
        semi_colon_token(1),
        return_token(2),
        equals_token(2),
        semi_colon_token(2),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.EQUALS, 2)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_statement_list(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_block_statement():
    """
    This test checks if the function `parse_tokens_for_block_statement` can
    parse the tokens when provided with tokens containing a block statement.
    """

    IDENTIFIER = "PROLOG"
    DECIMAL_LITERAL_VALUE = "87"

    tokens: Tokens = (
        while_token(),
        left_parenthesis_token(),
        identifier_token(IDENTIFIER),
        right_parenthesis_token(),
        left_curly_brace_token(),
        identifier_token(IDENTIFIER),
        plus_token(),
        equals_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        semi_colon_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    qualified_identifier = QualifiedIdentifier(IDENTIFIER)
    factor = FactorNode(qualified_identifier=qualified_identifier)
    term = TermNode(factor)
    expression = ExpressionNode(term)
    while_condition_comp_expression = ComparisonExpression(expression)

    increment_comp_expression: ComparisonExpression
    increment_comp_expression = generate_single_comp_expression(
        DECIMAL_LITERAL_VALUE
    )

    variable_increment = VariableIncrement(IDENTIFIER, increment_comp_expression)
    inline_statement = InlineStatement(variable_increment)
    statement_list = StatementList(inline_statement)
    
    while_statement = WhileStatement(while_condition_comp_expression,
                                     statement_list)
    
    block_statement = BlockStatement(while_statement)

    expected_output_tokens: Tokens = (end_of_file_token,) 
    expected_output = NodeSuccess(expected_output_tokens, block_statement)

    node_result: NodeResult = parse_tokens_for_block_statement(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_while_statement_with_empty_body():
    """
    This test checks if the function `parse_tokens_for_while_statement` can
    parse the tokens when provided with a while loop without a body.
    """

    tokens: Tokens = (
        while_token(),
        left_parenthesis_token(),
        false_token(),
        right_parenthesis_token(),
        left_curly_brace_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    comp_expression: ComparisonExpression = generate_single_comp_expression(
        "FALSE"
    )
    
    statement_list = StatementList()

    while_statement = WhileStatement(comp_expression, statement_list)
    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, while_statement)

    node_result: NodeResult = parse_tokens_for_while_statement(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_error_for_while_statement_with_faulty_condition():
    """
    This test checks if the function `parse_tokens_for_while_statement` can
    produce the correct error for a while statement with a syntax error in its
    condition.
    """

    tokens: Tokens = (
        while_token(1),
        left_parenthesis_token(1),
        minus_token(1),
        right_parenthesis_token(),
        left_curly_brace_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.MINUS, 1)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_while_statement(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_while_statement_with_non_empty_body():
    """
    This test checks if the function `parse_tokens_for_while_statement` can
    parse the tokens when provided with a while with a non-empty body/
    """
 
    tokens: Tokens = (
        while_token(),
        left_parenthesis_token(),
        false_token(),
        right_parenthesis_token(),
        left_curly_brace_token(),
        return_token(),
        semi_colon_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    comp_expression: ComparisonExpression = generate_single_comp_expression("FALSE")
    
    return_statement = ReturnStatement()
    inline_statement = InlineStatement(return_statement)
    statement_list = StatementList(inline_statement)

    while_statement = WhileStatement(comp_expression, statement_list)
    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, while_statement)

    node_result: NodeResult = parse_tokens_for_while_statement(tokens)

    assert expected_output == node_result

   
def test_parser_can_generate_correct_error_for_while_statement_with_faulty_body():
    """
    This test checks if the function `parse_tokens_for_while_staement` can
    produce the correct error for a while statement with a syntax error in its
    body.
    """

    IDENTIFIER = "variable"

    tokens: Tokens = (
        while_token(1),
        left_parenthesis_token(1),
        true_token(1),
        right_parenthesis_token(1),
        left_curly_brace_token(1),
        identifier_token(IDENTIFIER, 2),
        minus_token(2),
        divide_token(2),
        semi_colon_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.DIVIDE, 2)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_while_statement(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_if_statement_with_empty_body():
    """
    This test checks if the function `parse_tokens_for_if_statement` can parse
    the tokens when provided with an if statement without a body.
    """
 
    tokens: Tokens = (
        if_token(),
        left_parenthesis_token(),
        false_token(),
        right_parenthesis_token(),
        left_curly_brace_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    comp_expression: ComparisonExpression = generate_single_comp_expression(
        "FALSE"
    )
    
    statement_list = StatementList()

    if_statement = IfStatement(comp_expression, statement_list)
    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, if_statement)

    node_result: NodeResult = parse_tokens_for_if_statement(tokens)

    assert expected_output == node_result   


def test_parser_can_generate_error_for_if_statement_with_faulty_condition():
    """
    This test checks if the function `parse_tokens_for_if_statement` can
    produce the correct error for a while statement with a syntax error in its
    condition.
    """

    tokens: Tokens = (
        if_token(1),
        left_parenthesis_token(1),
        minus_token(1),
        right_parenthesis_token(),
        left_curly_brace_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.MINUS, 1)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_if_statement(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_if_statement_with_non_empty_body():
    """
    This test checks if the function `parse_tokens_for_if_statement` can
    parse the tokens when provided with a if with a non-empty body/
    """

    tokens: Tokens = (
        if_token(),
        left_parenthesis_token(),
        false_token(),
        right_parenthesis_token(),
        left_curly_brace_token(),
        return_token(),
        false_token(),
        semi_colon_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    comp_expression: ComparisonExpression = generate_single_comp_expression("FALSE")

    return_statement = ReturnStatement(comp_expression)
    inline_statement = InlineStatement(return_statement)
    statement_list = StatementList(inline_statement)

    if_statement = IfStatement(comp_expression, statement_list)
    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, if_statement)

    node_result: NodeResult = parse_tokens_for_if_statement(tokens)

    assert expected_output == node_result
 

def test_parser_can_generate_correct_error_for_if_statement_with_faulty_body():
    """
    This test checks if the function `parse_tokens_for_if_staement` can
    produce the correct error for a if statement with a syntax error in its
    body.
    """

    IDENTIFIER = "variable"

    tokens: Tokens = (
        if_token(1),
        left_parenthesis_token(1),
        true_token(1),
        right_parenthesis_token(1),
        left_curly_brace_token(1),
        identifier_token(IDENTIFIER, 2),
        minus_token(2),
        divide_token(2),
        semi_colon_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.DIVIDE, 2)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_if_statement(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_if_else_if_statement():
    """
    This test checks if the function `parse_tokens_for_if_statement` can parse
    the tokens when provided with an if statement with an else if clause.
    """

    IDENTIFIER = "y"

    tokens: Tokens = (
        if_token(),
        left_parenthesis_token(),
        true_token(),
        right_parenthesis_token(),
        left_curly_brace_token(),
        right_curly_brace_token(),
        else_token(),
        if_token(),
        left_parenthesis_token(),
        true_token(),
        equals_token(),
        equals_token(),
        false_token(),
        right_parenthesis_token(),
        left_curly_brace_token(),
        identifier_token(IDENTIFIER),
        plus_token(),
        plus_token(),
        semi_colon_token(),
        right_curly_brace_token(),
        end_of_file_token
    )
    
    true_factor = FactorNode("TRUE")
    true_term = TermNode(true_factor)
    true_expression = ExpressionNode(true_term)
    true_comp_expression = ComparisonExpression(true_expression)

    if_statement_statement_list = StatementList()

    false_factor = FactorNode("FALSE")
    false_term = TermNode(false_factor)
    false_expression = ExpressionNode(false_term)
    else_if_comp_expression = ComparisonExpression(
        true_expression,
        ComparisonOperator.BOOLEAN_EQUAL,
        false_expression
    )

    increment_factor = FactorNode("1")
    increment_term = TermNode(increment_factor)
    increment_expression = ExpressionNode(increment_term)
    increment_comp_expression = ComparisonExpression(increment_expression)

    variable_increment = VariableIncrement(IDENTIFIER,
                                           increment_comp_expression)

    inline_statement = InlineStatement(variable_increment)
    else_if_statement_list = StatementList(inline_statement)

    else_if_statement = IfStatement(
        else_if_comp_expression,
        else_if_statement_list
    )

    if_statement = IfStatement(
        true_comp_expression,
        if_statement_statement_list,
        additional_if_statement=else_if_statement
    )

    expected_output_tokens: Tokens = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, if_statement)

    node_result: NodeResult = parse_tokens_for_if_statement(tokens)

    assert expected_output == node_result


def test_parser_can_produce_correct_error_for_faulty_if_else_if_statement():
    """
    This test checks if the function `parse_tokens_for_if_statement` can
    produce the correct error for a faulty input that fails its else if
    statement.
    """

    tokens: Tokens = (
        if_token(1),
        left_parenthesis_token(1),
        false_token(1),
        right_parenthesis_token(1),
        left_curly_brace_token(1),
        right_curly_brace_token(2),
        else_token(3),
        if_token(3),
        left_parenthesis_token(3),
        right_parenthesis_token(3),
        left_curly_brace_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(
        TokenType.RIGHT_PARENTHESIS, 3
    )
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_if_statement(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_if_else_statement():
    """
    This test checks if the function `parse_tokens_for_if_statement` can parse
    the tokens when provided with an if statement with an else clause.
    """

    tokens: Tokens = (
        if_token(),
        left_parenthesis_token(),
        false_token(),
        right_parenthesis_token(),
        left_curly_brace_token(),
        return_token(),
        semi_colon_token(),
        right_curly_brace_token(),
        else_token(),
        left_curly_brace_token(),
        return_token(),
        semi_colon_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    false_factor = FactorNode("FALSE")
    false_term = TermNode(false_factor)
    false_expression = ExpressionNode(false_term)
    if_statement_comp_expression = ComparisonExpression(false_expression)

    return_statement = ReturnStatement()
    inline_statement = InlineStatement(return_statement)
    statement_list = StatementList(inline_statement)

    if_statement = IfStatement(
        if_statement_comp_expression,
        statement_list,
        else_clause=statement_list
    )

    expected_output_tokens: Tokens = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, if_statement)
    
    node_result: NodeResult = parse_tokens_for_if_statement(tokens)

    assert expected_output == node_result


def test_parser_can_produce_correct_error_for_fauly_if_else_statement():
    """
    This test checks if the function `parse_tokens_for_if_statement` can
    produce the correct error for a faulty input that fails its else statement.
    """

    tokens: Tokens = (
        if_token(1),
        left_parenthesis_token(1),
        false_token(1),
        right_parenthesis_token(1),
        left_curly_brace_token(1),
        right_curly_brace_token(2),
        else_token(3),
        left_curly_brace_token(3),
        semi_colon_token(4),
        right_curly_brace_token(5),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.SEMI_COLON, 4)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_if_statement(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_if_else_if_else_statement():
    """
    This test checks if the function `parse_tokens_for_if_statement` can parse
    the tokens when provided with an if statement with an else if clause and
    an else clause.
    """

    IDENTIFIER = "pt"

    tokens: Tokens = (
        if_token(),
        left_parenthesis_token(),
        true_token(),
        right_parenthesis_token(),
        left_curly_brace_token(),
        right_curly_brace_token(),
        else_token(),
        if_token(),
        left_parenthesis_token(),
        true_token(),
        equals_token(),
        equals_token(),
        false_token(),
        right_parenthesis_token(),
        left_curly_brace_token(),
        identifier_token(IDENTIFIER),
        plus_token(),
        plus_token(),
        semi_colon_token(),
        right_curly_brace_token(),
        else_token(),
        left_curly_brace_token(),
        return_token(),
        semi_colon_token(),
        right_curly_brace_token(),
        end_of_file_token
    )
    
    true_factor = FactorNode("TRUE")
    true_term = TermNode(true_factor)
    true_expression = ExpressionNode(true_term)
    true_comp_expression = ComparisonExpression(true_expression)

    if_statement_statement_list = StatementList()

    false_factor = FactorNode("FALSE")
    false_term = TermNode(false_factor)
    false_expression = ExpressionNode(false_term)
    else_if_comp_expression = ComparisonExpression(
        true_expression,
        ComparisonOperator.BOOLEAN_EQUAL,
        false_expression
    )

    increment_factor = FactorNode("1")
    increment_term = TermNode(increment_factor)
    increment_expression = ExpressionNode(increment_term)
    increment_comp_expression = ComparisonExpression(increment_expression)

    variable_increment = VariableIncrement(IDENTIFIER, increment_comp_expression)

    inline_statement = InlineStatement(variable_increment)
    else_if_statement_list = StatementList(inline_statement)

    return_statement = ReturnStatement()
    return_inline_statement = InlineStatement(return_statement)
    else_statement = StatementList(return_inline_statement)

    else_if_statement = IfStatement(
        else_if_comp_expression,
        else_if_statement_list,
        else_clause=else_statement,
    )

    if_statement = IfStatement(
        true_comp_expression,
        if_statement_statement_list,
        additional_if_statement=else_if_statement
    )

    expected_output_tokens: Tokens = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, if_statement)

    node_result: NodeResult = parse_tokens_for_if_statement(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_block_statement_body():
    """
    This test checks if the function `parse_tokens_for_block_statement_body`
    can correctly parse the tokens when provided with the correct input.
    """
   
    IDENTIFIER = "x"
    DECIMAL_LITERAL_VALUE = "89"

    tokens: Tokens = (
        left_curly_brace_token(),
        double_token(),
        identifier_token(IDENTIFIER),
        equals_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        semi_colon_token(),
        right_curly_brace_token(),
        end_of_file_token
    )
    
    comp_expression: ComparisonExpression = generate_single_comp_expression(
        DECIMAL_LITERAL_VALUE
    )

    variable_initialization = VariableInitialization(IDENTIFIER, comp_expression)

    inline_statement = InlineStatement(variable_initialization)
    statement_list = StatementList(inline_statement)

    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, statement_list)

    node_result: NodeResult = parse_tokens_for_block_statement_body(tokens)

    assert expected_output == node_result


def test_parser_can_produce_error_for_block_statement_without_left_brace():
    """
    This test checks if the function `parse_tokens_for_block_statement_body`
    can produce the correct error when given an input that omits the opening/left
    curly brace.
    """

    IDENTIFIER = "Z"
    DECIMAL_LITERAL_VALUE = "699"

    tokens: Tokens = (
        int_token(1),
        identifier_token(IDENTIFIER),
        equals_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        semi_colon_token(),
        right_curly_brace_token(),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.INT, 1)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_block_statement_body(tokens)
    
    assert expected_output == node_result
    

def test_parser_can_produce_error_for_block_statement_without_right_brace():
    """
    This test checks if the function `parse_tokens_for_block_statement_body`
    can produce the correct error when given an input that omits the opening/left
    curly brace.
    """

    IDENTIFIER = "X"
    DECIMAL_LITERAL_VALUE = "88"

    tokens: Tokens = (
        left_curly_brace_token(1),
        double_token(2),
        identifier_token(IDENTIFIER, 2),
        equals_token(2),
        decimal_literal_token(DECIMAL_LITERAL_VALUE, 2),
        semi_colon_token(2),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.END_OF_FILE, -1)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_block_statement_body(tokens)
    
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

    tokens: Tuple[Token, Token, Token] = (
        return_token(),
        semi_colon_token(),
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

    IDENTIFIER = "yolo"
    DECIMAL_LITERAL_VALUE = "78"

    tokens: Tokens = (
        char_token(),
        identifier_token(IDENTIFIER),
        equals_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        semi_colon_token(),
        end_of_file_token
    )

    comparison_expression: ComparisonExpression = generate_single_comp_expression(
        DECIMAL_LITERAL_VALUE
    )

    variable_initialization = VariableInitialization(IDENTIFIER,
                                                     comparison_expression)

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

    IDENTIFIER = "YOlo"
    DECIMAL_LITERAL_VALUE = "66"

    tokens: Tokens = (
        int_token(1),
        identifier_token(IDENTIFIER, 1),
        equals_token(1),
        decimal_literal_token(DECIMAL_LITERAL_VALUE, 1),
        divide_token(1),
        decimal_literal_token(DECIMAL_LITERAL_VALUE, 1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.END_OF_FILE, -1)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_inline_statement(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_for_variable_increment_for_plus_plus():
    """
    This test checks that the parser can correctly generate an ast for
    incrementing a variable.

    This test checks for the ++ operator
    """
    
    IDENTIFIER = "yoworld"

    tokens: Tokens = (
        identifier_token(IDENTIFIER),
        plus_token(),
        plus_token(),
        semi_colon_token(),
        end_of_file_token
    )

    DEFAULT_INCREMENT = "1"

    comp_expression: ComparisonExpression = generate_single_comp_expression(
        DEFAULT_INCREMENT
    )

    variable_increment = VariableIncrement(IDENTIFIER, comp_expression)
    
    expected_output_tokens: Tokens = (
        semi_colon_token(),
        end_of_file_token
    )
    expected_output = NodeSuccess(expected_output_tokens, variable_increment)

    node_result: NodeResult = parse_tokens_for_variable_increment(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_increment_with_expression():
    """
    This test checks that the parser can generate the correct VariableIncrement
    object when supplied with an expression like "x += 1" opposed to "x++"
    """

    IDENTIFIER = "yowoRLd"
    DECIMAL_LITERAL_VALUE = "91"

    tokens: Tokens = (
        identifier_token(IDENTIFIER),
        plus_token(),
        equals_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        semi_colon_token(),
        end_of_file_token
    )

    comp_expression: ComparisonExpression = generate_single_comp_expression(
        DECIMAL_LITERAL_VALUE
    ) 

    variable_increment = VariableIncrement(IDENTIFIER, comp_expression)
    
    expected_output_tokens: Tokens = (
        semi_colon_token(),
        end_of_file_token
    )
    expected_output = NodeSuccess(expected_output_tokens, variable_increment)

    node_result: NodeResult = parse_tokens_for_variable_increment(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_error_given_bad_expression_to_increment():
    """
    This test checks that the parser can correctly generate an error using the
    `parse_tokens_for_variable_increment` function when given an expression that
    contains a syntax error like "x += /9".
    """

    IDENTIFIER = "yy"
    DECIMAL_LITERAL_VALUE = "74"

    tokens: Tokens = (
        identifier_token(IDENTIFIER, 1),
        plus_token(1),
        equals_token(1),
        decimal_literal_token(DECIMAL_LITERAL_VALUE, 1),
        plus_token(1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.END_OF_FILE, -1)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_variable_increment(tokens)
    
    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_empty_return_statement():
    """
    This test checks that the function `parse_tokens_for_return_statement`
    can return the correct ReturnStatement object when given an empty return
    statement (i.e. returning without an expression provided)
    """

    tokens: Tuple[Token, Token, Token] = (
        return_token(),
        semi_colon_token(),
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

    DECIMAL_LITERAL_VALUE = "86"

    tokens: Tokens = (
        return_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        plus_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        semi_colon_token(),
        end_of_file_token
    )

    factor = FactorNode(DECIMAL_LITERAL_VALUE)
    term = TermNode(factor)

    additional_expression = ExpressionNode(term)
    expression = ExpressionNode(term, ArithmeticOperator.PLUS,
                                additional_expression)
    comparison_expression = ComparisonExpression(expression)

    return_statement = ReturnStatement(comparison_expression)

    expected_output_tokens: Tuple[Token, Token] = (
        semi_colon_token(),
        end_of_file_token
    )
    expected_output = NodeSuccess(expected_output_tokens, return_statement)

    node_result: NodeResult = parse_tokens_for_return_statement(tokens)

    assert expected_output == node_result


def test_parse_can_generate_correct_error_given_invalid_expression_to_return():
    """
    This test cehcks that the function `parse_tokens_for_return_statement` can
    return the correct error when given an invalid expression to return.
    """
    
    tokens: Tuple[Token, Token, Token] = (
        return_token(1),
        return_token(1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.RETURN, 1)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_return_statement(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_variable_initialization():
    """
    This test checks that the parser can correctly generate a
    VariableInitialization object using the
    `parse_tokens_for_variable_initilization` function
    """

    IDENTIFIER = "myvar"
    DECIMAL_LITERAL_VALUE = "63"

    tokens: Tokens = (
        double_token(),
        identifier_token(IDENTIFIER),
        equals_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        semi_colon_token(),
        end_of_file_token
    )

    comparison_expression: ComparisonExpression = generate_single_comp_expression(
        DECIMAL_LITERAL_VALUE
    )
    
    variable_intialization = VariableInitialization(
        IDENTIFIER,
        comparison_expression
    )

    expected_output_tokens: Tuple[Token, Token] = (
        semi_colon_token(),
        end_of_file_token
    )
    expected_output = NodeSuccess(expected_output_tokens, variable_intialization)

    node_result: NodeResult = parse_tokens_for_variable_initialization(tokens)

    assert expected_output == node_result


def test_parser_can_return_correct_error_for_initialization_when_expects_equals():
    """
    This test checks that the parser's function
    `parse_tokens_for_variable_initialization` can return the correct NodeFailure
    object when given a tuple of tokens that omits the equals sign. 
    """

    IDENTIFIER = "errorprone"

    tokens: Tuple[Token, Token, Token] = (
        char_token(1),
        identifier_token(IDENTIFIER, 1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.END_OF_FILE, -1)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_variable_initialization(tokens)

    assert expected_output == node_result


def test_parser_can_return_correct_error_for_initialization_when_expects_expression():
    """
    This test checks the function `parse_tokens_for_variable_initialization`
    can return the correct NodeFailure object when given a list of tokens
    that omits an expression (which is required).
    """

    IDENTIFIER = "anju"

    tokens: Tuple[Token, Token, Token, Token] = (
        char_token(1),
        identifier_token(IDENTIFIER, 1),
        equals_token(1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.END_OF_FILE, -1)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_variable_initialization(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_simple_comparison_expression():
    """
    This test checks if the function `parse_tokens_for_comparison_expression`
    can correctly parse the tokens and construct a ComparisonExpression object
    given an input that has a singular expression without an operator or
    additional expression.
    """

    tokens: Tuple[Token, Token, Token] = (
        true_token(),
        right_parenthesis_token(),
        end_of_file_token
    )

    comparison_expression: ComparisonExpression = generate_single_comp_expression(
        "TRUE"
    )

    expected_output_tokens: Tuple[Token, Token] = tokens[1:]
    expected_output = NodeSuccess(expected_output_tokens, comparison_expression)

    node_result: NodeResult = parse_tokens_for_comparison_expression(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_comparison_with_one_expression():
    """
    This test checks if the function `parse_tokens_for_comparison_expression`
    can correctly parse the tokenbs and construct a ComparisonExpression object
    given an input with one expression that may have multiple factors and terms.
    """

    IDENTIFIER = "LOOKATME"
    DECIMAL_LITERAL_VALUE = "2701"

    tokens: Tokens = (
        identifier_token(IDENTIFIER),
        multiply_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        right_parenthesis_token(),
        end_of_file_token
    )

    qualified_identifier = QualifiedIdentifier(IDENTIFIER)
    factor = FactorNode(qualified_identifier=qualified_identifier)

    factor_for_additional_term = FactorNode(DECIMAL_LITERAL_VALUE)
    additional_term = TermNode(factor_for_additional_term)
    
    term = TermNode(factor, ArithmeticOperator.MULTIPLY, additional_term)
    expression = ExpressionNode(term)

    comparison_expression = ComparisonExpression(expression)

    expected_output_tokens: Tuple[Token, Token] = (
        right_parenthesis_token(),
        end_of_file_token
    )
    expected_output = NodeSuccess(expected_output_tokens, comparison_expression)

    node_result: NodeResult = parse_tokens_for_comparison_expression(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_complex_comparison_expression():
    """
    This test checks if the function `parse_tokens_for_comparison_expression`
    can correctly parse the tokens and construct a ComparisonExpression object
    given an input with an operator and an additional expression.
    """

    tokens: Tokens = (
        true_token(),
        exclamation_token(),
        equals_token(),
        false_token(),
        right_parenthesis_token(),
        end_of_file_token
    )

    factor = FactorNode("TRUE")
    term = TermNode(factor)
    expression = ExpressionNode(term)

    additional_factor = FactorNode("FALSE")
    additional_term = TermNode(additional_factor)
    additional_expression = ExpressionNode(additional_term)

    comparison_expression = ComparisonExpression(expression,
                                                 ComparisonOperator.NOT_EQUAL,
                                                 additional_expression)
    
    expected_output_tokens: Tuple[Token, Token] = (
        right_parenthesis_token(),
        end_of_file_token
    )
    expected_output = NodeSuccess(expected_output_tokens, comparison_expression)

    node_result: NodeResult = parse_tokens_for_comparison_expression(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_error_for_complex_comparison_expression():
    """
    This test checks if the function `parse_tokens_for_comparison_expression`
    can correctly generate an error for a complex expression with a syntax error
    """
    
    DECIMAL_LITERAL_VALUE = "31"

    tokens: Tokens = (
        decimal_literal_token(DECIMAL_LITERAL_VALUE, 1),
        less_than_token(1),
        plus_token(1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.PLUS, 1)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_comparison_expression(tokens)

    assert expected_output == node_result


def test_parser_can_produce_error_for_comp_expression_with_no_left_paren():
    """
    This test checks if the function `parse_tokens_for_expression_in_paren` can
    parse the tokens when provided with a comparison expression with no left
    parenthesis.
    """

    tokens: Tokens = (
        true_token(1),
        right_parenthesis_token(1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.TRUE, 1)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_expression_in_paren(tokens)
    
    assert expected_output == node_result


def test_parser_can_produce_error_for_comp_expression_with_no_right_paren():
    """
    This test checks if the function `parse_tokens_for_expression_in_paren` can
    parse the tokens when provided with a comparison expression with no right
    parenthesis.
    """

    tokens: Tokens = (
        left_parenthesis_token(1),
        false_token(1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.END_OF_FILE, -1)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_expression_in_paren(tokens)
    
    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_simple_expression():
    """
    This test checks if the parser can properly generate an AST for a simple
    expression with a singular term and that term has a singular factor.
    """

    DECIMAL_LITERAL_VALUE = "64"
    
    tokens: Tuple[Token, Token] = (
            decimal_literal_token(DECIMAL_LITERAL_VALUE),
        end_of_file_token
    )

    factor_node = FactorNode(DECIMAL_LITERAL_VALUE)
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
    
    DECIMAL_LITERAL_VALUE = "24"

    tokens: Tuple[Token, Token, Token, Token] = (
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        multiply_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        end_of_file_token
    )

    single_factor = FactorNode(DECIMAL_LITERAL_VALUE)
    multiply_operator = ArithmeticOperator.MULTIPLY
    
    factor_for_additional_term = FactorNode(DECIMAL_LITERAL_VALUE)
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

    DECIMAL_LITERAL_VALUE = "7"

    tokens: Tuple[Token, Token, Token, Token] = (
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        plus_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        end_of_file_token
    )

    factor = FactorNode(DECIMAL_LITERAL_VALUE)
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

    DECIMAL_LITERAL_VALUE = "5"

    tokens: Tuple[Token, Token, Token, Token] = (
        decimal_literal_token(DECIMAL_LITERAL_VALUE, 1),
        plus_token(1),
        minus_token(1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.MINUS, 1)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_expression(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_simple_term():
    """
    This test checks if the parser can successfully generate an ast when given
    a single term. This test specifically checks the `parse_tokens_for_term`
    function.
    """

    DECIMAL_LITERAL_VALUE = "90"

    tokens: Tuple[Token, Token] = (
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        end_of_file_token
    )

    factor_node = FactorNode(DECIMAL_LITERAL_VALUE)
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

    FIRST_DECIMAL_LITERAL_VALUE = "1"
    SECOND_DECIMAL_LITERAL_VALUE = "2"

    tokens: Tuple[Token, Token, Token, Token] = (
        decimal_literal_token(FIRST_DECIMAL_LITERAL_VALUE),
        multiply_token(),
        decimal_literal_token(SECOND_DECIMAL_LITERAL_VALUE),
        end_of_file_token
    )

    single_factor = FactorNode(FIRST_DECIMAL_LITERAL_VALUE)
    
    factor_for_additional_term = FactorNode(SECOND_DECIMAL_LITERAL_VALUE)
    additional_term = TermNode(factor_for_additional_term)

    term_node = TermNode(single_factor, ArithmeticOperator.MULTIPLY,
                         additional_term)

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

    FIRST_DECIMAL_LITERAL_VALUE = "101"
    SECOND_DECIMAL_LITERAL_VALUE = "20"

    tokens: Tuple[Token, Token, Token, Token] = (
        decimal_literal_token(FIRST_DECIMAL_LITERAL_VALUE),
        divide_token(),
        decimal_literal_token(SECOND_DECIMAL_LITERAL_VALUE),
        end_of_file_token
    )

    single_factor = FactorNode(FIRST_DECIMAL_LITERAL_VALUE)
    
    factor_for_additional_term = FactorNode(SECOND_DECIMAL_LITERAL_VALUE)
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

    DECIMAL_LITERAL_VALUE = "3"

    tokens: Tuple[Token, Token, Token, Token] = (
        decimal_literal_token(DECIMAL_LITERAL_VALUE, 1),
        divide_token(1),
        divide_token(1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.DIVIDE, 1)
    expected_output = NodeFailure(error_message)

    node_output: NodeResult = parse_tokens_for_term(tokens)

    assert expected_output == node_output 


def test_parser_can_generate_correct_ast_for_multiple_terms():
    """
    This test checks that the parser can properly generate an AST for multiple
    terms at once
    """
 
    DECIMAL_LITERAL_VALUE = "33"

    tokens: Tokens = (
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        multiply_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        divide_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        end_of_file_token
    )

    first_factor = FactorNode(DECIMAL_LITERAL_VALUE)
    second_factor = FactorNode(DECIMAL_LITERAL_VALUE)
    third_factor = FactorNode(DECIMAL_LITERAL_VALUE)
    
    additional_term_of_additional_term = TermNode(third_factor)
    additional_term = TermNode(second_factor, ArithmeticOperator.DIVIDE,
                               additional_term_of_additional_term)
    
    term = TermNode(first_factor, ArithmeticOperator.MULTIPLY,
                              additional_term)


    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, term)

    node_result: NodeResult = parse_tokens_for_term(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_single_factor_with_decimal_literal():
    """
    This test checks if the function `parse_tokens_for_factor` returns the
    correct FactorNode object using a decimal literal.
    """

    DECIMAL_LITERAL_VALUE = "12"

    tokens: Tuple[Token, Token] = (
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        end_of_file_token
    )

    factor_node = FactorNode(DECIMAL_LITERAL_VALUE)
    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, factor_node)

    node_output: NodeResult = parse_tokens_for_factor(tokens)

    assert expected_output == node_output 


def test_parser_can_generate_correct_ast_for_single_factor_with_true_token():
    """
    This test checks if the function `parse_tokens_for_factor` returns the
    correct FactorNode object using a true_token.
    """

    tokens: Tuple[Token, Token] = (
        true_token(),
        end_of_file_token
    )

    factor_node = FactorNode("TRUE")
    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, factor_node)

    node_output: NodeResult = parse_tokens_for_factor(tokens)

    assert expected_output == node_output 


def test_parser_can_generate_correct_ast_for_single_factor_with_false_token():
    """
    This test checks if the function `parse_tokens_for_factor` returns the
    correct FactorNode object using a false_token.
    """

    tokens: Tuple[Token, Token] = (
        false_token(),
        end_of_file_token
    )

    factor_node = FactorNode("FALSE")
    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, factor_node)

    node_output: NodeResult = parse_tokens_for_factor(tokens)

    assert expected_output == node_output 


def test_parser_can_generate_correct_ast_for_single_factor_with_identifier():
    """
    This test checks if the function `parse_tokens_for_factor` returns the
    correct FactorNode object using a identifier token.
    """

    IDENTIFIER = "RaidenShuga"

    tokens: Tuple[Token, Token] = (
        identifier_token(IDENTIFIER),
        end_of_file_token
    )

    qualified_identifier = QualifiedIdentifier(IDENTIFIER)
    factor_node = FactorNode(qualified_identifier=qualified_identifier)
    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, factor_node)

    node_output: NodeResult = parse_tokens_for_factor(tokens)

    assert expected_output == node_output 


def test_parser_can_generate_correct_ast_for_single_factor_with_string_literal():
    """
    This test checks if the function `parse_tokens_for_factor` returns the
    correct FactorNode object using a string literal token.
    """

    STRING_LITERAL_VALUE = "raidenShuga"

    tokens: Tuple[Token, Token] = (
        string_literal_token(STRING_LITERAL_VALUE),
        end_of_file_token
    )

    factor_node = FactorNode(STRING_LITERAL_VALUE)
    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, factor_node)

    node_output: NodeResult = parse_tokens_for_factor(tokens)

    assert expected_output == node_output 


def test_parser_can_generate_correct_error_for_factor():
    """
    This test checks if the function `parse_tokens_for_factor` returns the
    correct NodeFailure object.
    """

    tokens: Tuple[Token, Token] = (
        plus_token(1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.PLUS, 1)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_factor(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_single_identifier():
    """
    This test checks that the parser can correctly generate a QualifiedIdentifier
    object using the `parse_tokens_for_qualified_identifier` function when given
    a single identifier.
    """

    IDENTIFIER = "hlelo"

    tokens: Tokens = (
        identifier_token(IDENTIFIER),
        end_of_file_token
    )

    qualified_identifier = QualifiedIdentifier(IDENTIFIER)
    expected_output = NodeSuccess(tokens[1:], qualified_identifier)

    node_result: NodeResult = parse_tokens_for_qualified_identifier(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_error_for_omitted_additional_identifier():
    """
    This test checks that the parser can correctly generate an error using the
    `parse_tokens_for_qualified_identifier` function when omitting the
    additional identifier.
    
    Ex: myident.
    ^^^^^^^^^^^ <-- there's only a dot; not additional identifier
    """

    IDENTIFIER = "errORRRRR"
    
    tokens: Tokens = (
        identifier_token(IDENTIFIER, 1),
        period_token(1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.END_OF_FILE, -1)
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_qualified_identifier(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_qualified_identifier():
    """
    This test checks that the parser can correctly generate a QualifiedIdentifier
    object using the `parse_tokens_for_qualified_identifier` function when given
    a QUALIFIED identifier.
    """

    IDENTIFIER = "System"
    ADDITIONAL_IDENTIFER = "out"

    tokens: Tokens = (
        identifier_token(IDENTIFIER, 1),
        period_token(1),
        identifier_token(ADDITIONAL_IDENTIFER, 1),
        end_of_file_token
    )

    additional_identifier = QualifiedIdentifier(ADDITIONAL_IDENTIFER)
    qualified_identifier = QualifiedIdentifier(IDENTIFIER, additional_identifier)

    expected_output_tokens: Tokens = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, qualified_identifier)

    node_result: NodeResult = parse_tokens_for_qualified_identifier(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_no_argument_method_call():
    """
    This test checks that the parser can correctly generate a MethodCall object
    using the `parse_tokens_for_method_call` function.
    """

    IDENTIFIER = "func"

    tokens: Tokens = (
        left_parenthesis_token(),
        right_parenthesis_token(),
        end_of_file_token
    )

    qualified_identifier = QualifiedIdentifier(IDENTIFIER)
    argument_list = ArgumentList()
    method_call = MethodCall(qualified_identifier, argument_list)

    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, method_call)

    node_result: NodeResult = parse_tokens_for_method_call(tokens,
                                                           qualified_identifier)

    assert expected_output == node_result


def test_parser_can_generate_correct_error_for_method_call_when_expecting_parenthesis():
    """
    This test checks if the function `parse_tokens_for_method_call` returns the
    correct NodeFailure object when given an input that has a syntax error.
    """

    IDENTIFIER = "heELLO"

    tokens: Tokens = (
        left_parenthesis_token(1),
        plus_token(1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(TokenType.PLUS, 1)
    expected_output = NodeFailure(error_message)

    qualified_identifier = QualifiedIdentifier(IDENTIFIER)

    node_result: NodeResult = parse_tokens_for_method_call(tokens,
                                                           qualified_identifier)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_single_argument_method_call():
    """
    This test checks if the function `parse_tokens_for_method_call` returns the
    correct MethodCall object when given a single argument.
    """

    IDENTIFIER = "tok"
    DECIMAL_LITERAL_VALUE = "10"

    tokens: Tokens = (
        left_parenthesis_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        right_parenthesis_token(),
        end_of_file_token
    )

    comparison_expression: ComparisonExpression = generate_single_comp_expression(
        DECIMAL_LITERAL_VALUE
    )

    qualified_identifier = QualifiedIdentifier(IDENTIFIER)
    argument_list = ArgumentList(comparison_expression)
    method_call = MethodCall(qualified_identifier, argument_list)

    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, method_call)

    node_result: NodeResult = parse_tokens_for_method_call(tokens,
                                                           qualified_identifier)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_multiple_arguments_for_method_call():
    """
    This test checks if the function `parse_tokens_for_method_call` returns the
    correct MethodCall object when given a tuple of multiple arguments as input.
    """

    IDENTIFIER = "raiden"
    DECIMAL_LITERAL_VALUE = "42"

    tokens: Tokens = (
        left_parenthesis_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        comma_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        right_parenthesis_token(),
        end_of_file_token
    )

    comparison_expression: ComparisonExpression = generate_single_comp_expression(
        DECIMAL_LITERAL_VALUE
    )

    qualified_identifier = QualifiedIdentifier(IDENTIFIER)
    additional_argument_list = ArgumentList(comparison_expression)
    argument_list = ArgumentList(comparison_expression, additional_argument_list)
    method_call = MethodCall(qualified_identifier, argument_list)

    expected_output_tokens: Tuple[Token] = (end_of_file_token,)
    expected_output = NodeSuccess(expected_output_tokens, method_call)

    node_result: NodeResult = parse_tokens_for_method_call(tokens,
                                                           qualified_identifier)

    assert expected_output == node_result


def test_parser_can_generate_correct_ast_for_argument_list_with_no_arguments():
    """
    This test checks that the parser can correctly generate an ArgumentList object
    using the `parse_tokens_for_argument_list` function when given an input
    without an arguments supplied.
    """

    tokens: Tuple[Token, Token] = (
        right_parenthesis_token(),
        end_of_file_token
    )

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

    DECIMAL_LITERAL_VALUE = "5"

    tokens: Tuple[Token, Token, Token] = (
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        right_parenthesis_token(),
        end_of_file_token
    )

    comparison_expression: ComparisonExpression = generate_single_comp_expression(
        DECIMAL_LITERAL_VALUE
    )

    argument_list = ArgumentList(comparison_expression)
    
    expected_output_tokens: Tuple[Token, Token] = (
        right_parenthesis_token(),
        end_of_file_token
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

    DECIMAL_LITERAL_VALUE = "3"

    tokens: Tokens = (
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        comma_token(),
        decimal_literal_token(DECIMAL_LITERAL_VALUE),
        right_parenthesis_token(),
        end_of_file_token
    )

    comparison_expression: ComparisonExpression = generate_single_comp_expression(
        DECIMAL_LITERAL_VALUE
    )

    additional_argument_list = ArgumentList(comparison_expression)
    argument_list = ArgumentList(comparison_expression, additional_argument_list)
    
    expected_output_tokens: Tuple[Token, Token] = (
        right_parenthesis_token(),
        end_of_file_token
    )
    expected_output = NodeSuccess(expected_output_tokens, argument_list)

    node_result: NodeResult = parse_tokens_for_argument_list(tokens)

    assert expected_output == node_result


def test_parser_can_generate_correct_error_when_argument_list_expects_parenthesis_or_comma():
    """
    This test checks that the parser can correctly generate a NodeFailure object
    when supplied a faulty input that omits either a comma or a parenthesis.
    """

    DECIMAL_LITERAL_VALUE = "3435"

    tokens: Tokens = (
        decimal_literal_token(DECIMAL_LITERAL_VALUE, 1),
        comma_token(1),
        decimal_literal_token(DECIMAL_LITERAL_VALUE, 1),
        decimal_literal_token(DECIMAL_LITERAL_VALUE, 1),
        right_parenthesis_token(1),
        end_of_file_token
    )

    error_message: str = ERROR_MESSAGE_FOR_PARSER.format(
        TokenType.DECIMAL_LITERAL, 1
    )
    expected_output = NodeFailure(error_message)

    node_result: NodeResult = parse_tokens_for_argument_list(tokens)

    assert expected_output == node_result


# -----------------------------------------------------------------------------
# START OF EMITTER TESTS
# -----------------------------------------------------------------------------


def test_emitter_can_produce_correct_output_for_class_declaration():
    """
    This test checks that the emitter can produce the correct output when given
    a ClassDeclaration object.
    """
    
    NUMBER = "86"
    return_comp_expression: ComparisonExpression
    return_comp_expression = generate_single_comp_expression(NUMBER)

    return_statement = ReturnStatement(return_comp_expression)
    inline_statement = InlineStatement(return_statement)
    statement_list = StatementList(inline_statement)

    parameter_list = ParameterList()

    METHOD_IDENTIFIER = "hello"
    method_declaration = MethodDeclaration(
        METHOD_IDENTIFIER, parameter_list, statement_list
    )
    method_declaration_list = MethodDeclarationList(method_declaration)
    CLASS_IDENTIFEIR = "Person"
    emitter_input = ClassDeclaration(
        CLASS_IDENTIFEIR,
        method_declaration_list
    )

    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = (
        f"class {CLASS_IDENTIFEIR}:\n" +
        f"    def {METHOD_IDENTIFIER}():\n" +
        "        return 86\n"
    )

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_method_declaration_list_multiple_methods():
    """
    This test checks that the emitter can produce the correct output when given
    a MethodDeclarationList object with more than one method
    """

    METHOD_IDENTIFIER = "mymethod"
    PARAMETER_1 = "num"
    parameter_list = ParameterList(PARAMETER_1)

    comp_expression: ComparisonExpression = generate_single_comp_expression("0")
    return_statement = ReturnStatement(comp_expression)
    inline_statement = InlineStatement(return_statement)
    statement_list = StatementList(inline_statement)

    method_declaration = MethodDeclaration(
        METHOD_IDENTIFIER, parameter_list, statement_list
    )
    additional_method_declaration_list = MethodDeclarationList(method_declaration) 
    emitter_input = MethodDeclarationList(method_declaration,
                                          additional_method_declaration_list)
    emitter_result: str = emit_ast_into_output(emitter_input)
    expected_output = (
        f"def {METHOD_IDENTIFIER}(num):\n" +
        "    return 0\n" +
        f"def {METHOD_IDENTIFIER}(num):\n" +
        "    return 0\n"
    )
    
    assert expected_output == emitter_result



def test_emitter_can_produce_correct_output_for_one_method_method_declaration_list():
    """
    This test checks that the emitter can produce the correct output when given
    a MethodDeclarationList object with a singular method.
    """

    METHOD_IDENTIFIER = "mymethod"
    PARAMETER_1 = "num"
    parameter_list = ParameterList(PARAMETER_1)

    comp_expression: ComparisonExpression = generate_single_comp_expression("0")
    return_statement = ReturnStatement(comp_expression)
    inline_statement = InlineStatement(return_statement)
    statement_list = StatementList(inline_statement)

    method_declaration = MethodDeclaration(
        METHOD_IDENTIFIER, parameter_list, statement_list
    )
 
    emitter_input = MethodDeclarationList(method_declaration)
    emitter_result: str = emit_ast_into_output(emitter_input)
    expected_output = (
        f"def {METHOD_IDENTIFIER}(num):\n" +
        "    return 0\n"
    )
    
    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_empty_method_declaration_list():
    """
    This test checks that the emitter can produce the correct output when given
    a MethodDeclarationList object with all of its fields set to None.
    """

    emitter_input = MethodDeclarationList()
    emitter_result: str = emit_ast_into_output(emitter_input)
    expected_output = ""
    
    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_method_declaration():
    """
    This test checks that the emitter can produce the correct output when given
    a MethodDeclaration object.
    """

    METHOD_IDENTIFIER = "mymethod"
    parameter_list = ParameterList()

    comp_expression: ComparisonExpression = generate_single_comp_expression("0")
    return_statement = ReturnStatement(comp_expression)
    inline_statement = InlineStatement(return_statement)
    statement_list = StatementList(inline_statement)

    emitter_input = MethodDeclaration(
        METHOD_IDENTIFIER, parameter_list, statement_list
    )
    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = (
        f"def {METHOD_IDENTIFIER}():\n" +
        "    return 0\n"
    )

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_multiple_parameters_parameter_list():
    """
    This test checks that the emitter can produce the correct output when given
    a ParameterList object with multiple parameters
    """

    PARAMETER_3 = "lena"
    parameter_3_list = ParameterList(PARAMETER_3)

    PARAMETER_2 = "shin"
    parameter_2_list = ParameterList(PARAMETER_2, parameter_3_list)

    IDENTIFIER = "kurena"
    emitter_input = ParameterList(IDENTIFIER, parameter_2_list)
    emitter_result: str = emit_ast_into_output(emitter_input)
    expected_output = "kurena, shin, lena"

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_singular_parameter_parameter_list():
    """
    This test checks that the emitter can produce the correct output when given
    a ParameterList object with only its first field, `parameter`, given a field,
    and its other field set to None.
    """

    IDENTIFIER = "kurena"
    emitter_input = ParameterList(IDENTIFIER)
    emitter_result: str = emit_ast_into_output(emitter_input)
    expected_output = IDENTIFIER

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_empty_parameter_list():
    """
    This test checks that the emitter can produce the correct output when given
    a ParameterList object with both of its fields set to None.
    """

    emitter_input = ParameterList()
    emitter_result: str = emit_ast_into_output(emitter_input)
    expected_output = ""

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_statement_list_with_multiple_statements():
    """
    This test checks that the emitter can prodcue the correct output when given
    a StatementList object with both of its fields set to values that are not
    None.
    """

    NUMBER = "86"
    comp_expression_for_additional_statement_list: ComparisonExpression
    comp_expression_for_additional_statement_list = generate_single_comp_expression(
        NUMBER
    )

    inline_statement_for_additional_statement_list = InlineStatement(
        comp_expression_for_additional_statement_list
    )
    additional_statement_list = StatementList(
        inline_statement_for_additional_statement_list
    )

    comp_expression: ComparisonExpression = generate_single_comp_expression("theo")
    IDENTIFIER = "var"
    variable_initialization = VariableInitialization(IDENTIFIER, comp_expression)
    inline_statement = InlineStatement(variable_initialization)
    emitter_input = StatementList(inline_statement, additional_statement_list)
    emitter_result: str = emit_ast_into_output(emitter_input)
    expected_output = (
        "var = theo\n" +
        "86\n"
    )

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_statement_list_with_singular_statement():
    """
    This test checks that the emitter can prodcue the correct output when given
    a StatementList object with only its `statement` field given a value and its
    `additional_statement_list` field set to None
    """

    comp_expression: ComparisonExpression = generate_single_comp_expression("theo")
    IDENTIFIER = "var"
    variable_initialization = VariableInitialization(IDENTIFIER, comp_expression)
    inline_statement = InlineStatement(variable_initialization)
    emitter_input = StatementList(inline_statement)
    emitter_result: str = emit_ast_into_output(emitter_input)
    expected_output = "var = theo\n"

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_empty_statement_list():
    """
    This test checks that the emitter can prodcue the correct output when given
    a StatementList object with both of its fields set to None; thus, making it
    an empty StatementList.
    """

    emitter_input = StatementList()
    emitter_result: str = emit_ast_into_output(emitter_input)
    expected_output = ""

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_block_statement():
    """
    This test checks that the emitter can produce the correct ouptput when given
    a BlockStatement object.
    """

    comp_expression: ComparisonExpression = generate_limited_comp_expression(
        "5", ComparisonOperator.LESS_THAN_OR_EQUAL, "86"
    )
    return_statement = ReturnStatement(comp_expression)
    inline_statement = InlineStatement(return_statement)
    statement_list = StatementList(inline_statement)
    while_statement = WhileStatement(comp_expression, statement_list)
    emitter_input = BlockStatement(while_statement)

    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = (
        "while 5<=86:\n" +
        "    return 5<=86\n"
    )

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_if_else_if_statement():
    """
    This test checks that the emitter can produce the correct output when given
    an IfStatement object for an IfStatement with an additional if statement.
    """

    additional_if_comp_expression: ComparisonExpression
    additional_if_comp_expression = generate_single_comp_expression("cond")
    additional_if_return_statement = ReturnStatement()
    additional_if_inline_statement = InlineStatement(
        additional_if_return_statement
    )
    additional_if_statement_list = StatementList(additional_if_inline_statement)
    additional_if_statement = IfStatement(additional_if_comp_expression,
                                          additional_if_statement_list)

    comp_expression: ComparisonExpression
    comp_expression = generate_single_comp_expression("false")
    return_statement = ReturnStatement()
    inline_statement = InlineStatement(return_statement)
    statement_list = StatementList(inline_statement)
    emitter_input = IfStatement(comp_expression, statement_list,
                                additional_if_statement=additional_if_statement)

    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = (
        "if false:\n" +
        "    return\n" +
        "elif cond:\n" +
        "    return\n"
    )

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_if_else_statement():
    """
    This test checks that the emitter can produce the correct output when given
    an IfStatement object for an IfStatement with an else clause.
    """

    comp_expression: ComparisonExpression
    comp_expression = generate_single_comp_expression("false")
    return_statement = ReturnStatement()
    inline_statement = InlineStatement(return_statement)
    statement_list = StatementList(inline_statement)

    VARIABLE_INCREMENT_IDENTIFIER = "anju"
    variable_increment_comp_expression: ComparisonExpression
    variable_increment_comp_expression = generate_single_comp_expression("12")
    variable_increment = VariableIncrement(
        VARIABLE_INCREMENT_IDENTIFIER, variable_increment_comp_expression
    )
    variable_increment_inline_statement = InlineStatement(variable_increment)
    else_statement_list = StatementList(variable_increment_inline_statement)
    emitter_input = IfStatement(comp_expression, statement_list,
                                else_clause=else_statement_list)

    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = (
        "if false:\n" +
        "    return\n" +
        "else:\n" +
        "    anju += 12\n"
    )

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_a_lone_if_statement():
    """
    This test checks that the emitter can produce the correct output when given
    an IfStatement object for an IfStatement by itself.
    """

    comp_expression = generate_single_comp_expression("false")
    return_statement = ReturnStatement()
    inline_statement = InlineStatement(return_statement)
    statement_list = StatementList(inline_statement)
    emitter_input = IfStatement(comp_expression, statement_list)

    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = (
        "if false:\n" +
        "    return\n"
    )

    assert expected_output == emitter_result





def test_emitter_can_produce_correct_output_for_while_statement():
    """
    This test checks that the emitter can produce the correct output when given
    a WhileStatement object.
    """

    comp_expression = generate_single_comp_expression("true")
    return_statement = ReturnStatement()
    inline_statement = InlineStatement(return_statement)
    statement_list = StatementList(inline_statement)
    emitter_input = WhileStatement(comp_expression, statement_list)

    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = (
        "while true:\n" +
        "    return\n"
    )

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_inline_statement():
    """
    This test checks that the emitter can produce the correct output when given
    an InlineStatement object.
    """

    return_statement = ReturnStatement()
    emitter_input = InlineStatement(return_statement)
    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = f"return\n"

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_variable_increment():
    """
    This test checks that emitter can produce the correct output when given a
    VariableIncrement object.
    """

    IDENTIFIER = "laughingfox"
    NUMBER = "86"
    comp_expression: ComparisonExpression = generate_single_comp_expression(
        NUMBER
    )
    emitter_input = VariableIncrement(IDENTIFIER, comp_expression)

    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = f"{IDENTIFIER} += {NUMBER}"

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_variable_initialization():
    """
    This test checks that the emitter can produce the correct output when given
    a VariableInitialization object.
    """

    IDENTIFIER = "theo"
    NUMBER = "12"
    comp_expression: ComparisonExpression = generate_single_comp_expression(
        NUMBER
    ) 
    emitter_input = VariableInitialization(IDENTIFIER, comp_expression)

    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = f"{IDENTIFIER} = {NUMBER}"

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_empty_return_statement():
    """
    This test checks that the emitter can produce the correct output when given
    a ReturnStatement object with its `comparison_expression` field set to None;
    thus, making it an empty return statement.
    """

    emitter_input = ReturnStatement()
    emitter_result: str = emit_ast_into_output(emitter_input)
    expected_output = "return"
    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_non_empty_return_statement():
    """
    This test checks that the emitter can produce the correct output when given
    a ReturnStatement object with its `comparison_expression` field set not to
    None; thus, making it a return statement with a value.
    """

    IDENTIFIER = "kurena"
    comp_expression: ComparisonExpression = generate_single_comp_expression(
        IDENTIFIER
    )
    emitter_input = ReturnStatement(comp_expression)

    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = f"return {IDENTIFIER}"

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_single_expression_comp_expression():
    """
    This test checks that the emitter can produce the correct output when given
    a ComparisonExpression object with only the `first_expression` field given
    a value and its other fields remaining None.
    """

    NUMBER = "0"
    factor = FactorNode(NUMBER)
    term = TermNode(factor)
    expression = ExpressionNode(term)
    emitter_input = ComparisonExpression(expression)

    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = NUMBER

    assert expected_output == emitter_result 


def test_emitter_can_produce_correct_output_for_complex_comp_expression():
    """
    This test checks that the emitter can produce the correct output when given
    a ComparisonExpression object with all of its fields not set to None; every
    field will be given a value. An example expression may be: 5 <= 3
    """

    FIRST_NUMBER = "2"
    operator = ComparisonOperator.NOT_EQUAL
    SECOND_NUMBER = "93"
    emitter_input: ComparisonExpression = generate_limited_comp_expression(
        FIRST_NUMBER, operator, SECOND_NUMBER
    )

    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = f"{FIRST_NUMBER}!={SECOND_NUMBER}"

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_single_term_expression():
    """
    This test checks that the emitter can produce the correct output when given
    an ExpressionNode object with only the `first_term_node` field given a
    value and its other fields remaining None.
    """
    
    NUMBER = "85"
    factor = FactorNode(NUMBER)
    term = TermNode(factor)
    emitter_input = ExpressionNode(term)
    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = NUMBER

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_complex_expression_node():
    """
    This test checks that the emitter can produce the correct output when given
    an ExpressionNode object with all of its field not set to None; an example
    input may look like "6 + 5 - 1"
    """

    NUMBER = "86"
    number_factor = FactorNode(NUMBER)
    additional_term = TermNode(number_factor)

    IDENTIFIER = "anju"
    identifier_factor = FactorNode(IDENTIFIER)
    complex_term = TermNode(identifier_factor, ArithmeticOperator.MULTIPLY,
                            additional_term)
    additional_expression = ExpressionNode(complex_term)

    ANOTHER_NUMBER = "1"
    another_number_factor = FactorNode(ANOTHER_NUMBER)
    number_term = TermNode(another_number_factor)
    emitter_input = ExpressionNode(number_term, ArithmeticOperator.PLUS,
                                   additional_expression)

    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = f"1+{IDENTIFIER}*{NUMBER}"

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_single_factor_term():
    """
    This test checks that the emitter can produce the correct output when given
    a TermNode object with only it `first_factor_node` field given a value and
    its other fields remaining None.
    """
    
    NUMBER = "85"
    factor = FactorNode(NUMBER)
    emitter_input = TermNode(factor)
    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = NUMBER

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_complex_term_node():
    """
    This test checks that the emitter can produce the correct output when given
    a TermNode object with all of its field not set to None; an example input
    may look like "6 * 3 / 1"
    """

    NUMBER = "86"
    number_factor = FactorNode(NUMBER)
    additional_term = TermNode(number_factor)

    IDENTIFIER = "anju"
    identifier_factor = FactorNode(IDENTIFIER)
    emitter_input = TermNode(identifier_factor, ArithmeticOperator.MULTIPLY,
                             additional_term)
    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = f"{IDENTIFIER}*{NUMBER}"

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_factor_with_no_call_or_ident():
    """
    This test checks that the emitter can produce the correct output when given
    a FactorNode object with a number_or_string field and NOT a method_call
    field OR a qualified_identifier field.
    """

    NUMBER = "86"
    emitter_input = FactorNode(NUMBER)
    emitter_result: str = emit_ast_into_output(emitter_input)
    
    expected_output = NUMBER

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_factor_with_only_qual_identifier():
    """
    This test checks that the emitter can produce the correct output when given
    a FactorNode object with a `qualified_identifier` field and NOT a `method_call`
    field or a `number_or_string` field
    """

    ADDITIONAL_IDENTIFIER = "lena"
    additional_identifier = QualifiedIdentifier(ADDITIONAL_IDENTIFIER)

    IDENTIFIER = "shin"
    qualified_identifier = QualifiedIdentifier(IDENTIFIER, additional_identifier)
    emitter_input = FactorNode(qualified_identifier=qualified_identifier)
    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = f"{IDENTIFIER}.{ADDITIONAL_IDENTIFIER}"

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_factor_node_with_a_method_call():
    """
    This test checks that the emitter can produce the correct output when given
    a FactorNode object with a method_call field and NOT a number_or_string
    field.
    """

    NUMBER = "86"
    argument_comp_expression: ComparisonExpression
    argument_comp_expression = generate_single_comp_expression(NUMBER)
    argument_list = ArgumentList(argument_comp_expression)

    IDENTIFIER = "vladilena"
    qualified_identifier = QualifiedIdentifier(IDENTIFIER)
    method_call = MethodCall(qualified_identifier, argument_list)
    emitter_input = FactorNode(method_call=method_call)

    emitter_result: str = emit_ast_into_output(emitter_input)
    
    expected_output = f"{IDENTIFIER}({NUMBER})"

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_simple_qualified_identifier():
    """
    This test checks that the emitter can produce the correct output when given
    a QualifiedIdentifier object with only the `identifier` field given a value.
    """

    IDENTIFIER = "shin"
    emitter_input = QualifiedIdentifier(IDENTIFIER)
    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = IDENTIFIER

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_complex_qualified_identifier():
    """
    This test checks that the emitter can produce the correct output when given
    a QualifiedIdentifier object with the `identifier` field given a value and
    the `additional_identifier` field given a value.
    """

    ADDITIONAL_IDENTIFIER = "lena"
    additional_identifier = QualifiedIdentifier(ADDITIONAL_IDENTIFIER)

    IDENTIFIER = "shin"
    emitter_input = QualifiedIdentifier(IDENTIFIER, additional_identifier)
    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = f"{IDENTIFIER}.{ADDITIONAL_IDENTIFIER}"

    assert expected_output == emitter_result



def test_emitter_can_produce_correct_output_for_method_call():
    """
    This test checks that the emitter can produce the correct output when given
    a MethodCall object.
    """

    IDENTIFIER = "shinnei"
    qualified_identifier = QualifiedIdentifier(IDENTIFIER)
    argument_list = ArgumentList()
    emitter_input = MethodCall(qualified_identifier, argument_list)

    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = f"{IDENTIFIER}()"

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_method_call_with_println():
    """
    This test checks that the emitter can produce the correct output when given
    a MethodCall object, and then translate that input from 'System.out.println'
    to 'print' (since that is the name of the function in Python).
    """

    PRINTLN_IDENTIFIER = "println"
    println_qualified_identifier = QualifiedIdentifier(PRINTLN_IDENTIFIER)

    OUT_IDENTIFIER = "out"
    out_qualified_identifier = QualifiedIdentifier(OUT_IDENTIFIER,
                                                   println_qualified_identifier)

    IDENTIFIER = "System"
    qualified_identifier = QualifiedIdentifier(IDENTIFIER,
                                               out_qualified_identifier)
    argument_list = ArgumentList()
    emitter_input = MethodCall(qualified_identifier, argument_list)

    emitter_result: str = emit_ast_into_output(emitter_input)

    expected_output = f"print()"

    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_argument_list_with_no_arguments():
    """
    This test checks that the emitter can produce the correct output when given
    an ArgumentList object which contains no arguments
    (i.e., it's first parameter is None).
    """

    emitter_input = ArgumentList()
    emitter_result: str = emit_ast_into_output(emitter_input)
    expected_output = ""
    
    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_singular_argument_argument_list():
    """
    This test checks that the emitter can produce the correct output when given
    an ArgumentList object which contains a singular argument.
    """

    NUMBER = "86"
    argument_comp_expression: ComparisonExpression
    argument_comp_expression = generate_single_comp_expression(NUMBER)
    emitter_input = ArgumentList(argument_comp_expression)
    
    emitter_result: str = emit_ast_into_output(emitter_input)
    
    expected_output = NUMBER
    
    assert expected_output == emitter_result


def test_emitter_can_produce_correct_output_for_multiple_argument_argument_list():
    """
    This test checks that the emitter can produce the correct output when given
    an ArgumentList object which contains multiple arguments.
    """

    IDENTIFIER = "eightysixsu"
    additional_argument_comp_expression: ComparisonExpression
    additional_argument_comp_expression = generate_single_comp_expression(
        IDENTIFIER
    )
    additional_argument_list = ArgumentList(additional_argument_comp_expression)

    NUMBER = "85"
    argument_comp_expression: ComparisonExpression
    argument_comp_expression = generate_single_comp_expression(NUMBER)
    emitter_input = ArgumentList(argument_comp_expression,
                                 additional_argument_list)

    emitter_result: str = emit_ast_into_output(emitter_input) 

    expected_output = f"{NUMBER}, {IDENTIFIER}" 

    assert expected_output == emitter_result



