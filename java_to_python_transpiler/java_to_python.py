from __future__ import annotations
from collections.abc import KeysView

"""
This module's purpose is to tokenize user input (some Java source code string)
"""

from dataclasses import dataclass
import re
from enum import Enum
from typing import Dict, List, Optional, Union, Tuple


TokenType = str


# todo: convert these strings into an enumeration
SINGLE_LINE_COMMENT_TOKEN_TYPE = "SINGLE_LINE_COMMENT"
LEFT_PARENTHESIS_TOKEN_TYPE = "LEFT_PARENTHESIS"
RIGHT_PARENTHESIS_TOKEN_TYPE = "RIGHT_PARENTHESIS"
LEFT_CURLY_BRACE_TOKEN_TYPE = "LEFT_CURLY_BRACE"
RIGHT_CURLY_BRACE_TOKEN_TYPE = "RIGHT_CURLY_BRACE"
LEFT_BRACKET_TOKEN_TYPE = "LEFT_BRACKET"
RIGHT_BRACKET_TOKEN_TYPE = "RIGHT_BRACKET"
SEMI_COLON_TOKEN_TYPE = "SEMI_COLON"
COMMA_TOKEN_TYPE = "COMMA"
EXCLAMATION_TOKEN_TYPE = "EXCLAMATION"
EQUALS_TOKEN_TYPE = "EQUALS"
LESS_THAN_TOKEN_TYPE = "LESS_THAN"
GREATER_THAN_TOKEN_TYPE = "GREATER_THAN"
PLUS_TOKEN_TYPE = "PLUS"
MINUS_TOKEN_TYPE = "MINUS"
MULTIPLY_TOKEN_TYPE = "MULTIPLY"
DIVIDE_TOKEN_TYPE = "DIVIDE"
FLOAT_LITERAL_TOKEN_TYPE = "FLOAT_LITERAL"
DECIMAL_LITERAL_TOKEN_TYPE = "DECIMAL_LITERAL"
STRING_LITERAL_TOKEN_TYPE = "STRING_LITERAL"
WHITESPACE_TOKEN_TYPE = "WHITESPACE"
IDENTIFIER_TOKEN_TYPE = "IDENTIFIER"
END_OF_FILE_TOKEN_TYPE = "EOF"

TRUE_TOKEN_TYPE = r"TRUE"
FALSE_TOKEN_TYPE = r"FALSE"
NULL_TOKEN_TYPE = r"NULL"
PUBLIC_TOKEN_TYPE = r"PUBLIC"
PRIVATE_TOKEN_TYPE = r"PRIVATE"
VOID_TOKEN_TYPE = r"VOID"
STATIC_TOKEN_TYPE = r"STATIC"
BYTE_TOKEN_TYPE = r"BYTE"
SHORT_TOKEN_TYPE = r"SHORT"
CHAR_TOKEN_TYPE = r"CHAR"
INT_TOKEN_TYPE = r"INT"
LONG_TOKEN_TYPE = r"LONG"
FLOAT_TOKEN_TYPE = r"FLOAT"
DOUBLE_TOKEN_TYPE = r"DOUBLE"
BOOLEAN_TOKEN_TYPE = r"BOOLEAN"
CLASS_TOKEN_TYPE = r"CLASS"
RETURN_TOKEN_TYPE = r"RETURN"
NEW_TOKEN_TYPE = r"NEW"
PACKAGE_TOKEN_TYPE = r"PACKAGE"
IMPORT_TOKEN_TYPE = r"IMPORT"
IF_TOKEN_TYPE = r"IF"
ELSE_TOKEN_TYPE = r"ELSE"
WHILE_TOKEN_TYPE = r"WHILE"


TOKEN_PATTERNS: Dict[str, str] = {
    r"//.*": SINGLE_LINE_COMMENT_TOKEN_TYPE,
    r"\(": LEFT_PARENTHESIS_TOKEN_TYPE,
    r"\)": RIGHT_PARENTHESIS_TOKEN_TYPE,
    r"\{": LEFT_CURLY_BRACE_TOKEN_TYPE,
    r"\}": RIGHT_CURLY_BRACE_TOKEN_TYPE,
    r"\[": LEFT_BRACKET_TOKEN_TYPE,
    r"\]": RIGHT_BRACKET_TOKEN_TYPE,
    r";": SEMI_COLON_TOKEN_TYPE,
    r",": COMMA_TOKEN_TYPE,
    r"!": EXCLAMATION_TOKEN_TYPE,
    r"=": EQUALS_TOKEN_TYPE,
    r"\<": LESS_THAN_TOKEN_TYPE,
    r"\>": GREATER_THAN_TOKEN_TYPE,
    r"\+": PLUS_TOKEN_TYPE,
    r"-": MINUS_TOKEN_TYPE,
    r"\*": MULTIPLY_TOKEN_TYPE,
    r"\/": DIVIDE_TOKEN_TYPE,
    r"\d+\.\d+": FLOAT_LITERAL_TOKEN_TYPE,
    r"\d+": DECIMAL_LITERAL_TOKEN_TYPE,
    r"\".*\"": STRING_LITERAL_TOKEN_TYPE,
    #    r'"([^\"\\],|\\[btnfr\"\'\\])*"': STRING_LITERAL_TOKEN_TYPE,
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
class LexerFailure:
    """
    Represents an error occurring in the Lexer.

    error_message is the message to be printed to the user
    """

    error_message: str


ERROR_MESSAGE_FOR_LEXER = "Found an unknown character, '{0}'"


def report_error_for_lexer(unknown_character: str) -> LexerFailure:
    """
    This function's purpose is to report an error that occurred in the
    lexer.
    """

    error_message: str = ERROR_MESSAGE_FOR_LEXER.format(unknown_character)

    return LexerFailure(error_message)


Tokens = Tuple[Token, ...]
LexerResult = Tokens | LexerFailure


def scan_and_tokenize_input(user_input: str) -> LexerResult:
    """ This function's purpose is to be the entrypoint for the lexer. """

    tokens: List[Token] = []
    position = 0
    length_of_input: int = len(user_input)
    
    while position < length_of_input:
        match: Union[re.Match[str], None] = None

        # Incase you don't remember hunter, this gives you everything after
        # position
        sliced_input: str = user_input[position:]
        
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
            unknown_character: str = user_input[position]
            return report_error_for_lexer(unknown_character)

        position += match.end()


    # I don't think this closure needs to be tested. It used as a map a couple
    # lines after it is declared.
    def change_identifier_to_keyword(token: Token) -> Token:
        """
        The purpose of this function is to take in a Token.

        If the Token is not an identifier then the Token is returned as it

        If the Token is of type identifier, it will check if that identifier is
        a keyword. If it is a keyword, then it will create a new keyword Token
        """
        
        KEYWORDS: Dict[str, str] = {
            r"true": TRUE_TOKEN_TYPE,
            r"false": FALSE_TOKEN_TYPE,
            r"null": NULL_TOKEN_TYPE,
            r"public": PUBLIC_TOKEN_TYPE,
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
        
        token_type_not_identifier = token.token_type != IDENTIFIER_TOKEN_TYPE
        keywords_keys: KeysView = KEYWORDS.keys()

        if token_type_not_identifier or (token.value not in keywords_keys):
            return token

        keyword_token_type: str = KEYWORDS[token.value]

        # The value is kind of redundant
        keyword_token = Token(keyword_token_type, token.value)

        return keyword_token


    tokens_with_keywords: map[Token] = map(change_identifier_to_keyword, tokens)
    tokens_with_keywords_as_list: List[Token] = list(tokens_with_keywords)

    # Reminder, let's stop using mutable data and switch to immutable data
    end_of_file_token = Token(END_OF_FILE_TOKEN_TYPE, "")
    tokens_with_keywords_as_list.append(end_of_file_token)

    tokens_as_tuple: Tokens = tuple(tokens_with_keywords_as_list)

    return tokens_as_tuple 


@dataclass
class ParserFailure:
    """
    Represents an error occurring while parsing.

    `error_message` is the the message to be printed for the user
    """

    error_messasge: str


@dataclass
class NodeSuccess:
    """
    Represents a successful node result.

    `tokens` is the remaining list of Tokens 
    `node` is the output to be added to the AST
    """

    tokens: Tokens
 
    node: Union[
        ComparisonExpression, ExpressionNode, TermNode, FactorNode,
        MethodCall, ArgumentList,
        VariableInitialization, ReturnStatement, VariableIncrement,
        InlineStatement,
        WhileStatement, IfStatement,
        BlockStatement,
        StatementList,
        ParameterList
    ]


@dataclass
class NodeFailure:
    """
    Represents an error the occurs in a node.

    `error_message` is the message to be printed for the user.
    """

    error_message: str


class ComparisonOperator(Enum):
    """
    Enumeration class representing comparison operators in a boolean expression.

    Enum Members:
    - LESS_THAN: Represents the less_than operator '<'
    - GREATER_THAN: Represents the greater_than operator '>'
    - LESS_THAN_OR_EQUAL: Represents the less_than_or_equal_to operator '<='
    - GREATER_THAN_OR_EQUAL: Represents the greater_than_or_equal_to operator '>='
    - BOOLEAN_EQUAL: Represents the boolean equality operator '=='
    - NOT_EQUAL: Represents the not equals operator '!='
    """

    LESS_THAN = "<"
    GREATER_THAN = ">"
    LESS_THAN_OR_EQUAL = "<="
    GREATER_THAN_OR_EQUAL = ">="
    BOOLEAN_EQUAL = "=="
    NOT_EQUAL = "!="


@dataclass
class ComparisonExpression:
    """
    Represents a boolean expression comparing two expressions to each other.

    `first_expression` is represented by an ExpressionNode

    `operator` is represented by a ComparisonOperator
    
    `second_expression` is represented by an ExpressionNode
    """

    expression: ExpressionNode
    operator: Optional[ComparisonOperator] = None
    additional_expression: Optional[ExpressionNode] = None


@dataclass
class ExpressionNode:
    """
    Represents a node in the abstract syntax tree (AST) for mathematical
    expressions.

    Attributes:
    
    - single_term_node (TermNode): The term node representing a single
    mathematical term.
    
    - operator (Optional[ArithmeticOperator]): The arithmetic operator
    connecting this node with the next. Defaults to None if no operator is
    present.
    
    - additional_expression_node (Optional[ExpressionNode]): Another expression
    node connected to this one through the specified operator.
    Defaults to None if no additional expression is present. 

    """

    single_term_node: TermNode
    operator: Optional[ArithmeticOperator] = None
    additional_expression_node: Optional[ExpressionNode] = None


@dataclass
class TermNode:
    """
    Represents a node in the abstract syntax tree (AST) for mathematical terms.

    Attributes:
    - first_factor_node (FactorNode): The factor node representing the first
    factor in the term.
    
    - operator (Optional[ArithmeticOperator]): The arithmetic operator
    connecting this term node with the next term node. Defaults to None if no
    operator is present.
    
    - second_factor_node (Optional[FactorNode]): Another factor node
    representing the second factor in the term, connected by the specified
    operator. Defaults to None if no second factor is present.
    """    

    single_factor_node: FactorNode
    operator: Optional[ArithmeticOperator] = None
    additional_term_node: Optional[TermNode] = None


class ArithmeticOperator(Enum):
    """
    Enumeration class representing arithmetic operators in a mathematical
    expression.

    Enum Members:
    - PLUS: Represents the addition operator '+'
    - MINUS: Represents the subtraction operator '-'
    - MULTIPLY: Represents the multiplication operator '*'
    - DIVIDE: Represents the division operator '/'
    """

    # The values of these enum members does not matter
    PLUS = "+"
    MINUS = "-"
    MULTIPLY = "*"
    DIVIDE = "/"


@dataclass
class FactorNode:
    """
    Represents a node for a numerical factor.

    number_or_identifier represents number or identifier that is the factor.
    It is not necessary to differentiate between the two because the emitter sees
    them the same way. This field defaults to an empty string.

    method_call represents a call to a method. This field defaults to None.

    FactorNode cannot have both its fields using their default values. If
    method_call is None, then number_or_identifier has a value, and vice-versa.
    """

    number_or_identifier: str = ""
    method_call: Optional[MethodCall] = None


@dataclass
class MethodCall:
    """
    Represents a call to a method.

    identifier represents the name of the method being called.

    argument_list represents the list of arguments supplied to the method. It
    defaults to None; None represents called a method with no arguments. 
    """
    
    identifier: str
    argument_list: Optional[ArgumentList] = None


@dataclass
class ArgumentList:
    """
    Represents a list of arguments.

    `argument` is represented by an comparisn expression node.

    `additonal_argument_list` represents additional arguments. This
    field defaults to None; when it is None, it represents a method being called
    with a singular argument.
    """

    argument: Optional[ComparisonExpression] = None
    additional_argument_list: Optional[ArgumentList] = None


@dataclass
class VariableInitialization:
    """
    Represents a variable being initialized.

    `identifier` is represented by a string; this is the name of the variable

    `comp_expression` is represented by a `ComparisonExpression`; this
    is the value of the variable
    """

    identifier: str
    comp_expression: ComparisonExpression 


@dataclass
class ReturnStatement:
    """
    Represents a return statement.

    `comp_expression` is represented by ComparisonExpression; this is the value
    being returned.
    """

    comp_expression: Optional[ComparisonExpression] = None


@dataclass
class VariableIncrement:
    """
    Represents a variable being incremented
    
    `identifier` is represented by a string; this is the variable being
    incremented

    `comp_expression` is represented by ComparisonExpression; this is the amount
    that the variable is being incremented by.
    """

    identifier: str
    comp_expression: ComparisonExpression 


@dataclass
class InlineStatement:
    """
    Represents a statement that ends with a semicolon opposed to a BlockStatement.

    `statement` can is represented by any of the statement objects like
    ReturnStatement or VariableInitialization
    """

    statement: Union[
        ReturnStatement, VariableInitialization, VariableIncrement,
        ExpressionNode
    ]


@dataclass
class IfStatement:
    """
    Represents an if statement.
    
    `comparison_expression` is represented by a ComparisonExpression; this is
    the condition of the if statement.

    `statement_list` is represented by a StatementList; this is the body of the
    if statement.
    """

    comparison_expression: ComparisonExpression
    statement_list: StatementList
    additional_if_statement: Optional[IfStatement] = None
    else_clause: Optional[StatementList] = None


@dataclass
class WhileStatement:
    """
    Represents a while loop.
    
    `comparison_expression` is represented by a ComparisonExpression; this is
    the condition of the while loop

    `statement_list` is represented by a StatementList; this is the body
    of the while loop
    """

    comparison_expressionn: ComparisonExpression
    statement_list: StatementList


@dataclass
class BlockStatement:
    """
    Represents a statement with its own curly braces.

    `statement` is represented by some curly brace statement like WhileStatement
    """

    statement: WhileStatement | IfStatement


@dataclass
class StatementList:
    """
    Represents one or more InlineStatement objects.

    `statement` (optional) is represented by an InlineStatement object; this the
    first statement of the chain

    `additional_statement_list` (optional) is represented by a
    StatementList object; this allows to chain the parser together
    """

    statement: InlineStatement | BlockStatement | None = None
    additional_statement_list: Optional[StatementList] = None


@dataclass
class ParameterList:
    """
    Represents the parameters of a method.

    `identifier` (optional) represents the name of the parameter.

    `additional_parameter_list` (optional) represents additional parameters
    kind of chained together.
    """

    identifier: Optional[str] = None
    additional_parameter_list: Optional[ParameterList] = None


# TODO: you could keep track of the return type, parameters, etc. for typehint
@dataclass
class MethodDeclaration:
    """
    Represents a method being declared.

    `identifier` represents the name of the method

    `parameter_list` represents the parameters of the method

    `statement_list` represents the body of the method
    """

    identifier: str

    statement_list: StatementList


ERROR_MESSAGE_FOR_PARSER = "Unexpected token type, {0}"


def report_error_in_parser(unexpected_token_type: TokenType) -> NodeFailure:
    """
    This function's purpose is to report an error found in the parser
    """

    error_message = ERROR_MESSAGE_FOR_PARSER.format(unexpected_token_type)

    return NodeFailure(error_message)


ParserResult = StatementList | ParserFailure


def parse_tokens(tokens: Tokens) -> ParserResult:
    """ This function's purpose is to be the entrypoint for the parser """

    root_node_result: NodeResult = parse_tokens_for_statement_list(tokens)

    if isinstance(root_node_result, NodeFailure):
        return ParserFailure(root_node_result.error_message)

    assert isinstance(root_node_result.node, StatementList)

    return root_node_result.node


# TODO: Everything todo w/ expr stops at EOF but it really should stop at SEMICOLON
# HOwever, that may only apply to statements but no if expr reaches semi, it stops
NodeResult = NodeSuccess | NodeFailure
VARIABLE_TYPES: Tuple[TokenType, ...] = (
    INT_TOKEN_TYPE, CHAR_TOKEN_TYPE, SHORT_TOKEN_TYPE, LONG_TOKEN_TYPE,
    BYTE_TOKEN_TYPE, DOUBLE_TOKEN_TYPE, BOOLEAN_TOKEN_TYPE, FLOAT_TOKEN_TYPE
)


# TODO: Enhance the documentation of the ALL THESE functions
# todo: add tests for expecting variable types; dw its just failures    
def parse_tokens_for_parameter_list(tokens: Tokens) -> NodeResult:
    """
    Parses a tuple of tokens in order to construct a ParameterList object.
    """

    initial_token: Token = tokens[0]

    if initial_token.token_type == RIGHT_PARENTHESIS_TOKEN_TYPE:
        parameter_list = ParameterList()
        return NodeSuccess(tokens, parameter_list)

    if initial_token.token_type not in VARIABLE_TYPES:
        return report_error_in_parser(initial_token.token_type)

    tokens_with_type_removed: Tokens = tokens[1:]

    expected_identifier_token: Token = tokens_with_type_removed[0]
    if expected_identifier_token.token_type != IDENTIFIER_TOKEN_TYPE:
        return report_error_in_parser(expected_identifier_token.token_type)

    tokens_with_identifier_removed: Tokens = tokens_with_type_removed[1:]

    EXPECTED_TYPES: Tuple[TokenType, TokenType] = (COMMA_TOKEN_TYPE,
                                                   RIGHT_PARENTHESIS_TOKEN_TYPE)

    expected_comma_or_paren_token: Token = tokens_with_identifier_removed[0]
    if expected_comma_or_paren_token.token_type not in EXPECTED_TYPES:
        return report_error_in_parser(expected_comma_or_paren_token.token_type)

    if expected_comma_or_paren_token.token_type == RIGHT_PARENTHESIS_TOKEN_TYPE:
        parameter_list = ParameterList(expected_identifier_token.value)
        return NodeSuccess(tokens_with_identifier_removed, parameter_list)

    tokens_with_comma_removed: Tokens = tokens_with_identifier_removed[1:]

    node_result_for_additional_parameter_list: NodeResult
    node_result_for_additional_parameter_list = parse_tokens_for_parameter_list(
        tokens_with_comma_removed
    )

    if isinstance(node_result_for_additional_parameter_list, NodeFailure):
        return node_result_for_additional_parameter_list

    assert isinstance(node_result_for_additional_parameter_list.node,
                      ParameterList)

    parameter_list = ParameterList(expected_identifier_token.value,
                                   node_result_for_additional_parameter_list.node)

    return NodeSuccess(node_result_for_additional_parameter_list.tokens,
                       parameter_list)


def parse_tokens_for_statement_list(tokens: Tokens) -> NodeResult:
    """
    Parses a tuple of tokens in order to construct an StatementList object
    """

    initial_token: Token = tokens[0]
    if initial_token.token_type == RIGHT_CURLY_BRACE_TOKEN_TYPE:

        empty_statement_list = StatementList()
        return NodeSuccess(tokens, empty_statement_list)

    BLOCK_STATEMENT_KEYWORDS: Tuple[TokenType, ...] = (
        WHILE_TOKEN_TYPE, IF_TOKEN_TYPE,
    )
    
    node_result_for_initial_statement: NodeResult = (
        parse_tokens_for_block_statement(tokens)
        if initial_token.token_type in BLOCK_STATEMENT_KEYWORDS
        else parse_tokens_for_inline_statement(tokens)
    )

    if isinstance(node_result_for_initial_statement, NodeFailure):
        return node_result_for_initial_statement

    EXPECTED_TYPES: Tuple[type, type] = (InlineStatement, BlockStatement)

    assert isinstance(node_result_for_initial_statement.node, EXPECTED_TYPES)

    current_token: Token = node_result_for_initial_statement.tokens[0]

    # Eventually, take the eof out. There's another place that does this.
    # take the eof out of there too
    if current_token.token_type in (RIGHT_CURLY_BRACE_TOKEN_TYPE,
                                    END_OF_FILE_TOKEN_TYPE):
        simple_statement_list = StatementList(node_result_for_initial_statement.node)

        return NodeSuccess(node_result_for_initial_statement.tokens,
                           simple_statement_list)

    result_for_additional_statement_list: NodeResult
    result_for_additional_statement_list = parse_tokens_for_statement_list(
        node_result_for_initial_statement.tokens
    )

    if isinstance(result_for_additional_statement_list, NodeFailure):
        return result_for_additional_statement_list

    assert isinstance(result_for_additional_statement_list.node, StatementList)

    statement_list = StatementList(
        node_result_for_initial_statement.node,
        result_for_additional_statement_list.node
    )

    return NodeSuccess(result_for_additional_statement_list.tokens, statement_list)


def parse_tokens_for_block_statement(tokens: Tokens) -> NodeResult:
    """
    Parses a tuple of tokens in order to construct a WhileStatement or some
    other block statement.
    """

    initial_token: Token = tokens[0]

    if initial_token.token_type == WHILE_TOKEN_TYPE:
        node_result_for_while_statement: NodeResult
        node_result_for_while_statement = parse_tokens_for_while_statement(tokens)

        if isinstance(node_result_for_while_statement, NodeFailure):
            return report_error_in_parser(initial_token.token_type)

        assert isinstance(node_result_for_while_statement.node, WhileStatement)
        
        block_statement = BlockStatement(node_result_for_while_statement.node)
        return NodeSuccess(node_result_for_while_statement.tokens, block_statement)

    elif initial_token.token_type == IF_TOKEN_TYPE:
        node_result_for_if_statement: NodeResult
        node_result_for_if_statement = parse_tokens_for_if_statement(tokens)

        if isinstance(node_result_for_if_statement, NodeFailure):
            return report_error_in_parser(initial_token.token_type)

        assert isinstance(node_result_for_if_statement.node, IfStatement)

        block_statement = BlockStatement(node_result_for_if_statement.node)
        return NodeSuccess(node_result_for_if_statement.tokens, block_statement)

    # This should be unreachable; maybe restructure the code to be more elegant
    assert False


def parse_tokens_for_while_statement(tokens: Tokens) -> NodeResult:
    """
    Parses a tuple of tokens in order to construct a WhileStatement object
    """

    tokens_with_keyword_removed: Tokens = tokens[1:]

    node_result_for_comp_expression_with_paren: NodeResult
    node_result_for_comp_expression_with_paren = parse_tokens_for_expression_in_paren(
        tokens_with_keyword_removed
    )

    if isinstance(node_result_for_comp_expression_with_paren, NodeFailure):
        return node_result_for_comp_expression_with_paren

    assert isinstance(node_result_for_comp_expression_with_paren.node,
                      ComparisonExpression)

    node_result_for_block_statement: NodeResult
    node_result_for_block_statement = parse_tokens_for_block_statement_body(
        node_result_for_comp_expression_with_paren.tokens
    )

    if isinstance(node_result_for_block_statement, NodeFailure):
        return node_result_for_block_statement

    assert isinstance(node_result_for_block_statement.node, StatementList)

    while_statement = WhileStatement(
        node_result_for_comp_expression_with_paren.node,
        node_result_for_block_statement.node
    )

    return NodeSuccess(node_result_for_block_statement.tokens, while_statement)


def parse_tokens_for_if_statement(tokens: Tokens) -> NodeResult:
    """
    Parses a tuple of tokens in order to construct an IfStatement object.
    """

    tokens_with_keyword_removed: Tokens = tokens[1:]

    node_result_for_comp_expression_with_paren: NodeResult
    node_result_for_comp_expression_with_paren = parse_tokens_for_expression_in_paren(
        tokens_with_keyword_removed
    )

    if isinstance(node_result_for_comp_expression_with_paren, NodeFailure):
        return node_result_for_comp_expression_with_paren

    assert isinstance(node_result_for_comp_expression_with_paren.node,
                      ComparisonExpression)

    node_result_for_block_statement: NodeResult
    node_result_for_block_statement = parse_tokens_for_block_statement_body(
        node_result_for_comp_expression_with_paren.tokens
    )

    if isinstance(node_result_for_block_statement, NodeFailure):
        return node_result_for_block_statement

    assert isinstance(node_result_for_block_statement.node, StatementList)

    possible_else_token: Token = node_result_for_block_statement.tokens[0]
    if possible_else_token.token_type != ELSE_TOKEN_TYPE:
        
        simple_if_statement = IfStatement(
            node_result_for_comp_expression_with_paren.node,
            node_result_for_block_statement.node
        )

        return NodeSuccess(
            node_result_for_block_statement.tokens,
            simple_if_statement
        )

    tokens_with_else_keyword_removed: Tokens
    tokens_with_else_keyword_removed = node_result_for_block_statement.tokens[1:]

    possible_if_token: Token = tokens_with_else_keyword_removed[0]
    if possible_if_token.token_type == IF_TOKEN_TYPE:

        node_result_for_additional_if_statement: NodeResult
        node_result_for_additional_if_statement = parse_tokens_for_if_statement(
            tokens_with_else_keyword_removed
        )

        if isinstance(node_result_for_additional_if_statement, NodeFailure):
            return node_result_for_additional_if_statement

        assert isinstance(node_result_for_additional_if_statement.node, IfStatement)

        complex_if_statement = IfStatement(
            node_result_for_comp_expression_with_paren.node,
            node_result_for_block_statement.node,
            additional_if_statement=node_result_for_additional_if_statement.node
        )

        # You shouldn't have to parse for else clause here (i dont think)

        return NodeSuccess(
            node_result_for_additional_if_statement.tokens,
            complex_if_statement
        )

    node_result_for_else_block_statement: NodeResult
    node_result_for_else_block_statement = parse_tokens_for_block_statement_body(
        tokens_with_else_keyword_removed
    )

    if isinstance(node_result_for_else_block_statement, NodeFailure):
        return node_result_for_else_block_statement

    assert isinstance(node_result_for_else_block_statement.node, StatementList)

    if_else_statement = IfStatement(
        node_result_for_comp_expression_with_paren.node,
        node_result_for_block_statement.node,
        else_clause=node_result_for_else_block_statement.node
    )

    return NodeSuccess(
        node_result_for_else_block_statement.tokens,
        if_else_statement
    )


def parse_tokens_for_expression_in_paren(tokens: Tokens) -> NodeResult:
    """
    Parses a tuple of tokens in order to construct a ComparisonExpression object,
    but also checks for open parenthesis and closing parenthesis.
    """

    expected_left_paren_token: Token = tokens[0]
    if expected_left_paren_token.token_type != LEFT_PARENTHESIS_TOKEN_TYPE:
        return report_error_in_parser(expected_left_paren_token.token_type)
 
    tokens_with_left_paren_removed: Tokens = tokens[1:]
    
    node_result_comp_expression: NodeResult
    node_result_comp_expression = parse_tokens_for_comparison_expression(
        tokens_with_left_paren_removed
    )

    if isinstance(node_result_comp_expression, NodeFailure):
        return node_result_comp_expression

    assert isinstance(node_result_comp_expression.node, ComparisonExpression)

    expected_right_paren_token: Token = node_result_comp_expression.tokens[0]
    if expected_right_paren_token.token_type != RIGHT_PARENTHESIS_TOKEN_TYPE:
        return report_error_in_parser(expected_right_paren_token.token_type)

    tokens_with_right_paren_removed: Tokens
    tokens_with_right_paren_removed = node_result_comp_expression.tokens[1:]

    return NodeSuccess(tokens_with_right_paren_removed,
                       node_result_comp_expression.node)


def parse_tokens_for_block_statement_body(tokens: Tokens) -> NodeResult:
    """
    Parses a tuple of tokens in order to construct a StatementList object, but
    also checks for open curly braces and closing curly braces (since this is
    meant to be used in a block statement).
    """

    expected_left_braces_token: Token = tokens[0]
    if expected_left_braces_token.token_type != LEFT_CURLY_BRACE_TOKEN_TYPE:
        return report_error_in_parser(expected_left_braces_token.token_type)

    tokens_with_left_braces_removed: Tokens = tokens[1:]
    node_result_for_statement_list: NodeResult = parse_tokens_for_statement_list(
        tokens_with_left_braces_removed
    )

    if isinstance(node_result_for_statement_list, NodeFailure):
        return node_result_for_statement_list

    assert isinstance(node_result_for_statement_list.node, StatementList)

    expected_right_braces_token: Token = node_result_for_statement_list.tokens[0]
    if expected_right_braces_token.token_type != RIGHT_CURLY_BRACE_TOKEN_TYPE:
        return report_error_in_parser(expected_right_braces_token.token_type)

    tokens_with_right_braces_removed: Tokens
    tokens_with_right_braces_removed = node_result_for_statement_list.tokens[1:]

    return NodeSuccess(tokens_with_right_braces_removed,
                       node_result_for_statement_list.node)


def parse_tokens_for_inline_statement(tokens: Tokens) -> NodeResult:
    """
    Parses a tuple of tokens in order to construct an InlineStatement object.
    """

    current_token: Token = tokens[0]
    
    if current_token.token_type == RETURN_TOKEN_TYPE:
        
        node_result_for_return_statement: NodeResult
        node_result_for_return_statement = parse_tokens_for_return_statement(tokens)

        if isinstance(node_result_for_return_statement, NodeFailure):
            return node_result_for_return_statement

        assert isinstance(node_result_for_return_statement.node, ReturnStatement)

        expected_semicolon_token: Token = node_result_for_return_statement.tokens[0]
        if expected_semicolon_token.token_type != SEMI_COLON_TOKEN_TYPE:
            return report_error_in_parser(expected_semicolon_token.token_type)

        tokens_with_semicolon_removed: Tokens
        tokens_with_semicolon_removed = node_result_for_return_statement.tokens[1:]

        inline_statement = InlineStatement(node_result_for_return_statement.node)
        return NodeSuccess(tokens_with_semicolon_removed, inline_statement)

    next_token: Token = tokens[1]
    
    conditions_for_variable_initialization: Tuple[bool, bool] = (
        current_token.token_type in VARIABLE_TYPES,
        next_token.token_type == IDENTIFIER_TOKEN_TYPE
    )

    if all(conditions_for_variable_initialization):
        node_result_for_initialization: NodeResult = \
                parse_tokens_for_variable_initialization(tokens)

        if isinstance(node_result_for_initialization, NodeFailure):
            return node_result_for_initialization

        assert isinstance(node_result_for_initialization.node, VariableInitialization)
 
        expected_semicolon_token: Token = node_result_for_initialization.tokens[0]

        if expected_semicolon_token.token_type != SEMI_COLON_TOKEN_TYPE:
            return report_error_in_parser(expected_semicolon_token.token_type)

        tokens_with_semicolon_removed: Tokens
        tokens_with_semicolon_removed = node_result_for_initialization.tokens[1:]
 
        inline_statement = InlineStatement(node_result_for_initialization.node)
        return NodeSuccess(tokens_with_semicolon_removed, inline_statement)

    length_of_tokens: int = len(tokens)

    # The purpose for being 5 is it should have [id, plus, plus | eq, semicol, eof]
    if length_of_tokens >= 5:

        next_next_token: Token = tokens[2]
        
        conditions_for_variable_increment: Tuple[bool, bool, bool] = (
            current_token.token_type == IDENTIFIER_TOKEN_TYPE,
            next_token.token_type == PLUS_TOKEN_TYPE,
            next_next_token.token_type in (PLUS_TOKEN_TYPE, EQUALS_TOKEN_TYPE),
        )

        if all(conditions_for_variable_increment):
            node_result_for_increment: NodeResult
            node_result_for_increment = parse_tokens_for_variable_increment(tokens)

            if isinstance(node_result_for_increment, NodeFailure):
                return node_result_for_increment

            assert isinstance(node_result_for_increment.node, VariableIncrement)

            expected_semicolon_token: Token = node_result_for_increment.tokens[0]
            if expected_semicolon_token.token_type != SEMI_COLON_TOKEN_TYPE:
                return report_error_in_parser(expected_semicolon_token.token_type)

            tokens_with_semicolon_removed: Tokens
            tokens_with_semicolon_removed = node_result_for_increment.tokens[1:]

            inline_statement = InlineStatement(node_result_for_increment.node)

            return NodeSuccess(tokens_with_semicolon_removed, inline_statement)


    # If no other statement, then try to parse an expression
    node_result_for_expression: NodeResult = parse_tokens_for_expression(tokens)

    if isinstance(node_result_for_expression, NodeFailure):
        return node_result_for_expression

    assert isinstance(node_result_for_expression.node, ExpressionNode)

    expected_semicolon_token: Token = node_result_for_expression.tokens[0]
    
    if expected_semicolon_token.token_type != SEMI_COLON_TOKEN_TYPE:
        return report_error_in_parser(expected_semicolon_token.token_type)

    tokens_with_semicolon_removed: Tokens
    tokens_with_semicolon_removed = node_result_for_expression.tokens[1:]

    inline_statement = InlineStatement(node_result_for_expression.node)
    return NodeSuccess(tokens_with_semicolon_removed, inline_statement)


def parse_tokens_for_variable_increment(tokens: Tokens) -> NodeResult:
    """
    Parses a tuple of tokens in order to construct a VariableIncrement object.
    """

    DEFAULT_INCREMENT = "1"

    identifier_token: Token = tokens[0]

    # We know for sure that tokens[1] == "+" so this one determine type of inc
    plus_or_equals_token: Token = tokens[2]

    # The purpose for removing the first two indicies is because inline stmt
    # already checks that the first three tokens are correct so we safely remove
    tokens_with_increment_removed: Tokens = tokens[3:]

    if plus_or_equals_token.token_type == PLUS_TOKEN_TYPE:
        factor = FactorNode(DEFAULT_INCREMENT)
        term = TermNode(factor)
        expression = ExpressionNode(term)
        comparison_expression = ComparisonExpression(expression)

        variable_increment = VariableIncrement(identifier_token.value,
                                               comparison_expression)

        return NodeSuccess(tokens_with_increment_removed, variable_increment)

    node_result_for_expression: NodeResult
    node_result_for_expression = parse_tokens_for_comparison_expression(
        tokens_with_increment_removed
    )

    if isinstance(node_result_for_expression, NodeFailure):
        return node_result_for_expression

    assert isinstance(node_result_for_expression.node, ComparisonExpression)

    variable_increment = VariableIncrement(identifier_token.value,
                                           node_result_for_expression.node)

    return NodeSuccess(node_result_for_expression.tokens, variable_increment)


def parse_tokens_for_return_statement(tokens: Tokens) -> NodeResult:
    """
    Parses a tuple of tokens in order to construct a ReturnStatement object.
    """

    tokens_with_return_token_removed: Tokens = tokens[1:]

    current_token: Token = tokens_with_return_token_removed[0]
    if current_token.token_type == SEMI_COLON_TOKEN_TYPE:

        # Semicolon will get removed later on :: not here
        return_statement = ReturnStatement()
        return NodeSuccess(tokens_with_return_token_removed, return_statement)

    node_result_for_comp_expression: NodeResult = \
            parse_tokens_for_comparison_expression(tokens_with_return_token_removed)

    if isinstance(node_result_for_comp_expression, NodeFailure):
        return node_result_for_comp_expression

    assert isinstance(node_result_for_comp_expression.node, ComparisonExpression)

    return_statement = ReturnStatement(node_result_for_comp_expression.node)

    return NodeSuccess(node_result_for_comp_expression.tokens, return_statement)


def parse_tokens_for_variable_initialization(tokens: Tokens) -> NodeResult:
    """
    Parses a tuple of tokens in order to construct a VariableInitialization
    object.
    """

    tokens_with_variable_type_removed: Tokens = tokens[1:]

    identifier_token: Token = tokens_with_variable_type_removed[0]
    
    tokens_with_identifier_removed: Tokens = \
            tokens_with_variable_type_removed[1:]

    current_token: Token = tokens_with_identifier_removed[0]
    if current_token.token_type != EQUALS_TOKEN_TYPE:
        return report_error_in_parser(current_token.token_type)

    tokens_with_equals_removed: Tokens = tokens_with_identifier_removed[1:]

    node_result_for_comp_expression: NodeResult = \
            parse_tokens_for_comparison_expression(tokens_with_equals_removed)

    if isinstance(node_result_for_comp_expression, NodeFailure):
        return node_result_for_comp_expression

    assert isinstance(node_result_for_comp_expression.node, ComparisonExpression)

    variable_initialization = VariableInitialization(
        identifier_token.value, node_result_for_comp_expression.node
    )

    return NodeSuccess(node_result_for_comp_expression.tokens, variable_initialization)


def parse_tokens_for_comparison_expression(tokens: Tokens) -> NodeResult:
    """
    Parses a list of tokens to construct an ast for a boolean comparison
    expression.
    """

    COMPARISON_OPERATOR_TOKEN_TYPES: Tuple[TokenType, ...] = (
        LESS_THAN_TOKEN_TYPE, GREATER_THAN_TOKEN_TYPE,
        EXCLAMATION_TOKEN_TYPE, EQUALS_TOKEN_TYPE
    )

    node_result_for_expression: NodeResult = parse_tokens_for_expression(tokens)

    if isinstance(node_result_for_expression, NodeFailure):
        return node_result_for_expression

    assert isinstance(node_result_for_expression.node, ExpressionNode)

    current_token: Token = node_result_for_expression.tokens[0]
    if current_token.token_type in (RIGHT_PARENTHESIS_TOKEN_TYPE,
                                    SEMI_COLON_TOKEN_TYPE, COMMA_TOKEN_TYPE):
        
        comparison_expression = ComparisonExpression(node_result_for_expression.node)
        return NodeSuccess(node_result_for_expression.tokens, comparison_expression)

    if current_token.token_type not in COMPARISON_OPERATOR_TOKEN_TYPES:
        return report_error_in_parser(current_token.token_type)

    next_token: Token = node_result_for_expression.tokens[1]
    token_types: Tuple[TokenType, TokenType] = (current_token.token_type,
                                                next_token.token_type)

    operator: ComparisonOperator
    tokens_after_removing_operator: Tokens
    
    if token_types == (LESS_THAN_TOKEN_TYPE, EQUALS_TOKEN_TYPE):
        operator = ComparisonOperator.LESS_THAN_OR_EQUAL
        tokens_after_removing_operator = node_result_for_expression.tokens[2:]
    
    elif token_types == (GREATER_THAN_TOKEN_TYPE, EQUALS_TOKEN_TYPE):
        operator = ComparisonOperator.GREATER_THAN_OR_EQUAL
        tokens_after_removing_operator = node_result_for_expression.tokens[2:]

    elif token_types == (EXCLAMATION_TOKEN_TYPE, EQUALS_TOKEN_TYPE):
        operator = ComparisonOperator.NOT_EQUAL
        tokens_after_removing_operator = node_result_for_expression.tokens[2:]

    elif token_types == (EQUALS_TOKEN_TYPE, EQUALS_TOKEN_TYPE):
        operator = ComparisonOperator.BOOLEAN_EQUAL
        tokens_after_removing_operator = node_result_for_expression.tokens[2:]

    elif current_token.token_type == LESS_THAN_TOKEN_TYPE:
        operator = ComparisonOperator.LESS_THAN
        tokens_after_removing_operator = node_result_for_expression.tokens[1:]

    else:
        operator = ComparisonOperator.GREATER_THAN
        tokens_after_removing_operator = node_result_for_expression.tokens[1:]

    node_result_for_additional_expression: NodeResult = parse_tokens_for_expression(
            tokens_after_removing_operator
    )

    if isinstance(node_result_for_additional_expression, NodeFailure):
        return node_result_for_additional_expression

    assert isinstance(node_result_for_additional_expression.node, ExpressionNode)
    
    comparison_expression = ComparisonExpression(
        node_result_for_expression.node,
        operator,
        node_result_for_additional_expression.node)

    return NodeSuccess(node_result_for_additional_expression.tokens, comparison_expression)


def parse_tokens_for_expression(tokens: Tokens) -> NodeResult:
    """
    Parses a list of tokens to construct an abstract syntax tree (AST) for
    a mathematical expression.
    """

    EXPRESSION_TOKEN_TYPES: Tuple[TokenType, TokenType] = (PLUS_TOKEN_TYPE,
                                                           MINUS_TOKEN_TYPE)

    term_node_result: NodeResult = parse_tokens_for_term(tokens)

    if isinstance(term_node_result, NodeFailure):
        return term_node_result

    assert isinstance(term_node_result.node, TermNode)
    expression_node: ExpressionNode = ExpressionNode(term_node_result.node)

    node_success_for_simple_expression = NodeSuccess(term_node_result.tokens,
                                                     expression_node)

    # We don't pop the token off b/c there's a chance it's not a + or -
    current_token: Token = term_node_result.tokens[0]

    if current_token.token_type == END_OF_FILE_TOKEN_TYPE:
        return node_success_for_simple_expression

    if current_token.token_type not in EXPRESSION_TOKEN_TYPES:
        return node_success_for_simple_expression 

    # The purpose of this statement is to "delete" the first index of tokens
    # (but it's a tuple so you can't modify it)
    tokens_after_deleting_operator: Tokens = term_node_result.tokens[1:]

    expression_node_operator = (
        ArithmeticOperator.PLUS
        if current_token.token_type == PLUS_TOKEN_TYPE
        else ArithmeticOperator.MINUS 
    )

    additional_expression_node_result: NodeResult = \
            parse_tokens_for_expression(tokens_after_deleting_operator)
    
    if isinstance(additional_expression_node_result, NodeFailure):
        return additional_expression_node_result

    assert isinstance(additional_expression_node_result.node, ExpressionNode)

    complex_expression_node = ExpressionNode(
        expression_node.single_term_node,
        expression_node_operator,
        additional_expression_node_result.node
    )

    return NodeSuccess(additional_expression_node_result.tokens,
                       complex_expression_node)


def parse_tokens_for_term(tokens: Tokens) -> NodeResult:
    """
    Parses a list of tokens to construct an abstract syntax tree (AST) for
    a mathematical term.
    """

    TERM_TOKEN_TYPES: Tuple[TokenType, TokenType] = (MULTIPLY_TOKEN_TYPE,
                                                     DIVIDE_TOKEN_TYPE)

    factor_node_result: NodeResult = parse_tokens_for_factor(tokens)

    if isinstance(factor_node_result, NodeFailure):
        return factor_node_result

    assert isinstance(factor_node_result.node, FactorNode)
    term_node = TermNode(factor_node_result.node)

    node_success_for_simple_term = \
            NodeSuccess(factor_node_result.tokens, term_node)

    current_token: Token = factor_node_result.tokens[0]

    if current_token.token_type == END_OF_FILE_TOKEN_TYPE:
        return node_success_for_simple_term

    if current_token.token_type not in TERM_TOKEN_TYPES:
        return node_success_for_simple_term 

    tokens_after_deleting_operator: Tokens = factor_node_result.tokens[1:]

    term_node_operator = (
        ArithmeticOperator.MULTIPLY
        if current_token.token_type == MULTIPLY_TOKEN_TYPE
        else ArithmeticOperator.DIVIDE
    )

    additional_term_node_result: NodeResult = \
            parse_tokens_for_term(tokens_after_deleting_operator)
    
    if isinstance(additional_term_node_result, NodeFailure):
        return additional_term_node_result

    assert isinstance(additional_term_node_result.node, TermNode)

    complex_term_node = TermNode(
        term_node.single_factor_node,
        term_node_operator,
        additional_term_node_result.node
    )

    return NodeSuccess(additional_term_node_result.tokens, complex_term_node)


def parse_tokens_for_factor(tokens: Tokens) -> NodeResult:
    """
    Parses a list of tokens to construct an abstract syntax tree (AST) for
    a mathematical term.
    """

    FACTOR_TOKEN_TYPES: Tuple[TokenType, TokenType, TokenType, TokenType] = (
        DECIMAL_LITERAL_TOKEN_TYPE, TRUE_TOKEN_TYPE, FALSE_TOKEN_TYPE,
        IDENTIFIER_TOKEN_TYPE,
    )

    # The purpose for not popping off tokens[0] is b/c method_call may need to
    # use it. So it will be deleted later on when required
    current_token: Token = tokens[0]

    if current_token.token_type not in FACTOR_TOKEN_TYPES:
        return report_error_in_parser(current_token.token_type)

    next_token: Token = tokens[1]

    conditions_for_method_call: Tuple[bool, bool] = (
        current_token.token_type == IDENTIFIER_TOKEN_TYPE,
        next_token.token_type == LEFT_PARENTHESIS_TOKEN_TYPE
    )

    if all(conditions_for_method_call):

        node_result_for_method_call: NodeResult = parse_tokens_for_method_call(tokens)
        
        if isinstance(node_result_for_method_call, NodeFailure):
            return node_result_for_method_call

        assert isinstance(node_result_for_method_call.node, MethodCall)
    
        factor_node = FactorNode(method_call=node_result_for_method_call.node)

        return NodeSuccess(node_result_for_method_call.tokens, factor_node)

    tokens_after_deleting_current_token: Tokens = tokens[1:]

    # You may need to add a check for END_OF_FILE but idk yet
    factor_node = FactorNode(current_token.value)
    return NodeSuccess(tokens_after_deleting_current_token, factor_node)


def parse_tokens_for_method_call(tokens: Tokens) -> NodeResult:
    """
    This function parses a list of tokens in order to turn them into a MethodCall
    object.
    """

    identifier_token: Token = tokens[0]

    # The purpose of this line is to remove identifier token and left parenthesis
    tokens_without_identifier_and_parenthesis: Tokens = tokens[2:]

    node_result_argument_list: NodeResult = \
            parse_tokens_for_argument_list(tokens_without_identifier_and_parenthesis)
    
    if isinstance(node_result_argument_list, NodeFailure):
        return node_result_argument_list

    current_token: Token = node_result_argument_list.tokens[0]
    tokens_after_deleting_right_parenthesis: Tokens = \
            node_result_argument_list.tokens[1:]

    if current_token.token_type != RIGHT_PARENTHESIS_TOKEN_TYPE:
        return report_error_in_parser(current_token.token_type)

    assert isinstance(node_result_argument_list.node, ArgumentList)

    method_call = MethodCall(identifier_token.value,
                                         node_result_argument_list.node)
    return NodeSuccess(tokens_after_deleting_right_parenthesis, method_call)


def parse_tokens_for_argument_list(tokens: Tokens) -> NodeResult:
    """
    This function parses a list of tokens in order to turn them into an
    ArgumentList object.
    """

    current_token: Token = tokens[0]
    
    if current_token.token_type == END_OF_FILE_TOKEN_TYPE:
        return report_error_in_parser(END_OF_FILE_TOKEN_TYPE)

    # The purpose of this if statement is to account for no argument method calls 
    if current_token.token_type == RIGHT_PARENTHESIS_TOKEN_TYPE:
        argument_list = ArgumentList()
        return NodeSuccess(tokens, argument_list)

    node_result_comp_expression: NodeResult
    node_result_comp_expression = parse_tokens_for_comparison_expression(tokens)

    if isinstance(node_result_comp_expression, NodeFailure):
        return node_result_comp_expression

    NEXT_EXPECTED_TOKEN_TYPES: Tuple[TokenType, TokenType] = (
        COMMA_TOKEN_TYPE,
        RIGHT_PARENTHESIS_TOKEN_TYPE
    )

    next_token: Token = node_result_comp_expression.tokens[0]
    if next_token.token_type not in NEXT_EXPECTED_TOKEN_TYPES:
        return report_error_in_parser(next_token.token_type)

    assert isinstance(node_result_comp_expression.node, ComparisonExpression)

    if next_token.token_type == RIGHT_PARENTHESIS_TOKEN_TYPE:
        argument_list = ArgumentList(node_result_comp_expression.node)
        return NodeSuccess(node_result_comp_expression.tokens, argument_list)

    # The purpose of this code is to delete comma from tokens
    tokens_after_deleting_comma: Tokens = node_result_comp_expression.tokens[1:]

    node_result_additional_argument_list: NodeResult = \
            parse_tokens_for_argument_list(tokens_after_deleting_comma)

    if isinstance(node_result_additional_argument_list, NodeFailure):
        return node_result_additional_argument_list

    assert isinstance(node_result_additional_argument_list.node, ArgumentList)

    argument_list = ArgumentList(
        node_result_comp_expression.node,
        node_result_additional_argument_list.node
    )

    return NodeSuccess(node_result_additional_argument_list.tokens, argument_list)

