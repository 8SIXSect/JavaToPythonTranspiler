from __future__ import annotations
from collections.abc import KeysView

"""
This module's purpose is to tokenize user input (some Java source code string)
"""

from dataclasses import dataclass
import re
from enum import Enum
from typing import Dict, List, Optional, Union, Tuple


class TokenType(Enum):
    SINGLE_LINE_COMMENT = "SINGLE_LINE_COMMENT"
    LEFT_PARENTHESIS = "LEFT_PARENTHESIS"
    RIGHT_PARENTHESIS = "RIGHT_PARENTHESIS"
    LEFT_CURLY_BRACE = "LEFT_CURLY_BRACE"
    RIGHT_CURLY_BRACE = "RIGHT_CURLY_BRACE"
    LEFT_BRACKET = "LEFT_BRACKET"
    RIGHT_BRACKET = "RIGHT_BRACKET"
    SEMI_COLON = "SEMI_COLON"
    COMMA = "COMMA"
    PERIOD = "PERIOD"
    EXCLAMATION = "EXCLAMATION"
    EQUALS = "EQUALS"
    LESS_THAN = "LESS_THAN"
    GREATER_THAN = "GREATER_THAN"
    PLUS = "PLUS"
    MINUS = "MINUS"
    MULTIPLY = "MULTIPLY"
    DIVIDE = "DIVIDE"
    FLOAT_LITERAL = "FLOAT_LITERAL"
    DECIMAL_LITERAL = "DECIMAL_LITERAL"
    STRING_LITERAL = "STRING_LITERAL"
    WHITESPACE = "WHITESPACE"
    IDENTIFIER = "IDENTIFIER"
    END_OF_FILE = "EOF"

    TRUE = r"TRUE"
    FALSE = r"FALSE"
    NULL = r"NULL"
    PUBLIC = r"PUBLIC"
    PRIVATE = r"PRIVATE"
    VOID = r"VOID"
    STATIC = r"STATIC"
    BYTE = r"BYTE"
    SHORT = r"SHORT"
    CHAR = r"CHAR"
    INT = r"INT"
    LONG = r"LONG"
    FLOAT = r"FLOAT"
    DOUBLE = r"DOUBLE"
    BOOLEAN = r"BOOLEAN"
    CLASS = r"CLASS"
    RETURN = r"RETURN"
    NEW = r"NEW"
    PACKAGE = r"PACKAGE"
    IMPORT = r"IMPORT"
    IF = r"IF"
    ELSE = r"ELSE"
    WHILE = r"WHILE"


TOKEN_PATTERNS: Dict[str, TokenType] = {
    r"//.*": TokenType.SINGLE_LINE_COMMENT,
    r"\(": TokenType.LEFT_PARENTHESIS,
    r"\)": TokenType.RIGHT_PARENTHESIS,
    r"\{": TokenType.LEFT_CURLY_BRACE,
    r"\}": TokenType.RIGHT_CURLY_BRACE,
    r"\[": TokenType.LEFT_BRACKET,
    r"\]": TokenType.RIGHT_BRACKET,
    r";": TokenType.SEMI_COLON,
    r",": TokenType.COMMA,
    r"\.": TokenType.PERIOD,
    r"!": TokenType.EXCLAMATION,
    r"=": TokenType.EQUALS,
    r"\<": TokenType.LESS_THAN,
    r"\>": TokenType.GREATER_THAN,
    r"\+": TokenType.PLUS,
    r"-": TokenType.MINUS,
    r"\*": TokenType.MULTIPLY,
    r"\/": TokenType.DIVIDE,
    r"\d+\.\d+": TokenType.FLOAT_LITERAL,
    r"\d+": TokenType.DECIMAL_LITERAL,
    r"\".*\"": TokenType.STRING_LITERAL,
    r"\s+": TokenType.WHITESPACE,
    r"[a-zA-Z_$][\da-zA-Z_]*": TokenType.IDENTIFIER,
}


@dataclass
class Token:
    """
    This class represents tokens.

    It's two fields are token_type and value which a self-explanatory.

    Additionally, it contains field, `line_number`

    Additionally, it contains field, `column_start`

    The purpose of these fields is to track error information
    """

    token_type: TokenType 
    value: str
    line_number: int
    column_start: int


@dataclass
class LexerFailure:
    """
    Represents an error occurring in the Lexer.

    error_message is the message to be printed to the user
    """

    error_message: str


ERROR_MESSAGE_FOR_LEXER = "Found an unknown character, '{0}' on line {1}"


def report_error_for_lexer(unknown_character: str,
                           line_number: int) -> LexerFailure:
    """
    This function's purpose is to report an error that occurred in the
    lexer.
    """

    error_message: str = ERROR_MESSAGE_FOR_LEXER.format(unknown_character,
                                                        line_number)

    return LexerFailure(error_message)


Tokens = Tuple[Token, ...]
LexerResult = Tokens | LexerFailure


def scan_and_tokenize_input(user_input: str) -> LexerResult:
    """ This function's purpose is to be the entrypoint for the lexer. """

    tokens: List[Token] = []
    position = 0
    line_number = 1
    column_position = 1

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

                token_value: str = match.group(0)

                token_type: TokenType = TOKEN_PATTERNS[pattern]
                if token_type == TokenType.WHITESPACE:
                    line_count = token_value.count("\n")
                    line_number += line_count

                    if line_count > 0:
                        column_position = 1
                    break

                token = Token(token_type, token_value, line_number, column_position)
                tokens.append(token)
                break
        
        if match is None:
            unknown_character: str = user_input[position]
            return report_error_for_lexer(unknown_character, line_number)

        match_end: int = match.end()
        position += match_end
        column_position += match_end


    # I don't think this closure needs to be tested. It used as a map a couple
    # lines after it is declared.
    def change_identifier_to_keyword(token: Token) -> Token:
        """
        The purpose of this function is to take in a Token.

        If the Token is not an identifier then the Token is returned as it

        If the Token is of type identifier, it will check if that identifier is
        a keyword. If it is a keyword, then it will create a new keyword Token
        """
        
        KEYWORDS: Dict[str, TokenType] = {
            r"true": TokenType.TRUE,
            r"false": TokenType.FALSE,
            r"null": TokenType.NULL,
            r"public": TokenType.PUBLIC,
            r"private": TokenType.PRIVATE,
            r"void": TokenType.VOID,
            r"static": TokenType.STATIC,
            r"byte": TokenType.BYTE,
            r"short": TokenType.SHORT,
            r"char": TokenType.CHAR,
            r"int": TokenType.INT,
            r"long": TokenType.LONG,
            r"float": TokenType.FLOAT,
            r"double": TokenType.DOUBLE,
            r"boolean": TokenType.BOOLEAN,
            r"class": TokenType.CLASS,
            r"return": TokenType.RETURN,
            r"new": TokenType.NEW,
            r"package": TokenType.PACKAGE,
            r"import": TokenType.IMPORT,
            r"if": TokenType.IF,
            r"else": TokenType.ELSE,
            r"while": TokenType.WHILE,
        }
        
        token_type_not_identifier = token.token_type != TokenType.IDENTIFIER
        keywords_keys: KeysView = KEYWORDS.keys()

        if token_type_not_identifier or (token.value not in keywords_keys):
            return token

        keyword_token_type: TokenType = KEYWORDS[token.value]

        # The value is kind of redundant
        keyword_token = Token(
            keyword_token_type, token.value,
            token.line_number, token.column_start
        )

        return keyword_token


    tokens_with_keywords: map[Token] = map(change_identifier_to_keyword, tokens)
    tokens_with_keywords_as_list: List[Token] = list(tokens_with_keywords)

    # Reminder, let's stop using mutable data and switch to immutable data
    end_of_file_token = Token(TokenType.END_OF_FILE, "", -1, -1)
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
        NoNode,

        ComparisonExpression, ExpressionNode, TermNode, FactorNode,
        MethodCall, ArgumentList, QualifiedIdentifier,
        VariableInitialization, ReturnStatement, VariableIncrement,
        InlineStatement,
        WhileStatement, IfStatement,
        BlockStatement,
        StatementList,
        ParameterList,
        MethodDeclaration, MethodDeclarationList,
        ClassDeclaration
    ]


@dataclass
class NodeFailure:
    """
    Represents an error the occurs in a node.

    `error_message` is the message to be printed for the user.
    """

    error_message: str


@dataclass
class NoNode:
    """
    Some aspects of Java don't need to be represented in Python; however, I
    want to still check for syntax, so this node is used for that case.
    """


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

    `operator` (optional) is represented by a ComparisonOperator
    
    `second_expression` (optional) is represented by an ExpressionNode
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

    `number_or_ident_or_string` represents number or identifier or string literal
    that is the factor.
    It is not necessary to differentiate between these because the emitter sees
    them the same way. This field defaults to an empty string.

    method_call represents a call to a method. This field defaults to None.

    FactorNode cannot have both its fields using their default values. If
    method_call is None, then number_or_ident_or_string has a value, and
    vice-versa.
    """

    number_or_ident_or_string: str = ""
    qualified_identifier: Optional[QualifiedIdentifier] = None
    method_call: Optional[MethodCall] = None


@dataclass
class QualifiedIdentifier:
    """
    Qualified or chained identifier

    `identifier` represents the current part of the identifier
    `additional_identifier` (optional) allows one to chain more clauses
    """

    identifier: str
    additional_identifier: Optional[QualifiedIdentifier] = None


@dataclass
class MethodCall:
    """
    Represents a call to a method.

    `qualified_identifier` represents the name of the method being called.

    argument_list represents the list of arguments supplied to the method.
    """
    
    qualified_identifier: QualifiedIdentifier 
    argument_list: ArgumentList 


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
        ComparisonExpression
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


@dataclass
class MethodDeclaration:
    """
    Represents a method being declared.

    `identifier` represents the name of the method

    `parameter_list` represents the parameters of the method

    `statement_list` represents the body of the method
    """

    identifier: str
    parameter_list: ParameterList
    statement_list: StatementList


@dataclass
class MethodDeclarationList:
    """
    Represents a list of methods being declared.

    `method_declaration` (optional) represents the initial method declaration
    `additional_method_declaration_list` (optional) can chain more method
    declarations together
    """

    method_declaration: Optional[MethodDeclaration] = None
    additional_method_declaration_list: Optional[MethodDeclarationList] = None


@dataclass
class ClassDeclaration:
    """
    Represents a class being declared.

    `identifier` represents the name of the class

    `method_list` represents a list of method declarations
    """

    identifier: str
    method_list: MethodDeclarationList 


ERROR_MESSAGE_FOR_PARSER = "Unexpected token type, {0} on line {1}"


def report_error_in_parser(unexpected_token_type: TokenType,
                           line_number: int) -> NodeFailure:
    """
    This function's purpose is to report an error found in the parser
    """

    error_message = ERROR_MESSAGE_FOR_PARSER.format(unexpected_token_type,
                                                    line_number)

    return NodeFailure(error_message)


ParserResult = ClassDeclaration | ParserFailure


def parse_tokens(tokens: Tokens) -> ParserResult:
    """ This function's purpose is to be the entrypoint for the parser """

    root_node_result: NodeResult = parse_tokens_for_class_declaration(tokens)

    if isinstance(root_node_result, NodeFailure):
        return ParserFailure(root_node_result.error_message)

    assert isinstance(root_node_result.node, ClassDeclaration)

    return root_node_result.node


NodeResult = NodeSuccess | NodeFailure

VARIABLE_TYPES: Tuple[TokenType, ...] = (
    TokenType.INT, TokenType.CHAR, TokenType.SHORT, TokenType.LONG,
    TokenType.BYTE, TokenType.DOUBLE, TokenType.BOOLEAN, TokenType.FLOAT,
    TokenType.IDENTIFIER,
)

ACCESS_MODIFIER_TYPES: Tuple[TokenType, ...] = (
    TokenType.PUBLIC, TokenType.PRIVATE, TokenType.STATIC
)

def parse_tokens_for_class_declaration(tokens: Tokens) -> NodeResult:
    """
    Parses a tuple of tokens in order to construct a ClassDeclaration object.
    """

    node_result_for_access_modifier_list: NodeResult
    node_result_for_access_modifier_list = parse_tokens_for_access_modifier_list(
        tokens
    )

    if isinstance(node_result_for_access_modifier_list, NodeFailure):
        return node_result_for_access_modifier_list

    expected_class_token: Token = node_result_for_access_modifier_list.tokens[0]
    if expected_class_token.token_type != TokenType.CLASS:
        return report_error_in_parser(expected_class_token.token_type,
                                      expected_class_token.line_number)

    tokens_with_class_token_removed: Tokens
    tokens_with_class_token_removed = node_result_for_access_modifier_list.tokens[1:]

    expected_identifier_token: Token = tokens_with_class_token_removed[0]
    if expected_identifier_token.token_type != TokenType.IDENTIFIER:
        return report_error_in_parser(expected_identifier_token.token_type,
                                      expected_identifier_token.line_number)

    tokens_with_identifier_removed: Tokens = tokens_with_class_token_removed[1:]

    expected_left_brace_token: Token = tokens_with_identifier_removed[0]
    if expected_left_brace_token.token_type != TokenType.LEFT_CURLY_BRACE:
        return report_error_in_parser(expected_left_brace_token.token_type,
                                      expected_left_brace_token.line_number)

    tokens_with_left_brace_removed: Tokens = tokens_with_identifier_removed[1:]

    node_result_for_method_dec_list: NodeResult
    node_result_for_method_dec_list = parse_tokens_for_method_declaration_list(
        tokens_with_left_brace_removed
    )

    if isinstance(node_result_for_method_dec_list, NodeFailure):
        return node_result_for_method_dec_list

    assert isinstance(node_result_for_method_dec_list.node, MethodDeclarationList)

    expected_right_brace_token: Token = node_result_for_method_dec_list.tokens[0]
    if expected_right_brace_token.token_type != TokenType.RIGHT_CURLY_BRACE:
        return report_error_in_parser(expected_right_brace_token.token_type,
                                      expected_right_brace_token.line_number)

    tokens_with_right_brace_removed: Tokens
    tokens_with_right_brace_removed = node_result_for_method_dec_list.tokens[1:]

    class_declaration = ClassDeclaration(
        expected_identifier_token.value,
        node_result_for_method_dec_list.node
    )

    # Right now, you shouldn't really have to return tokens but once comments
    # and that stuff is added, this part will be matter b/c there will b stuff
    # after the class is declared
    return NodeSuccess(tokens_with_right_brace_removed, class_declaration)


def parse_tokens_for_method_declaration_list(tokens: Tokens) -> NodeResult:
    """
    Parses a tuple of tokens in order to construct a MethodDeclarationList
    object.
    """

    initial_token: Token = tokens[0]
    if initial_token.token_type == TokenType.RIGHT_CURLY_BRACE:

        method_declaration_list = MethodDeclarationList()
        return NodeSuccess(tokens, method_declaration_list)

    node_result_for_method_declaration: NodeResult
    node_result_for_method_declaration = parse_tokens_for_method_declaration(
        tokens
    )

    if isinstance(node_result_for_method_declaration, NodeFailure):
        return node_result_for_method_declaration

    assert isinstance(node_result_for_method_declaration.node, MethodDeclaration)

    current_token: Token = node_result_for_method_declaration.tokens[0]
    if current_token.token_type not in ACCESS_MODIFIER_TYPES:
        
        method_declaration_list = MethodDeclarationList(
            node_result_for_method_declaration.node
        )

        return NodeSuccess(node_result_for_method_declaration.tokens,
                           method_declaration_list)

    node_result_for_another_method_dec_list: NodeResult
    node_result_for_another_method_dec_list = parse_tokens_for_method_declaration_list(
        node_result_for_method_declaration.tokens
    )

    if isinstance(node_result_for_another_method_dec_list, NodeFailure):
        return node_result_for_another_method_dec_list

    assert isinstance(node_result_for_another_method_dec_list.node,
                      MethodDeclarationList)

    method_declaration_list = MethodDeclarationList(
        node_result_for_method_declaration.node,
        node_result_for_another_method_dec_list.node
    )

    return NodeSuccess(
        node_result_for_another_method_dec_list.tokens,
        method_declaration_list
    )


def parse_tokens_for_method_declaration(tokens: Tokens) -> NodeResult:
    """
    Parses a tuple of tokens in order to construct a MethodDeclaration object.
    """

    node_result_for_access_modifier_list: NodeResult
    node_result_for_access_modifier_list = parse_tokens_for_access_modifier_list(
        tokens
    )

    if isinstance(node_result_for_access_modifier_list, NodeFailure):
        return node_result_for_access_modifier_list


    RETURN_TYPES: Tuple[TokenType, ...] = VARIABLE_TYPES + (TokenType.VOID,)
   
    node_result_for_return_type: NodeResult
    node_result_for_return_type = parse_tokens_for_complete_variable_type(
        node_result_for_access_modifier_list.tokens, RETURN_TYPES
    )

    if isinstance(node_result_for_return_type, NodeFailure):
        return node_result_for_return_type

    expected_identifier_token: Token = node_result_for_return_type.tokens[0]
    if expected_identifier_token.token_type != TokenType.IDENTIFIER:
        return report_error_in_parser(expected_identifier_token.token_type,
                                      expected_identifier_token.line_number)

    tokens_with_identifier_removed: Tokens = node_result_for_return_type.tokens[1:]

    expected_left_paren_token: Token = tokens_with_identifier_removed[0]
    if expected_left_paren_token.token_type != TokenType.LEFT_PARENTHESIS:
        return report_error_in_parser(expected_left_paren_token.token_type,
                                      expected_left_paren_token.line_number)

    tokens_with_left_paren_removed: Tokens = tokens_with_identifier_removed[1:]

    node_result_for_parameter_list: NodeResult = parse_tokens_for_parameter_list(
        tokens_with_left_paren_removed
    )
    
    if isinstance(node_result_for_parameter_list, NodeFailure):
        return node_result_for_parameter_list

    assert isinstance(node_result_for_parameter_list.node, ParameterList)

    # Parameter list should check for right paren (fingers crossed)
    tokens_with_right_paren_removed: Tokens
    tokens_with_right_paren_removed = node_result_for_parameter_list.tokens[1:]

    node_result_for_block_statement_body: NodeResult
    node_result_for_block_statement_body = parse_tokens_for_block_statement_body(
        tokens_with_right_paren_removed
    )

    if isinstance(node_result_for_block_statement_body, NodeFailure):
        return node_result_for_block_statement_body

    assert isinstance(node_result_for_block_statement_body.node, StatementList)

    method_declaration = MethodDeclaration(
        expected_identifier_token.value,
        node_result_for_parameter_list.node,
        node_result_for_block_statement_body.node
    )

    return NodeSuccess(node_result_for_block_statement_body.tokens,
                       method_declaration)


def parse_tokens_for_access_modifier_list(tokens: Tokens) -> NodeResult:
    """
    Parses a tuple of tokens in order to verify that there is at least 1
    access modifier, and then to parse through all the access modifiers.
    """

    expected_access_modifier: Token = tokens[0]
    if expected_access_modifier.token_type not in ACCESS_MODIFIER_TYPES:
        return report_error_in_parser(expected_access_modifier.token_type,
                                      expected_access_modifier.line_number)

    # Mutable data sucks but it's the best solution here without crazy recursion
    tokens_as_list: List[Token] = list(tokens)

    current_token: Token = tokens_as_list[0]

    # The initial access modifier should be deleted from here
    # 0 is used because you keep deleting the 0th index
    while current_token.token_type in ACCESS_MODIFIER_TYPES:
        del tokens_as_list[0]
        
        current_token = tokens_as_list[0]

    tokens_as_tuple: Tokens = tuple(tokens_as_list)
    no_node = NoNode()

    return NodeSuccess(tokens_as_tuple, no_node)   


def parse_tokens_for_complete_variable_type(tokens: Tokens,
                                            variable_types: Tuple[TokenType, ...]
                                            ) -> NodeResult:
    """
    Parses a tuple of tokens in order to construct a NoNode object.

    This function is soley used for verifying the syntax, and it doesn't care
    for any of the data (as it returns a NoNode).

    This function not only parses for the variable type but also checks if it
    returns with [] right after it signifying an array.
    """

    expected_variable_type_token: Token = tokens[0]
    if expected_variable_type_token.token_type not in variable_types:
        return report_error_in_parser(expected_variable_type_token.token_type,
                                      expected_variable_type_token.line_number)

    tokens_with_variable_type_removed: Tokens = tokens[1:]

    # There's no point in declaraing this twice since it's static.
    no_node = NoNode()

    possible_left_bracket_token: Token = tokens_with_variable_type_removed[0]
    if possible_left_bracket_token.token_type != TokenType.LEFT_BRACKET:
        return NodeSuccess(tokens_with_variable_type_removed, no_node)

    tokens_with_left_bracket_removed: Tokens = tokens_with_variable_type_removed[1:]

    expected_right_bracket_token: Token = tokens_with_left_bracket_removed[0]
    if expected_right_bracket_token.token_type != TokenType.RIGHT_BRACKET:
        return report_error_in_parser(expected_right_bracket_token.token_type,
                                      expected_right_bracket_token.line_number)

    tokens_with_brackets_removed: Tokens = tokens_with_left_bracket_removed[1:]

    return NodeSuccess(tokens_with_brackets_removed, no_node)


def parse_tokens_for_parameter_list(tokens: Tokens) -> NodeResult:
    """
    Parses a tuple of tokens in order to construct a ParameterList object.
    """

    initial_token: Token = tokens[0]

    if initial_token.token_type == TokenType.RIGHT_PARENTHESIS:
        parameter_list = ParameterList()
        return NodeSuccess(tokens, parameter_list)

    node_result_for_variable_type: NodeResult
    node_result_for_variable_type = parse_tokens_for_complete_variable_type(
        tokens, VARIABLE_TYPES
    )

    if isinstance(node_result_for_variable_type, NodeFailure):
        return node_result_for_variable_type

    expected_identifier_token: Token = node_result_for_variable_type.tokens[0]
    if expected_identifier_token.token_type != TokenType.IDENTIFIER:
        return report_error_in_parser(expected_identifier_token.token_type,
                                      expected_identifier_token.line_number)

    tokens_with_identifier_removed: Tokens = node_result_for_variable_type.tokens[1:]

    EXPECTED_TYPES: Tuple[TokenType, TokenType] = (TokenType.COMMA,
                                                   TokenType.RIGHT_PARENTHESIS)

    expected_comma_or_paren_token: Token = tokens_with_identifier_removed[0]
    if expected_comma_or_paren_token.token_type not in EXPECTED_TYPES:
        return report_error_in_parser(expected_comma_or_paren_token.token_type,
                                      expected_comma_or_paren_token.line_number)

    if expected_comma_or_paren_token.token_type == TokenType.RIGHT_PARENTHESIS:
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
    if initial_token.token_type == TokenType.RIGHT_CURLY_BRACE:

        empty_statement_list = StatementList()
        return NodeSuccess(tokens, empty_statement_list)

    BLOCK_STATEMENT_KEYWORDS: Tuple[TokenType, ...] = (
        TokenType.WHILE, TokenType.IF,
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
    if current_token.token_type in (TokenType.RIGHT_CURLY_BRACE,
                                    TokenType.END_OF_FILE):
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

    if initial_token.token_type == TokenType.WHILE:
        node_result_for_while_statement: NodeResult
        node_result_for_while_statement = parse_tokens_for_while_statement(tokens)

        if isinstance(node_result_for_while_statement, NodeFailure):
            return report_error_in_parser(initial_token.token_type,
                                          initial_token.line_number)

        assert isinstance(node_result_for_while_statement.node, WhileStatement)
        
        block_statement = BlockStatement(node_result_for_while_statement.node)
        return NodeSuccess(node_result_for_while_statement.tokens, block_statement)

    elif initial_token.token_type == TokenType.IF:
        node_result_for_if_statement: NodeResult
        node_result_for_if_statement = parse_tokens_for_if_statement(tokens)

        if isinstance(node_result_for_if_statement, NodeFailure):
            return report_error_in_parser(initial_token.token_type,
                                          initial_token.line_number)

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
    if possible_else_token.token_type != TokenType.ELSE:
        
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
    if possible_if_token.token_type == TokenType.IF:

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
    if expected_left_paren_token.token_type != TokenType.LEFT_PARENTHESIS:
        return report_error_in_parser(expected_left_paren_token.token_type,
                                      expected_left_paren_token.line_number)
 
    tokens_with_left_paren_removed: Tokens = tokens[1:]
    
    node_result_comp_expression: NodeResult
    node_result_comp_expression = parse_tokens_for_comparison_expression(
        tokens_with_left_paren_removed
    )

    if isinstance(node_result_comp_expression, NodeFailure):
        return node_result_comp_expression

    assert isinstance(node_result_comp_expression.node, ComparisonExpression)

    expected_right_paren_token: Token = node_result_comp_expression.tokens[0]
    if expected_right_paren_token.token_type != TokenType.RIGHT_PARENTHESIS:
        return report_error_in_parser(expected_right_paren_token.token_type,
                                      expected_right_paren_token.line_number)

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
    if expected_left_braces_token.token_type != TokenType.LEFT_CURLY_BRACE:
        return report_error_in_parser(expected_left_braces_token.token_type,
                                      expected_left_braces_token.line_number)

    tokens_with_left_braces_removed: Tokens = tokens[1:]
    node_result_for_statement_list: NodeResult = parse_tokens_for_statement_list(
        tokens_with_left_braces_removed
    )

    if isinstance(node_result_for_statement_list, NodeFailure):
        return node_result_for_statement_list

    assert isinstance(node_result_for_statement_list.node, StatementList)

    expected_right_braces_token: Token = node_result_for_statement_list.tokens[0]
    if expected_right_braces_token.token_type != TokenType.RIGHT_CURLY_BRACE:
        return report_error_in_parser(expected_right_braces_token.token_type,
                                      expected_right_braces_token.line_number)

    tokens_with_right_braces_removed: Tokens
    tokens_with_right_braces_removed = node_result_for_statement_list.tokens[1:]

    return NodeSuccess(tokens_with_right_braces_removed,
                       node_result_for_statement_list.node)


def parse_tokens_for_inline_statement(tokens: Tokens) -> NodeResult:
    """
    Parses a tuple of tokens in order to construct an InlineStatement object.
    """

    current_token: Token = tokens[0]
    
    if current_token.token_type == TokenType.RETURN:
        
        node_result_for_return_statement: NodeResult
        node_result_for_return_statement = parse_tokens_for_return_statement(tokens)

        if isinstance(node_result_for_return_statement, NodeFailure):
            return node_result_for_return_statement

        assert isinstance(node_result_for_return_statement.node, ReturnStatement)

        expected_semicolon_token: Token = node_result_for_return_statement.tokens[0]
        if expected_semicolon_token.token_type != TokenType.SEMI_COLON:
            return report_error_in_parser(expected_semicolon_token.token_type,
                                          expected_semicolon_token.line_number)

        tokens_with_semicolon_removed: Tokens
        tokens_with_semicolon_removed = node_result_for_return_statement.tokens[1:]

        inline_statement = InlineStatement(node_result_for_return_statement.node)
        return NodeSuccess(tokens_with_semicolon_removed, inline_statement)

    next_token: Token = tokens[1]

    condition_for_simple_initialization: bool = (
        current_token.token_type in VARIABLE_TYPES and
        next_token.token_type == TokenType.IDENTIFIER
    )

    condition_for_complex_initialization = False

    length_of_tokens: int = len(tokens)

    # Bare minimum: [type, l-bracket, r-bracket, id, eof]
    # Obviously, this still produces an error, but it will pass this section.
    if length_of_tokens >= 5:
        possible_right_bracket_token: Token = tokens[2]
        possible_identifier_token: Token = tokens[3]

        condition_for_complex_initialization = (
            current_token.token_type in VARIABLE_TYPES and
            next_token.token_type == TokenType.LEFT_BRACKET and
            possible_right_bracket_token.token_type == TokenType.RIGHT_BRACKET and
            possible_identifier_token.token_type == TokenType.IDENTIFIER
        )

    conditions_for_variable_initializaion: Tuple[bool, bool] = (
        condition_for_simple_initialization, condition_for_complex_initialization
    )

    if any(conditions_for_variable_initializaion):
        node_result_for_initialization: NodeResult = \
                parse_tokens_for_variable_initialization(tokens)

        if isinstance(node_result_for_initialization, NodeFailure):
            return node_result_for_initialization

        assert isinstance(node_result_for_initialization.node, VariableInitialization)
 
        expected_semicolon_token: Token = node_result_for_initialization.tokens[0]

        if expected_semicolon_token.token_type != TokenType.SEMI_COLON:
            return report_error_in_parser(expected_semicolon_token.token_type,
                                          expected_semicolon_token.line_number)

        tokens_with_semicolon_removed: Tokens
        tokens_with_semicolon_removed = node_result_for_initialization.tokens[1:]
 
        inline_statement = InlineStatement(node_result_for_initialization.node)
        return NodeSuccess(tokens_with_semicolon_removed, inline_statement)

    # The purpose for being 5 is it should have [id, plus, plus | eq, semicol, eof]
    if length_of_tokens >= 5:

        next_next_token: Token = tokens[2]
        
        conditions_for_variable_increment: Tuple[bool, bool, bool] = (
            current_token.token_type == TokenType.IDENTIFIER,
            next_token.token_type == TokenType.PLUS,
            next_next_token.token_type in (TokenType.PLUS, TokenType.EQUALS),
        )

        if all(conditions_for_variable_increment):
            node_result_for_increment: NodeResult
            node_result_for_increment = parse_tokens_for_variable_increment(tokens)

            if isinstance(node_result_for_increment, NodeFailure):
                return node_result_for_increment

            assert isinstance(node_result_for_increment.node, VariableIncrement)

            expected_semicolon_token: Token = node_result_for_increment.tokens[0]
            if expected_semicolon_token.token_type != TokenType.SEMI_COLON:
                return report_error_in_parser(expected_semicolon_token.token_type,
                                              expected_semicolon_token.line_number)

            tokens_with_semicolon_removed: Tokens
            tokens_with_semicolon_removed = node_result_for_increment.tokens[1:]

            inline_statement = InlineStatement(node_result_for_increment.node)

            return NodeSuccess(tokens_with_semicolon_removed, inline_statement)


    # If no other statement, then try to parse an expression
    node_result_for_comp_expression: NodeResult
    node_result_for_comp_expression = parse_tokens_for_comparison_expression(tokens)

    if isinstance(node_result_for_comp_expression, NodeFailure):
        return node_result_for_comp_expression 

    assert isinstance(node_result_for_comp_expression.node, ComparisonExpression)

    expected_semicolon_token: Token = node_result_for_comp_expression.tokens[0]
    
    if expected_semicolon_token.token_type != TokenType.SEMI_COLON:
        return report_error_in_parser(expected_semicolon_token.token_type,
                                      expected_semicolon_token.line_number)

    tokens_with_semicolon_removed: Tokens
    tokens_with_semicolon_removed = node_result_for_comp_expression.tokens[1:]

    inline_statement = InlineStatement(node_result_for_comp_expression.node)
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

    if plus_or_equals_token.token_type == TokenType.PLUS:
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
    if current_token.token_type == TokenType.SEMI_COLON:

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

    node_result_for_variable_type: NodeResult
    node_result_for_variable_type = parse_tokens_for_complete_variable_type(
        tokens, VARIABLE_TYPES
    )

    if isinstance(node_result_for_variable_type, NodeFailure):
        return node_result_for_variable_type

    identifier_token: Token = node_result_for_variable_type.tokens[0]
    
    tokens_with_identifier_removed: Tokens = node_result_for_variable_type.tokens[1:]

    current_token: Token = tokens_with_identifier_removed[0]
    if current_token.token_type != TokenType.EQUALS:
        return report_error_in_parser(current_token.token_type,
                                      current_token.line_number)

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
        TokenType.LESS_THAN, TokenType.GREATER_THAN,
        TokenType.EXCLAMATION, TokenType.EQUALS, 
    )

    node_result_for_expression: NodeResult = parse_tokens_for_expression(tokens)

    if isinstance(node_result_for_expression, NodeFailure):
        return node_result_for_expression

    assert isinstance(node_result_for_expression.node, ExpressionNode)

    current_token: Token = node_result_for_expression.tokens[0]
    if current_token.token_type in (TokenType.RIGHT_PARENTHESIS,
                                    TokenType.SEMI_COLON, TokenType.COMMA):
        
        comparison_expression = ComparisonExpression(node_result_for_expression.node)
        return NodeSuccess(node_result_for_expression.tokens, comparison_expression)

    if current_token.token_type not in COMPARISON_OPERATOR_TOKEN_TYPES:
        return report_error_in_parser(current_token.token_type,
                                      current_token.line_number)

    next_token: Token = node_result_for_expression.tokens[1]
    token_types: Tuple[TokenType, TokenType] = (current_token.token_type,
                                                next_token.token_type)

    operator: ComparisonOperator
    tokens_after_removing_operator: Tokens
    
    if token_types == (TokenType.LESS_THAN, TokenType.EQUALS):
        operator = ComparisonOperator.LESS_THAN_OR_EQUAL
        tokens_after_removing_operator = node_result_for_expression.tokens[2:]
    
    elif token_types == (TokenType.GREATER_THAN, TokenType.EQUALS):
        operator = ComparisonOperator.GREATER_THAN_OR_EQUAL
        tokens_after_removing_operator = node_result_for_expression.tokens[2:]

    elif token_types == (TokenType.EXCLAMATION, TokenType.EQUALS):
        operator = ComparisonOperator.NOT_EQUAL
        tokens_after_removing_operator = node_result_for_expression.tokens[2:]

    elif token_types == (TokenType.EQUALS, TokenType.EQUALS):
        operator = ComparisonOperator.BOOLEAN_EQUAL
        tokens_after_removing_operator = node_result_for_expression.tokens[2:]

    elif current_token.token_type == TokenType.LESS_THAN:
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

    EXPRESSION_TOKEN_TYPES: Tuple[TokenType, TokenType] = (TokenType.PLUS,
                                                           TokenType.MINUS)

    term_node_result: NodeResult = parse_tokens_for_term(tokens)

    if isinstance(term_node_result, NodeFailure):
        return term_node_result

    assert isinstance(term_node_result.node, TermNode)
    expression_node: ExpressionNode = ExpressionNode(term_node_result.node)

    node_success_for_simple_expression = NodeSuccess(term_node_result.tokens,
                                                     expression_node)

    # We don't pop the token off b/c there's a chance it's not a + or -
    current_token: Token = term_node_result.tokens[0]

    if current_token.token_type == TokenType.END_OF_FILE:
        return node_success_for_simple_expression

    if current_token.token_type not in EXPRESSION_TOKEN_TYPES:
        return node_success_for_simple_expression 

    # The purpose of this statement is to "delete" the first index of tokens
    # (but it's a tuple so you can't modify it)
    tokens_after_deleting_operator: Tokens = term_node_result.tokens[1:]

    expression_node_operator = (
        ArithmeticOperator.PLUS
        if current_token.token_type == TokenType.PLUS 
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

    TERM_TOKEN_TYPES: Tuple[TokenType, TokenType] = (TokenType.MULTIPLY,
                                                     TokenType.DIVIDE)

    factor_node_result: NodeResult = parse_tokens_for_factor(tokens)

    if isinstance(factor_node_result, NodeFailure):
        return factor_node_result

    assert isinstance(factor_node_result.node, FactorNode)
    term_node = TermNode(factor_node_result.node)

    node_success_for_simple_term = NodeSuccess(factor_node_result.tokens, term_node)

    current_token: Token = factor_node_result.tokens[0]

    if current_token.token_type == TokenType.END_OF_FILE:
        return node_success_for_simple_term

    if current_token.token_type not in TERM_TOKEN_TYPES:
        return node_success_for_simple_term 

    tokens_after_deleting_operator: Tokens = factor_node_result.tokens[1:]

    term_node_operator = (
        ArithmeticOperator.MULTIPLY
        if current_token.token_type == TokenType.MULTIPLY
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
    a mathematical factor.
    """

    FACTOR_TOKEN_TYPES: Tuple[TokenType, ...] = (
        TokenType.DECIMAL_LITERAL, TokenType.TRUE, TokenType.FALSE,
        TokenType.IDENTIFIER, TokenType.STRING_LITERAL,
    )

    # The purpose for not popping off tokens[0] is b/c method_call may need to
    # use it. So it will be deleted later on when required
    current_token: Token = tokens[0]

    if current_token.token_type not in FACTOR_TOKEN_TYPES:
        return report_error_in_parser(current_token.token_type,
                                      current_token.line_number)

    if current_token.token_type != TokenType.IDENTIFIER:        
        tokens_after_deleting_current_token: Tokens = tokens[1:]

        # You may need to add a check for END_OF_FILE but idk yet
        factor_node = FactorNode(current_token.value)
        return NodeSuccess(tokens_after_deleting_current_token, factor_node)

    node_result_for_qual_identifier: NodeResult
    node_result_for_qual_identifier = parse_tokens_for_qualified_identifier(
        tokens
    )

    if isinstance(node_result_for_qual_identifier, NodeFailure):
        return node_result_for_qual_identifier

    assert isinstance(node_result_for_qual_identifier.node, QualifiedIdentifier)

    possible_left_paren_token: Token = node_result_for_qual_identifier.tokens[0]
    if possible_left_paren_token.token_type != TokenType.LEFT_PARENTHESIS:
        factor_node = FactorNode(
            qualified_identifier=node_result_for_qual_identifier.node
        )

        return NodeSuccess(node_result_for_qual_identifier.tokens, factor_node)


    node_result_for_method_call: NodeResult = parse_tokens_for_method_call(
        node_result_for_qual_identifier.tokens,
        node_result_for_qual_identifier.node
    )
    
    if isinstance(node_result_for_method_call, NodeFailure):
        return node_result_for_method_call

    assert isinstance(node_result_for_method_call.node, MethodCall)

    factor_node = FactorNode(method_call=node_result_for_method_call.node)
    return NodeSuccess(node_result_for_method_call.tokens, factor_node)


def parse_tokens_for_qualified_identifier(tokens: Tokens) -> NodeResult:
    """
    This function parses a tuple of tokens in order to turn them into a
    QualifiedIdentifier object.
    """

    # The purpose of having this function check is NOT for when factor calls it
    # Factor will check that there is an identifier.
    # The purpose is when chaining the function for getting additional qual idents,
    # You'll need to check for that CASE
    expected_identifier_token: Token = tokens[0]
    if expected_identifier_token.token_type != TokenType.IDENTIFIER:
        return report_error_in_parser(expected_identifier_token.token_type,
                                      expected_identifier_token.line_number)

    tokens_with_identifier_removed: Tokens = tokens[1:]

    possible_period_token: Token = tokens_with_identifier_removed[0]
    if possible_period_token.token_type != TokenType.PERIOD:
        qualified_identifier = QualifiedIdentifier(expected_identifier_token.value)
        return NodeSuccess(tokens_with_identifier_removed, qualified_identifier)

    tokens_with_period_removed: Tokens = tokens_with_identifier_removed[1:]

    node_result_for_additional_identifier: NodeResult
    node_result_for_additional_identifier = parse_tokens_for_qualified_identifier(
        tokens_with_period_removed
    )

    if isinstance(node_result_for_additional_identifier, NodeFailure):
        return node_result_for_additional_identifier

    assert isinstance(node_result_for_additional_identifier.node,
                      QualifiedIdentifier)

    qualified_identifier = QualifiedIdentifier(
        expected_identifier_token.value,
        node_result_for_additional_identifier.node
    )

    return NodeSuccess(node_result_for_additional_identifier.tokens,
                       qualified_identifier)


def parse_tokens_for_method_call(tokens: Tokens,
                                 qualified_identifer: QualifiedIdentifier
                                 ) -> NodeResult:
    """
    This function parses a list of tokens in order to turn them into a MethodCall
    object.
    """

    # Method call is no longer responsible for removing identifier
    tokens_with_left_paren_removed: Tokens = tokens[1:]

    node_result_argument_list: NodeResult = parse_tokens_for_argument_list(
        tokens_with_left_paren_removed
    )
    
    if isinstance(node_result_argument_list, NodeFailure):
        return node_result_argument_list

    current_token: Token = node_result_argument_list.tokens[0]
    tokens_after_deleting_right_parenthesis: Tokens = \
            node_result_argument_list.tokens[1:]

    if current_token.token_type != TokenType.RIGHT_PARENTHESIS:
        return report_error_in_parser(current_token.token_type,
                                      current_token.line_number)

    assert isinstance(node_result_argument_list.node, ArgumentList)

    method_call = MethodCall(qualified_identifer, node_result_argument_list.node)
    return NodeSuccess(tokens_after_deleting_right_parenthesis, method_call)


def parse_tokens_for_argument_list(tokens: Tokens) -> NodeResult:
    """
    This function parses a list of tokens in order to turn them into an
    ArgumentList object.
    """

    current_token: Token = tokens[0]
    
    if current_token.token_type == TokenType.END_OF_FILE:
        return report_error_in_parser(TokenType.END_OF_FILE,
                                      current_token.line_number)

    # The purpose of this if statement is to account for no argument method calls 
    if current_token.token_type == TokenType.RIGHT_PARENTHESIS:
        argument_list = ArgumentList()
        return NodeSuccess(tokens, argument_list)

    node_result_comp_expression: NodeResult
    node_result_comp_expression = parse_tokens_for_comparison_expression(tokens)

    if isinstance(node_result_comp_expression, NodeFailure):
        return node_result_comp_expression

    NEXT_EXPECTED_TOKEN_TYPES: Tuple[TokenType, TokenType] = (
        TokenType.COMMA,
        TokenType.RIGHT_PARENTHESIS
    )

    next_token: Token = node_result_comp_expression.tokens[0]
    if next_token.token_type not in NEXT_EXPECTED_TOKEN_TYPES:
        return report_error_in_parser(next_token.token_type,
                                      next_token.line_number)

    assert isinstance(node_result_comp_expression.node, ComparisonExpression)

    if next_token.token_type == TokenType.RIGHT_PARENTHESIS:
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


Node = Union[
    ComparisonExpression, ExpressionNode, TermNode, FactorNode,
    MethodCall, ArgumentList, QualifiedIdentifier,
    VariableInitialization, ReturnStatement, VariableIncrement,
    InlineStatement,
    WhileStatement, IfStatement,
    BlockStatement,
    StatementList,
    ParameterList,
    MethodDeclaration, MethodDeclarationList,
    ClassDeclaration,

    ArithmeticOperator, ComparisonOperator
]


def emit_ast_into_output(node: Node, indent_level: int = 0) -> str:
    """
    This is the entrypoint for the emitter.

    This function's purpose is to emit an output (string) based on the abstract
    syntax tree inputted into the function (through the parameter `node`).
    """

    OPENED_PARENTHESIS = "("
    CLOSED_PARENTHESIS = ")"
    COLON = ":"
    PERIOD = "."
    COMMA = ","
    SPACE = " "
    NEW_LINE = "\n"
    EQUALS = "="
    PLUS = "+"
   
    WHILE_KEYWORD = "while"
    RETURN_KEYWORD = "return"
    IF_KEYWORD = "if"
    ELSE_KEYWORD = "else"
    DEF_KEYWORD = "def"
    CLASS_KEYWORD = "class"


    def with_indent(output: str) -> str:
        return indent_level * "    " + output

    match node:
        case ClassDeclaration(identifier, method_declaration_list):
            next_indent_level = indent_level + 1
            result_for_method_dec_list: str = emit_ast_into_output(
                method_declaration_list, next_indent_level
            )

            return (
                CLASS_KEYWORD + SPACE + identifier + COLON + NEW_LINE
                + result_for_method_dec_list
            )

        case MethodDeclarationList(None, None):
            # Possibly return a newline?
            return ""

        case MethodDeclarationList(method_declaration, None):
            assert method_declaration is not None
            return emit_ast_into_output(method_declaration, indent_level)

        case MethodDeclarationList(method_declaration, additional_method_dec_list):
            assert method_declaration is not None
            assert additional_method_dec_list is not None

            result_for_method_dec: str = emit_ast_into_output(method_declaration,
                                                              indent_level)
            result_for_additional_method_dec_list: str = emit_ast_into_output(
                additional_method_dec_list, indent_level
            )

            return result_for_method_dec + result_for_additional_method_dec_list

        case MethodDeclaration(identifier, parameter_list, statement_list):
            next_indent_level = indent_level + 1

            result_for_parameter_list: str = emit_ast_into_output(parameter_list,
                                                                  next_indent_level)
            result_for_statement_list: str = emit_ast_into_output(statement_list,
                                                                  next_indent_level)

            return with_indent(
                DEF_KEYWORD + SPACE + identifier + OPENED_PARENTHESIS
                + result_for_parameter_list + CLOSED_PARENTHESIS + COLON +
                NEW_LINE + result_for_statement_list
            )
        
        case ParameterList(None, None):
            return ""

        case ParameterList(identifier, None):
            assert identifier is not None 
            return identifier

        case ParameterList(identifier, additional_parameter_list):
            assert identifier is not None
            assert additional_parameter_list is not None

            result_for_additional_parameter_list: str = emit_ast_into_output(
                additional_parameter_list, indent_level 
            )

            return (
                identifier + COMMA + SPACE +
                result_for_additional_parameter_list 
            )

        case StatementList(None, None):
            return ""

        case StatementList(statement, None):
            assert statement is not None
            result_for_statement: str = emit_ast_into_output(statement,
                                                             indent_level)
            return with_indent(result_for_statement)

        case StatementList(statement, additional_statement_list):
            assert statement is not None
            assert additional_statement_list is not None

            result_for_statement: str = emit_ast_into_output(statement, indent_level)
            result_for_additional_statement_list: str = emit_ast_into_output(
                additional_statement_list, indent_level
            )

            return with_indent(result_for_statement +
                               result_for_additional_statement_list)
            

        case BlockStatement(statement):
            result_for_statement: str = emit_ast_into_output(statement,
                                                             indent_level)
            return (result_for_statement)
       
        case IfStatement(comparison_expression, statement_list,
                         else_if_statement, else_statement):
            
            result_for_comp_expression: str = emit_ast_into_output(
                comparison_expression, indent_level
            )
            
            next_indent_level = indent_level + 1
            result_for_statement_list: str = emit_ast_into_output(
                statement_list, next_indent_level
            )
            
            result_for_else_if_statement: str = (
                "" if else_if_statement is None
                else with_indent("el" + emit_ast_into_output(else_if_statement,
                                                             indent_level))
            )

            result_for_else_statement: str = (
                "" if else_statement is None
                else with_indent(ELSE_KEYWORD + COLON + NEW_LINE +
                emit_ast_into_output(else_statement, next_indent_level))
            )

            return (
                IF_KEYWORD + SPACE + result_for_comp_expression + COLON
                + NEW_LINE + result_for_statement_list
                + result_for_else_if_statement + result_for_else_statement
            )

        case WhileStatement(comparison_expression, statement_list):
            result_for_comp_expression: str = emit_ast_into_output(
                comparison_expression, indent_level
            )

            next_indent_level = indent_level + 1

            result_for_statement_list: str = emit_ast_into_output(statement_list,
                                                                  next_indent_level)

            return (
                WHILE_KEYWORD + SPACE + result_for_comp_expression + COLON
                + NEW_LINE + result_for_statement_list
            )
        
        case InlineStatement(statement):
            return emit_ast_into_output(statement, indent_level) + NEW_LINE

        case VariableIncrement(identifier, expression):
            result_for_expression: str = emit_ast_into_output(expression, indent_level)

            return (
                identifier + SPACE + PLUS + EQUALS + SPACE +
                result_for_expression
            )
        
        case VariableInitialization(identifier, expression):
            result_for_expression: str = emit_ast_into_output(expression, indent_level)

            return identifier + SPACE + EQUALS + SPACE + result_for_expression

        case ReturnStatement(None):
            return RETURN_KEYWORD

        case ReturnStatement(expression) if expression is not None:
            result_for_expression: str = emit_ast_into_output(expression, indent_level)

            return RETURN_KEYWORD + SPACE + result_for_expression

        case ComparisonExpression(expression, None, None):
            return emit_ast_into_output(expression, indent_level)

        case ComparisonExpression(expression, operator, additional_expression):
            assert operator is not None
            assert additional_expression is not None

            result_for_expression: str = emit_ast_into_output(expression, indent_level)
            result_for_operator: str = emit_ast_into_output(operator, indent_level)
            result_for_additional_expression: str = emit_ast_into_output(
                additional_expression, indent_level
            )

            return (
                result_for_expression + result_for_operator +
                result_for_additional_expression
            )

        case ExpressionNode(single_term_node, None, None):
            return emit_ast_into_output(single_term_node, indent_level)

        case ExpressionNode(single_term_node, operator, additional_expression_node):
            assert operator is not None
            assert additional_expression_node is not None

            result_for_single_factor: str = emit_ast_into_output(single_term_node,
                                                                 indent_level)

            result_for_operator: str = emit_ast_into_output(operator, indent_level)
            result_for_additional_term: str = emit_ast_into_output(
                additional_expression_node, indent_level
            )

            return (
                result_for_single_factor + result_for_operator +
                result_for_additional_term
            )

        case TermNode(single_factor_node, None, None):
            return emit_ast_into_output(single_factor_node, indent_level)

        case TermNode(single_factor_node, operator, additional_term_node):
            assert operator is not None
            assert additional_term_node is not None

            result_for_single_factor: str = emit_ast_into_output(single_factor_node,
                                                                 indent_level)
            result_for_operator: str = emit_ast_into_output(operator, indent_level)
            result_for_additional_term: str = emit_ast_into_output(
                additional_term_node, indent_level
            )

            return (
                result_for_single_factor + result_for_operator +
                result_for_additional_term
            )
        
        case FactorNode(number_or_string, None, None):
            return number_or_string

        case FactorNode("", qualified_identifier, None):
            assert qualified_identifier is not None
            return emit_ast_into_output(qualified_identifier, indent_level)

        case FactorNode("", None, method_call) if method_call is not None:
            return emit_ast_into_output(method_call, indent_level)

        case QualifiedIdentifier(identifier, None):
            return identifier
        
        case QualifiedIdentifier(identifier, additional_identifier):
            assert additional_identifier is not None

            return (
                identifier + PERIOD +
                emit_ast_into_output(additional_identifier)
            )

        case MethodCall(qualified_identifier, argument_list):
            result_for_qualified_identifier: str = emit_ast_into_output(
                qualified_identifier
            )
            result_for_argument_list: str = emit_ast_into_output(argument_list,
                                                                 indent_level)

            output_qualified_identifier: str

            match result_for_qualified_identifier:
                case "System.out.println":
                    output_qualified_identifier = "print"
                
                case _:
                    output_qualified_identifier = result_for_qualified_identifier


            return (
                output_qualified_identifier + OPENED_PARENTHESIS +
                result_for_argument_list + CLOSED_PARENTHESIS
            )

        case ArgumentList(None, _):
            return ""

        case ArgumentList(argument, None) if argument is not None:
            return emit_ast_into_output(argument, indent_level)

        case ArgumentList(argument, additional_argument_list):

            # Assertions are cleaner than a verbose guard clause
            assert argument is not None
            assert additional_argument_list is not None

            result_for_argument: str = emit_ast_into_output(argument, indent_level)
            result_for_additional_argument_list: str = emit_ast_into_output(
                additional_argument_list, indent_level
            )

            return (
                result_for_argument + COMMA + SPACE +
                result_for_additional_argument_list
            )


        case _:
            OPERATOR_TYPES: tuple = (ArithmeticOperator, ComparisonOperator)
            if isinstance(node, OPERATOR_TYPES):
                return node.value

            # This should be unreachable
            assert False


def supported_features_of_transpiler() -> Tuple[str, ...]:
    """
    This function's purpose is to return a Tuple of strings with each string
    being a name of a feature that is supported by the transpiler.
    """

    return (
        "Class Declarations",
        "Method Declarations",
        "If Statements",
        "While Statements",
        "Inline Statements",
        "Variable Increment",
        "Return Statements",
        "Comparison Expressions",
        "Expressions",
        "String Literals",
        "Decimal Literals",
        "Method Calls",
        "Qualified Identifiers",
    )

