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
END_OF_FILE_TOKEN_TYPE: str = "EOF"

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
class LexerFailure:
    """
    Represents an error occurring in the Lexer.

    error_message is the message to be printed to the user
    """

    error_message: str


ERROR_MESSAGE_FOR_LEXER: str = "Found an unknown character, '{0}'"


def report_error_for_lexer(unknown_character: str) -> LexerFailure:
    """
    This function's purpose is to report an error that occurred in the
    lexer.
    """

    error_message: str = ERROR_MESSAGE_FOR_LEXER.format(unknown_character)

    return LexerFailure(error_message)


LexerResult = List[Token] | LexerFailure


def scan_and_tokenize_input(user_input: str) -> LexerResult:
    """ This function's purpose is to be the entrypoint for the lexer. """

    tokens: List[Token] = []
    position: int = 0
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
                token: Token = Token(token_type, token_value) 
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
        
        token_type_not_identifier: bool = token.token_type != IDENTIFIER_TOKEN_TYPE
        keywords_keys: KeysView = KEYWORDS.keys()

        if token_type_not_identifier or (token.value not in keywords_keys):
            return token

        keyword_token_type: str = KEYWORDS[token.value]

        # The value is kind of redundant
        keyword_value: str = token.value
        keyword_token: Token = Token(keyword_token_type, keyword_value)

        return keyword_token


    tokens_with_keywords: map[Token] = map(change_identifier_to_keyword, tokens)
    tokens_with_keywords_as_list: List[Token] = list(tokens_with_keywords)

    # Reminder, let's stop using mutable data and switch to immutable data
    end_of_file_token: Token = Token(END_OF_FILE_TOKEN_TYPE, "")
    tokens_with_keywords_as_list.append(end_of_file_token)

    return tokens_with_keywords_as_list


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

    tokens: List[Token]
    node: Union[ExpressionNode, TermNode, FactorNode]


@dataclass
class NodeFailure:
    """
    Represents an error the occurs in a node.

    `error_message` is the message to be printed for the user.
    """

    error_message: str


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

    first_factor_node: FactorNode
    operator: Optional[ArithmeticOperator] = None
    second_factor_node: Optional[FactorNode] = None


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

    argument is represented by an expression node.

    additonal_argument_list represents additional arguments. This
    field defaults to None; when it is None, it represents a method being called
    with a singular argument.
    """

    argument: ExpressionNode
    additional_argument_list: Optional[ArgumentList] = None


NodeResult = Union[NodeSuccess, NodeFailure]


def report_error_in_parser(unexpected_token_type: TokenType) -> NodeFailure:
    """
    This function's purpose is to report an error found in the parser
    """

    ERROR_MESSAGE: str = "Unexpected token type, {0}"
    error_message: str = ERROR_MESSAGE.format(unexpected_token_type)

    return NodeFailure(error_message)


def parse_list_of_tokens(tokens: List[Token]) -> Union[ExpressionNode, ParserFailure]:
    """ This functions purpose is to be the entrypoint for the parser """

    root_node_result: NodeResult = parse_tokens_for_expression(tokens)

    if isinstance(root_node_result, NodeFailure):
        return ParserFailure(root_node_result.error_message)

    assert isinstance(root_node_result.node, ExpressionNode)

    return root_node_result.node


def parse_tokens_for_expression(tokens: List[Token]) -> NodeResult:
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
    expression_node = ExpressionNode(term_node_result.node)

    node_success_for_simple_expression: NodeSuccess = \
            NodeSuccess(term_node_result.tokens, expression_node)

    # We don't pop the token off b/c there's a chance it's not a + or -
    current_token: Token = tokens[0]

    if current_token.token_type == END_OF_FILE_TOKEN_TYPE:
        return node_success_for_simple_expression

    if current_token.token_type not in EXPRESSION_TOKEN_TYPES:
        return node_success_for_simple_expression 

    del tokens[0]

    expression_node_operator: ArithmeticOperator 

    if current_token.token_type == PLUS_TOKEN_TYPE:
        expression_node_operator = ArithmeticOperator.PLUS
    else:
        expression_node_operator = ArithmeticOperator.MINUS

    additional_expression_node_result: NodeResult = \
            parse_tokens_for_expression(term_node_result.tokens)
    
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


def parse_tokens_for_term(tokens: List[Token]) -> NodeResult:
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

    node_success_for_simple_term: NodeSuccess = \
            NodeSuccess(factor_node_result.tokens, term_node)

    # We don't pop the token off b/c there's a chance it's not a * or /
    current_token = tokens[0]

    if current_token.token_type == END_OF_FILE_TOKEN_TYPE:
        return node_success_for_simple_term 

    if current_token.token_type not in TERM_TOKEN_TYPES:
        return node_success_for_simple_term 

    del tokens[0]

    term_node_operator: ArithmeticOperator 

    if current_token.token_type == MULTIPLY_TOKEN_TYPE:
        term_node_operator = ArithmeticOperator.MULTIPLY
    else:
        term_node_operator = ArithmeticOperator.DIVIDE

    second_factor_node_result: NodeResult = parse_tokens_for_factor(tokens)
   
    if isinstance(second_factor_node_result, NodeFailure):
        return second_factor_node_result

    assert isinstance(second_factor_node_result.node, FactorNode)

    complex_term_node: TermNode = TermNode(term_node.first_factor_node,
                                           term_node_operator,
                                           second_factor_node_result.node)

    return NodeSuccess(second_factor_node_result.tokens, complex_term_node)


def parse_tokens_for_factor(tokens: List[Token]) -> NodeResult:
    """
    Parses a list of tokens to construct an abstract syntax tree (AST) for
    a mathematical term.
    """

    FACTOR_TOKEN_TYPES: List[str] = [
        DECIMAL_LITERAL_TOKEN_TYPE, TRUE_TOKEN_TYPE, FALSE_TOKEN_TYPE,
        IDENTIFIER_TOKEN_TYPE,
    ]

    current_token: Token = tokens.pop(0)

    if current_token.token_type not in FACTOR_TOKEN_TYPES:
        return report_error_in_parser(current_token.token_type)

    factor_node: FactorNode = FactorNode(number_or_identifier=current_token.value)
    return NodeSuccess(tokens, factor_node)

