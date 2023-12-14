from __future__ import annotations

"""
This module's purpose is to tokenize user input (some Java source code string)
"""

from dataclasses import dataclass, field
import re
from enum import Enum
from typing import List, Optional, Union, Tuple, AnyStr


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


@dataclass
class ParserResult:
    """
    This class defines the parser output.

    Either 'syntax_tree' or 'error_message' must have a value; they cannot both be
    empty.
    
    Specifically, if 'syntax_tree' is None, it indicates an error, and
    'error_message' must be a non-empty string.
    
    Conversely, if 'tokens' is not None, 'error_message' must be an
    empty string.
    """

    was_successful: bool
    syntax_tree: Optional[ExpressionNode] = None
    error_message: str = ""


@dataclass
class NodeResult:
    """
    This class defines the output of a node.

    Either ('tokens' and 'node') or 'error_message' must have a value; they
    cannot all be empty.
    
    Specifically, if ('tokens' is an empty list) or (node is None), it
    indicates an error, and 'error_message' must be a non-empty string.
    
    Conversely, if ('tokens' is not an empty list) and (node is not None),
    'error_message' must be an empty string.
    """

    was_successful: bool
    tokens: Optional[List[Token]] = field(default_factory=list)
    node: Union[ExpressionNode, TermNode, FactorNode, None] = None
    error_message: str = ""


# Type alias that is used for ExpressionNode and TermNode
ArithmeticOperatorMember = str


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
    operator: Optional[ArithmeticOperatorMember] = None
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
    operator: Optional[ArithmeticOperatorMember] = None
    second_factor_node: Optional[FactorNode] = None


@dataclass
class FactorNode:
    """
    Represents a node in the abstract syntax tree (AST) for a numerical factor.

    Attributes:
    - number (str): The numerical value associated with this factor.
    """
    number: str


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


def parse_list_of_tokens(tokens: List[Token]) -> ParserResult:
    """ This functions purpose is to be the entrypoint for the parser """


    class ParserErrorReason:
        """
        Enumeration class representing reasons for parser errors.

        Enum Members:
        
        - UNEXPECTED_TOKEN_TYPE: Indicates an unexpected token type during
        parsing. Usage: UNEXPECTED_TOKEN_TYPE.format(token_type)
        
        - VALUE_IS_NULL: Indicates finding a null value during parsing. Usage:
        VALUE_IS_NULL.format(value_name)
        
        - UNEXPECTED_TYPE: Indicates an unexpected node type during parsing.
        Usage: UNEXPECTED_TYPE.format(node_type)
        """
        
        UNEXPECTED_TOKEN_TYPE = "Unexpected Token Type, {0}"
        VALUE_IS_NULL = "Found A Null Value; {0} is Null"
        UNEXPECTED_TYPE = "Unexpected Node of Type, {0}"


    def report_error(unexpected_token_type: Optional[TokenType] = None,
                     unexpected_null: Optional[AnyStr] = None,
                     unexpected_type: Optional[AnyStr] = None) -> NodeResult:
        """
        This function's purpose is to report an error found in the parser
        """

        error_message = ""

        if unexpected_token_type is not None:
            error_message: str = ParserErrorReason.UNEXPECTED_TOKEN_TYPE.format(
                    unexpected_token_type)
        
        if unexpected_null is not None:
            error_message: str = ParserErrorReason.VALUE_IS_NULL.format(unexpected_null)

        if unexpected_type is not None:
            error_message: str = ParserErrorReason.UNEXPECTED_TYPE.format(unexpected_type)

        node_result = NodeResult(False, error_message=error_message)
        return node_result


    def parse_tokens_for_expression(tokens: List[Token]) -> NodeResult:
        """
        Parses a list of tokens to construct an abstract syntax tree (AST) for
        a mathematical expression.

        Args:
        - tokens (List[Token]): List of tokens to be parsed.

        Returns:
        - NodeResult: A result object containing information about the success
        of the parsing operation. If successful, the result includes the parsed
        expression node; otherwise, it contains an error message.
        """
        
        EXPRESSION_TOKEN_TYPES: Tuple[TokenType, TokenType] = (PLUS_TOKEN_TYPE,
                                                               MINUS_TOKEN_TYPE)
 
        term_node_result: NodeResult = parse_tokens_for_term(tokens)

        if not term_node_result.was_successful:
            return term_node_result

        if not isinstance(term_node_result.node, TermNode):

            type_of_term_node = type(term_node_result.node)
            UNEXPECTED_TYPE = str(type_of_term_node)

            unsuccessful_result: NodeResult = report_error(unexpected_type=UNEXPECTED_TYPE)
            return unsuccessful_result

        first_term_node: TermNode = term_node_result.node
        expression_node = ExpressionNode(first_term_node)

        node_result_for_simple_expression = NodeResult(True, term_node_result.tokens,
                                                       expression_node)
        
        if node_result_for_simple_expression.tokens is None:
            FOUND_NULL_VALUE = "Node Result for Simple Expression is None"            

            unsuccessful_result: NodeResult = report_error(unexpected_null=FOUND_NULL_VALUE)
            return unsuccessful_result

        length_of_tokens = len(node_result_for_simple_expression.tokens)

        if length_of_tokens == 0:
            return node_result_for_simple_expression 

        # We don't pop the token off b/c there's a chance it's not a + or -
        current_token = tokens[0]

        if current_token.token_type not in EXPRESSION_TOKEN_TYPES:
            return node_result_for_simple_expression 

        del tokens[0]

        expression_node_operator: ArithmeticOperatorMember 

        if current_token.token_type == PLUS_TOKEN_TYPE:
            expression_node_operator = ArithmeticOperator.PLUS
        else:
            expression_node_operator = ArithmeticOperator.MINUS
       
        if term_node_result.tokens is None:
            FOUND_NULL_VALUE = "Term Node Result Tokens is None"

            unsuccessful_result: NodeResult = report_error(unexpected_null=FOUND_NULL_VALUE)
            return unsuccessful_result

        tokens = term_node_result.tokens 

        additional_expression_node_result: NodeResult = parse_tokens_for_expression(tokens)
        
        if not additional_expression_node_result.was_successful:
            return additional_expression_node_result

        if additional_expression_node_result.tokens is None:
            FOUND_NULL_VALUE = "Additional Expression Node Result is None"

            unsuccessful_result: NodeResult = report_error(unexpected_null=FOUND_NULL_VALUE)
            return unsuccessful_result

        tokens = additional_expression_node_result.tokens

        additional_expression_node = additional_expression_node_result.node

        if not isinstance(additional_expression_node, ExpressionNode):
            type_of_additional_expression_node = type(additional_expression_node)
            UNEXPECTED_TYPE = str(type_of_additional_expression_node)

            unsuccessful_result: NodeResult = report_error(unexpected_type=UNEXPECTED_TYPE)
            return unsuccessful_result

        complex_term_node = ExpressionNode(expression_node.single_term_node,
                                           expression_node_operator,
                                           additional_expression_node)

        return NodeResult(True, tokens, complex_term_node)
 

    def parse_tokens_for_term(tokens: List[Token]) -> NodeResult:
        """
        Parses a list of tokens to construct an abstract syntax tree (AST) for
        a mathematical term.

        Args:
        - tokens (List[Token]): List of tokens to be parsed.

        Returns:
        - NodeResult: A result object containing information about the success
        of the parsing operation. If successful, the result includes the parsed
        term node; otherwise, it contains an error message.
        """


        TERM_TOKEN_TYPES: Tuple[TokenType, TokenType] = (MULTIPLY_TOKEN_TYPE,
                                                         DIVIDE_TOKEN_TYPE)

        factor_node_result: NodeResult = parse_tokens_for_factor(tokens)

        if not factor_node_result.was_successful:
            return factor_node_result

        if not isinstance(factor_node_result.node, FactorNode):
            type_of_factor_node_result_node = type(factor_node_result.node)
            UNEXPECTED_TYPE = str(type_of_factor_node_result_node)

            unsuccessful_result: NodeResult = report_error(unexpected_type=UNEXPECTED_TYPE)
            return unsuccessful_result

        first_factor_node: FactorNode = factor_node_result.node
        term_node = TermNode(first_factor_node)

        node_result_for_simple_term = NodeResult(True, factor_node_result.tokens, term_node)

        if node_result_for_simple_term.tokens is None:
            FOUND_NULL_VALUE = "Node Result for Simple Term's Tokens"
            unsuccessful_result: NodeResult = report_error(unexpected_null=FOUND_NULL_VALUE)
            return unsuccessful_result

        length_of_tokens: int = len(node_result_for_simple_term.tokens)

        if length_of_tokens == 0:
            return node_result_for_simple_term 

        # We don't pop the token off b/c there's a chance it's not a * or /
        current_token = tokens[0]

        if current_token.token_type not in TERM_TOKEN_TYPES:
            return node_result_for_simple_term 

        del tokens[0]

        term_node_operator: ArithmeticOperatorMember 

        if current_token.token_type == MULTIPLY_TOKEN_TYPE:
            term_node_operator = ArithmeticOperator.MULTIPLY
        else:
            term_node_operator = ArithmeticOperator.DIVIDE

        if factor_node_result.tokens is not None:
            tokens = factor_node_result.tokens

        second_factor_node_result: NodeResult = parse_tokens_for_factor(tokens)
        
        if not second_factor_node_result.was_successful:
            return second_factor_node_result

        second_factor_node_for_complex_term_node = second_factor_node_result.node

        if second_factor_node_result.tokens is None:
            unsuccessful_result: NodeResult = report_error("Null Error")
            return unsuccessful_result

        tokens = second_factor_node_result.tokens

        if not isinstance(second_factor_node_for_complex_term_node, FactorNode):
            unsuccessful_result: NodeResult = report_error("nil")
            return unsuccessful_result

        complex_term_node = TermNode(term_node.first_factor_node,
                                     term_node_operator,
                                     second_factor_node_for_complex_term_node)

        return NodeResult(True, tokens, complex_term_node)
            

    def parse_tokens_for_factor(tokens: List[Token]) -> NodeResult:
        """
        Parses a list of tokens to construct an abstract syntax tree (AST) for
        a mathematical term.

        Args:
        - tokens (List[Token]): List of tokens to be parsed.

        Returns:
        - NodeResult: A result object containing information about the success
        of the parsing operation. If successful, the result includes the parsed
        term node; otherwise, it contains an error message.
        """

        FACTOR_TOKEN_TYPES: List[str] = [
            DECIMAL_LITERAL_TOKEN_TYPE, TRUE_TOKEN_TYPE, FALSE_TOKEN_TYPE,
            IDENTIFIER_TOKEN_TYPE,
        ]

        current_token = tokens.pop(0)

        if current_token.token_type not in FACTOR_TOKEN_TYPES:
            return report_error(current_token.token_type)

        factor_node = FactorNode(current_token.value)

        return NodeResult(True, tokens, factor_node)


    root_node_result: NodeResult = parse_tokens_for_expression(tokens)

    if not root_node_result.was_successful:
        return ParserResult(False, error_message=root_node_result.error_message)

    if not isinstance(root_node_result.node, ExpressionNode):

        unsuccessful_result: NodeResult = report_error(str(root_node_result.node))
        return ParserResult(False, error_message=unsuccessful_result.error_message)

    root_node: ExpressionNode = root_node_result.node
    return ParserResult(True, root_node)

