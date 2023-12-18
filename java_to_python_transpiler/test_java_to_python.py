from java_to_python_transpiler.java_to_python import (ERROR_MESSAGE_FOR_LEXER,
                                                      LexerFailure,
                                                      report_error_for_lexer)


def test_report_error_for_lexer_returns_proper_error_object():
    """
    This test checks if the function `report_error_for_lexer` returns
    the correct LexerFailure object.
    """

    UNKNOWN_CHARACTER: str = "~"

    error_message: str= ERROR_MESSAGE_FOR_LEXER.format(UNKNOWN_CHARACTER)
    expected_output: LexerFailure = LexerFailure(error_message)

    report_error_output: LexerFailure = report_error_for_lexer(UNKNOWN_CHARACTER)

    assert expected_output == report_error_output

