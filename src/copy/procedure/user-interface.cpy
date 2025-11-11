USER-INTERFACE SECTION.
  UI-Ask-Normalized.
    PERFORM UI-Ask
    PERFORM UI-Normalize-Answer.

  UI-Ask.
    DISPLAY FUNCTION TRIM(ui-prompt) ": " WITH NO ADVANCING
    ACCEPT ui-answer
    PERFORM UI-Trim-Answer
    PERFORM UI-Validate-Answer.

  UI-Ask-Number.
    PERFORM UI-Ask.
    PERFORM UI-Validate-Number.

  UI-Confirm.
    DISPLAY FUNCTION TRIM(ui-prompt) "? (Y/N): " WITH NO ADVANCING
    ACCEPT ui-answer
    PERFORM UI-Normalize-Answer

    IF NOT ui-confirmed
      SET ui-denied TO TRUE
    END-IF.

  UI-Clear-Data.
    MOVE SPACES TO ui-prompt
    MOVE SPACES TO ui-answer
    MOVE ZEROS  TO ui-number
    SET ui-empty-answer TO TRUE.

ANSWER-FORMATTING SECTION.
  UI-Normalize-Answer.
    PERFORM UI-Trim-Answer
    MOVE FUNCTION UPPER-CASE(ui-answer) TO ui-answer.

  UI-Trim-Answer.
    MOVE FUNCTION TRIM(ui-answer) TO ui-answer.

ANSWER-VALIDATION SECTION.
  UI-Validate-Answer.
    IF ui-answer = SPACES
      SET ui-empty-answer TO TRUE
    ELSE 
      SET ui-valid-text TO TRUE
    END-IF.

  UI-Validate-Number.
    IF ui-empty-answer *> This is a necessary redundancy
      SET ui-invalid-number TO TRUE
      MOVE ZEROS TO ui-number
    ELSE IF FUNCTION NUMVAL(ui-answer) IS NUMERIC
      SET ui-valid-number TO TRUE
      MOVE FUNCTION NUMVAL(ui-answer) TO ui-number
    ELSE
      SET ui-invalid-number TO TRUE
      MOVE ZEROS TO ui-number
    END-IF.
