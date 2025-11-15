USER-INTERFACE SECTION.
  UI-Ask-Normalized.
    PERFORM UI-Ask
    PERFORM UI-Normalize-Response.

  UI-Ask-Number.
    PERFORM UI-Ask.
    PERFORM UI-Validate-Number.

  UI-Confirm.
    DISPLAY FUNCTION TRIM(ui-prompt) "? (Y/N): " WITH NO ADVANCING
    ACCEPT ui-response
    PERFORM UI-Normalize-Response
    MOVE ui-response TO ui-answer

    IF NOT ui-confirmed
      SET ui-denied TO TRUE
    END-IF.

  UI-Ask.
    DISPLAY FUNCTION TRIM(ui-prompt) ": " WITH NO ADVANCING
    ACCEPT ui-response.

  UI-Clear-Data.
    MOVE SPACES TO ui-prompt
    MOVE SPACES TO ui-response
    MOVE SPACES TO ui-answer
    MOVE ZEROS  TO ui-number
    SET ui-empty-answer TO TRUE.

ANSWER-FORMATTING SECTION.
  UI-Normalize-Response.
    PERFORM UI-Trim-Answer
    MOVE FUNCTION UPPER-CASE(ui-response) TO ui-response
    PERFORM UI-Validate-Answer.

  UI-Trim-Answer.
    MOVE FUNCTION TRIM(ui-response) TO ui-response.

ANSWER-VALIDATION SECTION.
  UI-Validate-Answer.
    IF ui-response = SPACES
      SET ui-empty-answer TO TRUE
    ELSE 
      SET ui-valid-text TO TRUE
    END-IF.

  UI-Validate-Number.
    IF ui-empty-answer *> This is a necessary redundancy
      SET ui-invalid-number TO TRUE
      MOVE ZEROS TO ui-number
      EXIT PARAGRAPH
    END-IF 

    IF FUNCTION NUMVAL(ui-response) IS NUMERIC
      SET ui-valid-number TO TRUE
      MOVE FUNCTION NUMVAL(ui-response) TO ui-number
    ELSE
      SET ui-invalid-number TO TRUE
      MOVE ZEROS TO ui-number
    END-IF.
