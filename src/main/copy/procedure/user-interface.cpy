USER-INTERFACE SECTION.
  UI-Ask-Normalized.
    PERFORM UI-Ask
    PERFORM UI-Normalize-Response.

  UI-Ask-Number.
    PERFORM UI-Ask
    PERFORM UI-Trim-Response.
    PERFORM UI-Validate-Number.
  *> IMPORTANT: USE `FUNCTION NUMVAL` WHEN MOVING UI-NUMBER INTO NUMERIC ITEMS WITH FEWER DIGITS

  UI-Ask.
    DISPLAY FUNCTION TRIM(ui-prompt) ": " WITH NO ADVANCING
    ACCEPT ui-response
    PERFORM UI-Validate-Response.

  UI-Confirm.
    DISPLAY FUNCTION TRIM(ui-prompt) "? (Y/N): " WITH NO ADVANCING
    ACCEPT ui-response
    PERFORM UI-Normalize-Response
    MOVE ui-response TO ui-answer

    IF NOT ui-confirmed
      SET ui-denied TO TRUE
    END-IF.

  UI-Clear-Data.
    MOVE SPACES TO ui-prompt
    MOVE SPACES TO ui-response
    MOVE SPACES TO ui-answer
    MOVE ZEROS  TO ui-number
    SET ui-empty-answer TO TRUE.

ANSWER-FORMATTING SECTION.
  UI-Normalize-Response.
    PERFORM UI-Trim-Response
    MOVE FUNCTION UPPER-CASE(ui-response) TO ui-response
    PERFORM UI-Validate-Response.

  UI-Trim-Response.
    MOVE FUNCTION TRIM(ui-response) TO ui-response.

ANSWER-VALIDATION SECTION.
  UI-Validate-Response.
    IF ui-response = SPACES
      SET ui-empty-answer TO TRUE
    ELSE 
      SET ui-valid-text TO TRUE
    END-IF.

  UI-Validate-Number.
    IF ui-empty-answer
      SET ui-invalid-number TO TRUE
      MOVE 0 TO ui-number
      EXIT PARAGRAPH
    END-IF 

    IF FUNCTION TRIM(ui-response) IS NUMERIC
      SET ui-valid-number TO TRUE
      MOVE FUNCTION TRIM(ui-response) TO ui-number
    ELSE
      SET ui-invalid-number TO TRUE
      MOVE 0 TO ui-number
    END-IF.

*> COPY "src/main/copy/procedure/user-interface.cpy".
