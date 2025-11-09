USER-INTERFACE SECTION.
  Ask.
    DISPLAY FUNCTION TRIM(question) ": " WITH NO ADVANCING
    ACCEPT response
    PERFORM Trim-Response
    PERFORM Validate-Response.

  Ask-Number.
    PERFORM Ask.
    PERFORM Validate-Number.

  Confirm.
    DISPLAY FUNCTION TRIM(question) "? (Y/N): " WITH NO ADVANCING
    ACCEPT response
    PERFORM Normalize-Response

    IF NOT confirmed
      SET denied TO TRUE
    END-IF.

  Clear-Input-Data.
    MOVE SPACES TO question
    MOVE SPACES TO response
    MOVE ZEROS  TO input-number
    SET empty-input TO TRUE.

RESPONSE-FORMATTING SECTION.
  Normalize-Response.
    PERFORM Trim-Response
    MOVE FUNCTION UPPER-CASE(response) TO response.

  Trim-Response.
    MOVE FUNCTION TRIM(response) TO response.

RESPONSE-VALIDATION SECTION.
  Validate-Response.
    IF response = SPACES
      SET empty-input TO TRUE
    ELSE 
      SET valid-text TO TRUE
    END-IF.

  Validate-Number.
    IF empty-input *> This is a necessary redundancy
      SET invalid-number TO TRUE
      MOVE ZEROS TO input-number
    ELSE IF FUNCTION NUMVAL(response) IS NUMERIC
      SET valid-number TO TRUE
      MOVE FUNCTION NUMVAL(response) TO input-number
    ELSE
      SET invalid-number TO TRUE
      MOVE ZEROS TO input-number
    END-IF.
