IDENTIFICATION DIVISION.
PROGRAM-ID. Character-Editor.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
  SELECT Character-Roster ASSIGN TO "data/characters.dat"
  ORGANIZATION IS INDEXED
  ACCESS MODE IS DYNAMIC
  RECORD KEY IS short-name
  FILE STATUS IS File-Status.

DATA DIVISION.
FILE SECTION.
FD Character-Roster.
  COPY "src/copy/file-description/character-record.cpy".

WORKING-STORAGE SECTION.
  COPY "src/copy/working-storage/user-interface-data.cpy".
  COPY "src/copy/working-storage/character-data.cpy".
  COPY "src/copy/working-storage/main-cast-data.cpy".

  78 ACT-CREATE         VALUE "CREATE-CHARACTER".
  78 ACT-EDIT           VALUE "EDIT-CHARACTER".
  78 ACT-DELETE         VALUE "DELETE-CHARACTER".
  78 ACT-ALL            VALUE "LIST-ALL".
  78 ACT-LIST           VALUE "LIST-CHARACTER".
  78 ACT-RESET          VALUE "RESET-FILE".
  78 ACT-QUIT           VALUE "QUIT".
  78 MENU-TABLE-LENGTH  VALUE 7.

  01 Mode-Status PIC 9 VALUE ZERO.
    88 menu-mode   VALUE 0.
    88 quit-mode   VALUES 1 THROUGH 9.

  01 Character-Status PIC 9 VALUE ZEROS.
    88 invalid-character  VALUE 0.
    88 new-character      VALUE 1.
    88 existing-character VALUE 2.

  01 Menu-Table.
    02 Menu-Entry     OCCURS MENU-TABLE-LENGTH TIMES INDEXED BY MENU-INDEX.
      03 menu-key     PIC X.
      03 menu-action  PIC X(16).

  01 File-Status    PIC 99.
    88 end-of-file  VALUE 10.

PROCEDURE DIVISION.
  PERFORM Initialize-Trait-Table.
  PERFORM Initialize-Menu-Table.

  Main-Logic.
    SET menu-mode TO TRUE.

    PERFORM UNTIL quit-mode
      SET menu-mode TO TRUE
      PERFORM Editor-Menu
      PERFORM Editor-Action
    END-PERFORM.
  STOP RUN.

  Editor-Menu.
    PERFORM VARYING MENU-INDEX FROM 1 BY 1 UNTIL MENU-INDEX > MENU-TABLE-LENGTH
      DISPLAY "[" menu-key(MENU-INDEX) "] " menu-action(MENU-INDEX)
    END-PERFORM

    MOVE "CHOOSE ACTION" TO ui-prompt
    PERFORM UI-Ask-Normalized.

  Editor-Action.
    SET MENU-INDEX TO 1
    SEARCH Menu-Entry
      AT END DISPLAY "INVALID ACTION."
      WHEN menu-key(MENU-INDEX) = ui-head
        PERFORM UI-Clear-Data
        EVALUATE menu-action(MENU-INDEX)
          WHEN ACT-CREATE PERFORM Try-Create-Character  UNTIL ui-denied
          WHEN ACT-EDIT   PERFORM Try-Edit-Character    UNTIL ui-denied
          WHEN ACT-DELETE PERFORM Try-Delete-Character  UNTIL ui-denied
          WHEN ACT-ALL    PERFORM List-Characters
          WHEN ACT-RESET  PERFORM Try-Reset-File
          WHEN ACT-QUIT   SET quit-mode TO TRUE
          WHEN OTHER      DISPLAY "NOT IMPLEMENTED"
        END-EVALUATE
    END-SEARCH.

  Try-Reset-File.
    MOVE "ARE YOU SURE YOU WANT TO RESET THE WHOLE ROSTER?" TO ui-prompt
    PERFORM UI-Confirm

    IF ui-confirmed
      DISPLAY "ERASING FILE..."
      OPEN OUTPUT Character-Roster
      CLOSE Character-Roster
    ELSE
      DISPLAY "RESET CANCELLED."
    END-IF.

  Try-Create-Character.
    PERFORM Lookup-Key

    EVALUATE TRUE
    WHEN new-character
      DISPLAY "CREATING NEW CHARACTER..."
      PERFORM Assign-All-Fields
      PERFORM Confirm-Record-Character
      IF ui-confirmed
        PERFORM Record-New-Character
      END-IF
    WHEN existing-character
      DISPLAY "CANNOT CREATE EXISTING CHARACTER."
    WHEN invalid-character
      DISPLAY "CANNOT CREATE INVALID CHARACTER: " ui-response
    WHEN OTHER
      DISPLAY "INVARIANT VIOLATION: INVALID CHARACTER-STATUS"
    END-EVALUATE

    MOVE "CREATE ANOTHER CHARACTER" TO ui-prompt
    PERFORM UI-Confirm.

  Try-Edit-Character.
    
    PERFORM Lookup-Key

    EVALUATE TRUE
    WHEN new-character
      DISPLAY "CANNOT EDIT NEW CHARACTER."
    WHEN existing-character
      DISPLAY "EDITING CHARACTER..."
      PERFORM Developer-View-Character
      PERFORM Select-Field UNTIL ui-denied
      PERFORM Confirm-Record-Character
      IF ui-confirmed 
        PERFORM Update-Character
      END-IF
    WHEN invalid-character
      DISPLAY "CANNOT EDIT INVALID CHARACTER: " ui-response
    WHEN OTHER
      DISPLAY "INVARIANT VIOLATION: INVALID CHARACTER-STATUS"
    END-EVALUATE
    
    MOVE "EDIT ANOTHER CHARACTER" TO ui-prompt
    PERFORM UI-Confirm.

  Try-Delete-Character.
    
    PERFORM Lookup-Key

    EVALUATE TRUE
    WHEN new-character
      DISPLAY "CANNOT DELETE NEW CHARACTER."
    WHEN existing-character
      PERFORM Developer-View-Character
      MOVE "ARE YOU SURE YOU WANT TO DELETE THIS CHARACTER" TO ui-prompt
      PERFORM UI-Confirm

      IF ui-confirmed
        OPEN I-O Character-Roster
          DELETE Character-Roster
            INVALID KEY DISPLAY "INVARIANT VIOLATION: TRIED TO DELETE INVALID KEY."
          END-DELETE
        CLOSE Character-Roster
      END-IF
    WHEN invalid-character
      DISPLAY "CANNOT DELETE INVALID CHARACTER: " ui-response
    WHEN OTHER
      DISPLAY "INVARIANT VIOLATION: INVALID CHARACTER-STATUS"
    END-EVALUATE

    MOVE "DELETE ANOTHER CHARACTER" TO ui-prompt
    PERFORM UI-Confirm.

  List-Characters.
      OPEN INPUT Character-Roster
      MOVE LOW-VALUE TO short-name
      START Character-Roster KEY >= short-name
        INVALID KEY DISPLAY "NO RECORDS FOUND"
        NOT INVALID KEY
          PERFORM UNTIL end-of-file
            READ Character-Roster NEXT RECORD
              AT END
                SET end-of-file TO TRUE
              NOT AT END
                PERFORM Developer-View-Character
            END-READ
          END-PERFORM
        END-START
      CLOSE Character-Roster.

CHARACTER-EDIT SECTION.
  Assign-All-Fields.
    PERFORM Reset-Index

    PERFORM UNTIL TRAIT-INDEX > TRAIT-TABLE-LENGTH
      PERFORM Assign-Field
      PERFORM Increment-Index
    END-PERFORM.

  Assign-Field.
    PERFORM UI-Clear-Data
    EVALUATE trait-code(TRAIT-INDEX)
      WHEN "LONG-NAME"
        MOVE "ENTER LONG NAME (21)" TO ui-prompt
        PERFORM UI-Ask
        IF ui-empty-answer
          MOVE trait-default(TRAIT-INDEX) TO long-name
        ELSE
          MOVE ui-response TO long-name
        END-IF
      WHEN "AGE"
        MOVE "ENTER AGE (##)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(trait-default(TRAIT-INDEX)) TO age
        ELSE
          MOVE ui-number TO age
        END-IF
      WHEN "BIRTH-MONTH"
        MOVE "ENTER BIRTH MONTH (##)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(trait-default(TRAIT-INDEX)) TO birth-month
        ELSE
          MOVE ui-number TO birth-month
        END-IF
      WHEN "BIRTH-DAY"
        MOVE "ENTER BIRTH DAY (##)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(trait-default(TRAIT-INDEX)) TO birth-day
        ELSE
          MOVE ui-number TO birth-day
        END-IF
      WHEN "HEIGHT"
        MOVE "ENTER HEIGHT (Fii)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(trait-default(TRAIT-INDEX)) TO height
        ELSE
          MOVE ui-number TO height
        END-IF
      WHEN "GENDER"
        MOVE "ENTER GENDER (10)" TO ui-prompt
        PERFORM UI-Ask
        IF ui-empty-answer
          MOVE trait-default(TRAIT-INDEX) TO gender
        ELSE
          PERFORM UI-Normalize-Response
          MOVE ui-response TO gender
        END-IF
      WHEN "RACE"
        MOVE "ENTER RACE (20)" TO ui-prompt
        PERFORM UI-Ask
        IF ui-empty-answer
          MOVE trait-default(TRAIT-INDEX) TO race
        ELSE
          PERFORM UI-Normalize-Response
          MOVE ui-response TO race
        END-IF
      WHEN "DESCRIPTION"
        MOVE "ENTER INFO (80)" TO ui-prompt
        PERFORM UI-Ask
        IF ui-empty-answer
          MOVE trait-default(TRAIT-INDEX) TO description
        ELSE
          MOVE ui-response TO description
        END-IF
      WHEN "ORIGINAL-TEAM"
        MOVE "ENTER TEAM (20)" TO ui-prompt
        PERFORM UI-Ask
        IF ui-empty-answer
          MOVE trait-default(TRAIT-INDEX) TO original-team
        ELSE
          PERFORM UI-Normalize-Response
          MOVE ui-response TO original-team
        END-IF
      WHEN "SALARY-NEED"
        MOVE "ENTER NEED (##)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(trait-default(TRAIT-INDEX)) TO salary-need
        ELSE
          MOVE ui-number TO salary-need
        END-IF
      WHEN "SALARY-WANT"
        MOVE "ENTER WANT (##)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(trait-default(TRAIT-INDEX)) TO salary-want
        ELSE
          MOVE ui-number TO salary-want
        END-IF
      WHEN "PER-DIEM"
        MOVE "ENTER PAY (##)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(trait-default(TRAIT-INDEX)) TO per-diem
        ELSE
          MOVE ui-number TO per-diem
        END-IF
      WHEN "JOB"
        MOVE "ENTER JOB (10)" TO ui-prompt
        PERFORM UI-Ask
        IF ui-empty-answer
          MOVE trait-default(TRAIT-INDEX) TO job
        ELSE
          PERFORM UI-Normalize-Response
          MOVE ui-response TO job
        END-IF
      WHEN "SKILL"
        MOVE "ENTER SKILL (10)" TO ui-prompt
        PERFORM UI-Ask
        IF ui-empty-answer
          MOVE trait-default(TRAIT-INDEX) TO skill
        ELSE
          PERFORM UI-Normalize-Response
          MOVE ui-response TO skill
        END-IF
      WHEN "HOBBY"
        MOVE "ENTER HOBBY (10)" TO ui-prompt
        PERFORM UI-Ask
        IF ui-empty-answer
          MOVE trait-default(TRAIT-INDEX) TO hobby
        ELSE
          PERFORM UI-Normalize-Response
          MOVE ui-response TO hobby
        END-IF
      WHEN "LEVEL"
        MOVE "ENTER LEVEL (##)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(trait-default(TRAIT-INDEX)) TO level
        ELSE
          MOVE ui-number TO level
        END-IF
      WHEN "EXPERIENCE"
        MOVE "ENTER EXPERIENCE (##)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(trait-default(TRAIT-INDEX)) TO experience
        ELSE
          MOVE ui-number TO experience
        END-IF
      WHEN "POWER-STAT"
        MOVE "ENTER POWER (##)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(trait-default(TRAIT-INDEX)) TO power-stat
        ELSE
          PERFORM Validate-Die
          MOVE ui-number TO power-stat
        END-IF
      WHEN "POWER-BONUS"
        MOVE "ENTER POWER BONUS (#)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(trait-default(TRAIT-INDEX)) TO power-bonus
        ELSE
          MOVE ui-number TO power-bonus
        END-IF
      WHEN "FOCUS-STAT"
        MOVE "ENTER FOCUS (##)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(trait-default(TRAIT-INDEX)) TO focus-stat
        ELSE
          PERFORM Validate-Die
          MOVE ui-number TO focus-stat
        END-IF
      WHEN "FOCUS-BONUS"
        MOVE "ENTER FOCUS BONUS (#)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(trait-default(TRAIT-INDEX)) TO focus-bonus
        ELSE
          MOVE ui-number TO focus-bonus
        END-IF
      WHEN "SPEED-STAT"
        MOVE "ENTER SPEED (##)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(trait-default(TRAIT-INDEX)) TO speed-stat
        ELSE
          PERFORM Validate-Die
          MOVE ui-number TO speed-stat
        END-IF
      WHEN "SPEED-BONUS"
        MOVE "ENTER SPEED BONUS (#)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(trait-default(TRAIT-INDEX)) TO speed-bonus
        ELSE
          MOVE ui-number TO speed-bonus
        END-IF
      WHEN "BODY"
        MOVE "ENTER BODY (###)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(trait-default(TRAIT-INDEX)) TO body
        ELSE
          MOVE ui-number TO body
        END-IF
      WHEN "MIND"
        MOVE "ENTER MIND (###)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(trait-default(TRAIT-INDEX)) TO mind
        ELSE
          MOVE ui-number TO mind
        END-IF
      WHEN "SPIRIT"
        MOVE "ENTER SPIRIT (###)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(trait-default(TRAIT-INDEX)) TO spirit
        ELSE
          MOVE ui-number TO spirit
        END-IF
      WHEN "CHARACTER-TYPE"
        MOVE "ENTER TYPE (PC | NPC | TEST)" TO ui-prompt
        PERFORM UI-Ask
        IF ui-empty-answer
          MOVE trait-default(TRAIT-INDEX) TO character-type
        ELSE
          MOVE ui-response TO character-type
        END-IF
    END-EVALUATE.

  Confirm-Record-Character.
    PERFORM Developer-View-Character

    MOVE "RECORD THIS CHARACTER" TO ui-prompt
    PERFORM UI-Confirm.

  Record-New-Character.
    DISPLAY "WRITING CHARACTER RECORD..."

    OPEN I-O Character-Roster
      WRITE Character-Record
      INVALID KEY DISPLAY "ERROR: COULD NOT WRITE CHARACTER RECORD"
    CLOSE Character-Roster.

  Update-Character.
    DISPLAY "UPDATING CHARACTER RECORD..."

    OPEN I-O Character-Roster
      READ Character-Roster KEY IS short-name
        INVALID KEY DISPLAY "ERROR: COULD NOT UPDATE CHARACTER RECORD"
        NOT INVALID KEY REWRITE Character-Record
    CLOSE Character-Roster.

  Select-Field.
    MOVE "ENTER A FIELD" TO ui-prompt
    PERFORM UI-Ask-Normalized
    MOVE ui-response TO ui-answer

    IF ui-exited
      SET ui-denied TO TRUE
      EXIT PARAGRAPH
    ELSE
      MOVE ui-response TO trait-buffer

      PERFORM Reset-Index
      SEARCH Trait-Entry
        AT END 
          DISPLAY "INVALID FIELD: " trait-buffer
        WHEN trait-label(TRAIT-INDEX) = trait-buffer
          PERFORM Assign-Field
      END-SEARCH
    END-IF.

  Lookup-Key.
    MOVE "ENTER KEY (SHORT-NAME) (10)" TO ui-prompt.
    PERFORM UI-Ask-Normalized

    SET invalid-character TO TRUE

    PERFORM Set-UI-Key-Status

    IF ui-invalid-key
      DISPLAY "INVALID KEY"
      SET invalid-character TO TRUE
    END-IF    

    PERFORM Validate-Character-Status.

VALIDATION SECTION.
  Validate-Die.
    IF NOT valid-die
      DISPLAY "INVALID DIE NUMBER. DEFAULTING TO 8"
      MOVE 8 to ui-number
    END-IF.

  Set-UI-Key-Status.
    IF ui-empty-answer OR ui-invalid-text
      SET ui-invalid-key TO TRUE
    ELSE
      SET ui-valid-key TO TRUE
    END-IF.

  Validate-Character-Status.
    OPEN I-O Character-Roster
      MOVE ui-response TO short-name
      READ Character-Roster KEY IS short-name
        INVALID KEY 
          SET new-character TO TRUE
        NOT INVALID KEY 
          SET existing-character TO TRUE
      END-READ
    CLOSE Character-Roster.

MAIN-MENU SECTION.
  Initialize-Menu-Table.
    SET MENU-INDEX TO 1
    MOVE "C" TO menu-key(1) MOVE ACT-CREATE TO menu-action(1)
    MOVE "E" TO menu-key(2) MOVE ACT-EDIT   TO menu-action(2)
    MOVE "D" TO menu-key(3) MOVE ACT-DELETE TO menu-action(3)
    MOVE "A" TO menu-key(4) MOVE ACT-ALL    TO menu-action(4)
    MOVE "L" TO menu-key(5) MOVE ACT-LIST   TO menu-action(5)
    MOVE "R" TO menu-key(6) MOVE ACT-RESET  TO menu-action(6)
    MOVE "Q" TO menu-key(7) MOVE ACT-QUIT   TO menu-action(7).

COPYBOOK SECTION.
  COPY "src/copy/procedure/user-interface.cpy".
  COPY "src/copy/procedure/character.cpy".
  COPY "src/copy/procedure/main-cast.cpy".

*> Build: `cobc -x -o build/character-editor src/cobol/character-editor.cob`
