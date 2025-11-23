IDENTIFICATION DIVISION.
PROGRAM-ID. CALENDAR-EDITOR.
AUTHOR. SARAMIR.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
  SELECT Calendar ASSIGN TO "data/calendar.dat"
  ORGANIZATION IS INDEXED
  ACCESS MODE IS DYNAMIC
  RECORD KEY numeric-date
  FILE STATUS IS calendar-file.

DATA DIVISION.
FILE SECTION.
FD Calendar
  LABEL RECORDS ARE STANDARD
  DATA RECORDS ARE Calendar-Record.

  01 Calendar-Record.
    02 numeric-date.
      03 month-number     PIC 99.
        88 valid-month    VALUES 1 THROUGH 13.
      03 day-number       PIC 99.
        88 valid-day      VALUES 1 THROUGH 28.
    02 named-date.
      03 month-name       PIC X(9).
      03 day-name         PIC X(8).
    02 challenger         PIC X(20) VALUE "RANDOM".
    02 scripted-event     PIC X(20) VALUE "NONE".
    02 travel-option.
      03 travel-option-1  PIC X(20).
      03 travel-option-2  PIC X(20).

WORKING-STORAGE SECTION.
  78 ACT-CLEAR    VALUE "CLEAR-EVENT".
  78 ACT-EDIT     VALUE "EDIT-EVENT".
  78 ACT-RESET    VALUE "RESET-FILE".
  78 ACT-QUIT     VALUE "QUIT".
  78 ACT-LIST     VALUE "LIST-EVENTS".
  78 MENU-LENGTH  VALUE 5.

  01 calendar-file PIC 99 VALUE ZEROS.
    88 end-of-file VALUE 10.

  01 Weekday-Table.
    02 Weekday-Entry OCCURS 7 TIMES INDEXED BY WEEKDAY-INDEX.
      03 weekday PIC X(8).

  01 Month-Table.
    02 Month-Entry OCCURS 13 TIMES INDEXED BY MONTH-INDEX.
      03 month PIC X(9).

  01 Menu-Table.
    02 Menu-Entry OCCURS MENU-LENGTH TIMES INDEXED BY MENU-INDEX.
      03 menu-key PIC X.
      03 menu-action PIC X(15).

  01 Program-Mode PIC 9 VALUE 0.
    88 menu-mode    VALUE ZERO.
    88 invalid-mode VALUE 1.
    88 quit-mode    VALUES 2 THROUGH 9.

  01 day-name-index PIC 9 VALUE 1.
  01 ignore-quotient PIC 9.
  01 creation-id PIC 9999 VALUE ZEROS.

COPY "src/main/copy/working-storage/user-interface-data.cpy".

PROCEDURE DIVISION.
Initialize-Tables.
  SET WEEKDAY-INDEX TO 1
  MOVE "SUNDAY"   TO WEEKDAY(1)
  MOVE "MOONDAY"  TO WEEKDAY(2)
  MOVE "EMBERDAY" TO WEEKDAY(3)
  MOVE "WASHDAY"  TO WEEKDAY(4)
  MOVE "GAILDAY"  TO WEEKDAY(5)
  MOVE "STONEDAY" TO WEEKDAY(6)
  MOVE "STARDAY"  TO WEEKDAY(7)

  SET MONTH-INDEX TO 1
  MOVE "ZEMBER"     TO MONTH(1)
  MOVE "ONYX"       TO MONTH(2)
  MOVE "SILVER"     TO MONTH(3)
  MOVE "NORTHSTAR"  TO MONTH(4)
  MOVE "SCARLET"    TO MONTH(5)
  MOVE "ASH"        TO MONTH(6)
  MOVE "HEXEMBER"   TO MONTH(7)
  MOVE "SHADOW"     TO MONTH(8)
  MOVE "AMBER"      TO MONTH(9)
  MOVE "EASTSTAR"   TO MONTH(10)
  MOVE "AZURE"      TO MONTH(11)
  MOVE "JADE"       TO MONTH(12)
  MOVE "ISEMBER"    TO MONTH(13)

  SET MENU-INDEX TO 1
  MOVE "E" TO menu-key(1) MOVE ACT-EDIT   TO menu-action(1)
  MOVE "C" TO menu-key(2) MOVE ACT-CLEAR  TO menu-action(2)
  MOVE "R" TO menu-key(3) MOVE ACT-RESET  TO menu-action(3)
  MOVE "Q" TO menu-key(4) MOVE ACT-QUIT   TO menu-action(4)
  MOVE "L" TO menu-key(5) MOVE ACT-LIST   TO menu-action(5).

Initialize-Calendar-Record.
  PERFORM Calculate-Weekday-Index.

Main-Logic.
  PERFORM UNTIL ui-quitted
    SET menu-mode TO TRUE
    PERFORM Calendar-Menu
    PERFORM Calendar-Function
  END-PERFORM
STOP RUN.

Calendar-Menu.
  PERFORM VARYING MENU-INDEX FROM 1 BY 1 UNTIL MENU-INDEX > MENU-LENGTH
    DISPLAY "[" menu-key(MENU-INDEX) "] " menu-action(MENU-INDEX)
  END-PERFORM

  MOVE "CHOOSE OPTION" TO ui-prompt
  PERFORM UI-Ask-Normalized.

Calendar-Function.
  SET MENU-INDEX TO 1

  SEARCH Menu-Entry
    AT END DISPLAY "INVALID ACTION"
    WHEN menu-key(MENU-INDEX) = ui-response
      EVALUATE menu-action(MENU-INDEX)
        WHEN ACT-EDIT   PERFORM Edit-Event
        WHEN ACT-CLEAR  PERFORM Clear-Event
        WHEN ACT-RESET  PERFORM Reset-Calendar
        WHEN ACT-QUIT   SET quit-mode TO TRUE
        WHEN ACT-LIST   PERFORM List-Events
      END-EVALUATE
  END-SEARCH.

ACTION SECTION.
  List-Events.
    OPEN INPUT Calendar
      MOVE LOW-VALUE TO numeric-date
      START Calendar KEY >= numeric-date
        INVALID KEY DISPLAY "NO EVENTS FOUND"
        NOT INVALID KEY
          PERFORM UNTIL end-of-file
            READ Calendar NEXT RECORD
              AT END SET end-of-file TO TRUE
              NOT AT END
                PERFORM Display-Calendar-Record
            END-READ
          END-PERFORM
      END-START
    CLOSE Calendar.

  Display-Calendar-Record.
    DISPLAY month-number "/" day-number
    DISPLAY day-name " " month-name " " day-number
    DISPLAY "CHALLENGER: " challenger
    DISPLAY "SCRIPTED EVENT: " scripted-event

    IF day-number = 1
      DISPLAY "OPTION A: " travel-option-1
      DISPLAY "OPTION B: " travel-option-2
    END-IF
    DISPLAY "---".

  Edit-Event.
    PERFORM UNTIL ui-exited
      PERFORM Get-Date

      IF invalid-mode
        DISPLAY "INVALID DATE."
        EXIT PARAGRAPH
      END-IF

      OPEN INPUT Calendar
        READ Calendar KEY IS numeric-date
        INVALID KEY DISPLAY "INVARIANT VIOLATION: INVALID KEY FOR EDIT EVENT."
        NOT INVALID KEY DISPLAY SPACES
      CLOSE Calendar

      PERFORM Display-Calendar-Record

      MOVE "ENTER CHALLENGER" TO ui-prompt
      PERFORM UI-Ask-Normalized
      IF NOT ui-empty-answer
        MOVE ui-response TO challenger
      END-IF

      IF day-number = 1
        MOVE "ENTER TRAVEL OPTION 1" TO ui-prompt
        PERFORM UI-Ask-Normalized
        IF NOT ui-empty-answer
          MOVE ui-response TO travel-option-1
        END-IF

        MOVE "ENTER TRAVEL OPTION 2" TO ui-prompt
        PERFORM UI-Ask-Normalized
        IF NOT ui-empty-answer
          MOVE ui-response TO travel-option-2
        END-IF
      ELSE
        MOVE "ENTER SCRIPTED EVENT" TO ui-prompt
        PERFORM UI-Ask-Normalized
        IF NOT ui-empty-answer
          MOVE ui-response TO scripted-event
        END-IF
      END-IF

      DISPLAY SPACES
      PERFORM Display-Calendar-Record

      MOVE "WRITE THIS RECORD" TO ui-prompt
      PERFORM UI-Confirm

      IF ui-confirmed
        OPEN I-O Calendar
          REWRITE Calendar-Record
        CLOSE Calendar
      END-IF

      MOVE "EDIT ANOTHER DATE" TO ui-prompt
      PERFORM Ui-Confirm

      IF NOT ui-confirmed
        SET ui-denied TO TRUE
      END-IF
    END-PERFORM.

  Clear-Event.
    PERFORM Get-Date

    IF invalid-mode
      DISPLAY "INVALID DATE. CANCELLING CLEAR-EVENT."
      EXIT PARAGRAPH
    END-IF

    PERFORM Generate-Calendar-Record

    DISPLAY "CLEARING EVENT ON: " month-number "/" day-number
    OPEN I-O Calendar
      REWRITE Calendar-Record
    CLOSE Calendar.

  Reset-Calendar.
    MOVE "ARE YOU SURE YOU WANT TO RESET THE CALENDAR" TO ui-prompt
    PERFORM Ui-Confirm

    IF ui-denied
      EXIT PARAGRAPH
    END-IF

    DISPLAY "RESETTING CALENDAR..."

    OPEN OUTPUT Calendar
      MOVE 0101 to numeric-date

      PERFORM UNTIL numeric-date = 1328
        PERFORM Generate-Calendar-Record
        WRITE Calendar-Record
        PERFORM Increment-Day
      END-PERFORM

      PERFORM Generate-Calendar-Record
      WRITE Calendar-Record
    CLOSE Calendar.

  Generate-Calendar-Record.
    MOVE MONTH(month-number) TO month-name
    MOVE WEEKDAY(day-name-index) TO day-name

    IF day-name-index = 7
      MOVE "WEEKLY SKIRMISH" TO challenger
    ELSE 
      MOVE "DAILY GAME" TO challenger 
    END-IF

    IF day-number = 28
      MOVE "MONTHLY MATCH" TO challenger
    END-IF 

    IF day-number = 1
      MOVE "TRAVEL" TO scripted-event
    ELSE
      MOVE "NONE" TO scripted-event
    END-IF.

DATE-MANAGEMENT SECTION.
  Calculate-Weekday-Index.
    DIVIDE day-number BY 7 GIVING ignore-quotient REMAINDER day-name-index
    IF day-name-index = 0 
      ADD 7 TO day-name-index
    END-IF.

  Increment-Day.
    ADD 1 TO day-number

    IF NOT valid-day
      MOVE 1 TO day-number
      PERFORM Increment-Month
    END-IF

    PERFORM Calculate-Weekday-Index.

  Increment-Month.
    ADD 1 TO month-number

    IF NOT valid-month
      MOVE 1 TO month-number
    END-IF.

CALENDAR-INTERFACE-SECTION.
  Get-Date.
    MOVE "ENTER MONTH" TO ui-prompt
    PERFORM UI-Ask-Number
    MOVE ui-number TO month-number

    MOVE "ENTER DAY" TO ui-prompt
    PERFORM UI-Ask-Number
    MOVE ui-number TO day-number

    PERFORM Validate-Input-Date.

  Validate-Input-Date.
    IF NOT valid-day
      DISPLAY "INVALID DAY"
      SET invalid-mode TO TRUE
    END-IF

    IF NOT valid-month
      DISPLAY "INVALID MONTH"
      SET invalid-mode TO TRUE
    END-IF.

COPYBOOK SECTION.
  COPY "src/main/copy/procedure/user-interface.cpy".

*>Build `cobc -x -o build/calendar-editor src/main/cobol/calendar-editor.cob`
