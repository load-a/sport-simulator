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
  DATA RECORDS ARE Calendar-Day.

  01 Calendar-Day.
    02 numeric-date.
      03 month-number PIC 99 VALUE 10.
      03 day-number   PIC 9 VALUE 1.
    02 named-date.
      03 month-name   PIC X(9).
      03 day-name     PIC X(8).
    02 challenger     PIC X(20) VALUE "PICK-UP GAME".
    02 fortune        PIC X(20) VALUE "NONE".

WORKING-STORAGE SECTION.
  78 ACT-CLEAR    VALUE "CLEAR-EVENT".
  78 ACT-EDIT     VALUE "EDIT-EVENT".
  78 ACT-RESET    VALUE "RESET-FILE".
  78 ACT-QUIT     VALUE "QUIT".
  78 MENU-LENGTH  VALUE 4.

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
    88 quit-mode    VALUES 1 THROUGH 9.

COPY "src/copy/working-storage/user-interface-data.cpy".

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
  MOVE "Q" TO menu-key(4) MOVE ACT-QUIT   TO menu-action(4).

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
    WHEN menu-key(MENU-INDEX) = ui-answer
      EVALUATE menu-action(MENU-INDEX)
        WHEN ACT-EDIT PERFORM Edit-Event
        WHEN ACT-CLEAR PERFORM Clear-Event
        WHEN ACT-RESET PERFORM Reset-Calendar
        WHEN ACT-QUIT SET quit-mode TO TRUE
      END-EVALUATE
  END-SEARCH.

ACTION SECTION.
Edit-Event.
  DISPLAY "NOT IMPLEMENTED".
Clear-Event.
  DISPLAY "NOT IMPLEMENTED".
Reset-Calendar.
  DISPLAY "NOT IMPLEMENTED".

COPYBOOK SECTION.
COPY "src/copy/procedure/user-interface.cpy".

*>Build `cobc -x -o build/calendar-editor src/cobol/calendar-editor.cob`
