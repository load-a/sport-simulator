IDENTIFICATION DIVISION.
PROGRAM-ID. Friendship-Editor.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
  SELECT Friend-List ASSIGN TO "data/friend-list.dat"
    ORGANIZATION IS INDEXED
    ACCESS MODE IS DYNAMIC
    RECORD KEY pairing
    FILE STATUS IS friend-file.

DATA DIVISION.
FILE SECTION.
FD Friend-List
  LABEL RECORDS ARE STANDARD
  RECORD CONTAINS 43 CHARACTERS
  DATA RECORD IS Friend-Record.

  01 Friend-Record.
    02 pairing.
      03 friend-key-1   PIC X(10) VALUE SPACES.
      03 friend-key-2   PIC X(10) VALUE SPACES.
    02 Relationship     PIC X(20) VALUE SPACES.
    02 Friendship-level PIC 999 VALUE ZEROS.


WORKING-STORAGE SECTION.
  COPY "src/main/copy/working-storage/user-interface-data.cpy".
  COPY "src/main/copy/working-storage/main-cast-data.cpy".

  78 ACT-EDIT-FRIENDSHIP  VALUE "EDIT-FRIENDSHIP".
  78 ACT-LIST-ALL         VALUE "LIST-ALL".
  78 ACT-LIST-CHARACTER   VALUE "LIST-CHARACTER".
  78 ACT-RESET-FILE       VALUE "RESET-FILE".
  78 ACT-QUIT             VALUE "QUIT".
  78 MENU-LENGTH          VALUE 5.

  01 friend-file        PIC 99.
    88 end-of-file      VALUE 10.

  01 temp-key           PIC X(10).

  01 SELECTION-INDEX    PIC 99 VALUE 1.

  01 Special-Characters PIC 9.
    88 regular          VALUE 0.
    88 yumi             VALUE 1.

  01 Pair-Table.
    02 pair-counter   PIC 9(4) VALUE 1.
    02 pair-status    PIC 9.
      88 pair-exists  VALUE 1.
      88 new-pair     VALUE ZERO.
    02 Pair-Entry     OCCURS 350 TIMES INDEXED BY PAIR-INDEX.
      03 pair-key     PIC X(20) VALUE SPACES.

  01 Program-Mode   PIC 9.
    88 menu-mode    VALUE ZERO.
    88 quit-mode    VALUES 1 THROUGH 9.

  01 Menu-Table.
    02 Menu-Entry OCCURS MENU-LENGTH TIMES INDEXED BY MENU-INDEX.
      03 menu-key PIC X.
      03 menu-action PIC X(20).


PROCEDURE DIVISION.
Initialize-Pair-Table.
  SET PAIR-INDEX TO 1
  PERFORM Initialize-MC-Table.

Initialize-Menu-Table.
  SET MENU-INDEX TO 1
  MOVE "E" TO menu-key(1) MOVE ACT-EDIT-FRIENDSHIP  TO menu-action(1)
  MOVE "L" TO menu-key(2) MOVE ACT-LIST-ALL         TO menu-action(2)
  MOVE "C" TO menu-key(3) MOVE ACT-LIST-CHARACTER   TO menu-action(3)
  MOVE "R" TO menu-key(4) MOVE ACT-RESET-FILE       TO menu-action(4)
  MOVE "Q" TO menu-key(5) MOVE ACT-QUIT             TO menu-action(5).

Main-Logic.
  SET menu-mode TO TRUE

  PERFORM UNTIL quit-mode
    SET menu-mode TO TRUE
    PERFORM Menu-Stage
    PERFORM Execute-Stage
  END-PERFORM.
  STOP RUN.

Menu-Stage.
  PERFORM VARYING MENU-INDEX FROM 1 BY 1 UNTIL MENU-INDEX > MENU-LENGTH
    DISPLAY menu-key(MENU-INDEX) ": " menu-action(MENU-INDEX)
  END-PERFORM

  MOVE "CHOSE ACTION" TO ui-prompt.
  PERFORM UI-Ask-Normalized.

Execute-Stage.
  SET MENU-INDEX TO 1
  SEARCH Menu-Entry
    AT END DISPLAY "INVALID CHOICE"
    WHEN menu-key(MENU-INDEX) = ui-head
      PERFORM UI-Clear-Data
      EVALUATE menu-action(MENU-INDEX)
        WHEN ACT-EDIT-FRIENDSHIP  PERFORM Edit-Friendship UNTIL ui-denied
        WHEN ACT-LIST-ALL         PERFORM List-All
        WHEN ACT-LIST-CHARACTER   PERFORM List-Character
        WHEN ACT-RESET-FILE       PERFORM Reset-File
        WHEN ACT-QUIT             SET quit-mode TO TRUE
        WHEN OTHER                DISPLAY "INVALID ACTION"
      END-EVALUATE
  END-SEARCH.

CREATION SECTION.
  Reset-File.
    OPEN OUTPUT Friend-List 
      PERFORM VARYING SELECTION-INDEX FROM 1 BY 1 UNTIL SELECTION-INDEX > 26
        PERFORM Write-Freindships VARYING MC-INDEX FROM 1 BY 1 UNTIL MC-INDEX > 26
      END-PERFORM
    CLOSE Friend-List.

  Write-Freindships.
    MOVE main-cast-key(SELECTION-INDEX) TO friend-key-1
    MOVE main-cast-key(MC-INDEX) TO friend-key-2

    IF friend-key-1 = friend-key-2
      EXIT PARAGRAPH
    END-IF

    IF friend-key-1 = "YUMI" OR friend-key-2 = "YUMI"
      SET yumi TO TRUE
    ELSE
      SET regular TO TRUE
    END-IF
    PERFORM Build-Pairing

    SET PAIR-INDEX TO 1
    SEARCH Pair-Entry
      AT END
        MOVE pairing TO pair-key(pair-counter)
        ADD 1 TO pair-counter
        SET new-pair TO TRUE
      WHEN pair-key(PAIR-INDEX) = pairing
        SET pair-exists TO TRUE
    END-SEARCH

    IF new-pair
      IF yumi
        MOVE 200 TO friendship-level
        MOVE "FRIENDS" TO relationship
      ELSE
        MOVE 100 TO friendship-level
        MOVE "ACQUAINTED" TO relationship
      END-IF
      
      WRITE Friend-Record
    END-IF.

  Build-Pairing.
    PERFORM Sort-Keys.
    MOVE friend-key-1 TO pairing (1:10)
    MOVE friend-key-2 TO pairing (11:10).

  Sort-Keys.
    IF friend-key-1 > friend-key-2 
      MOVE friend-key-1 TO temp-key
      MOVE friend-key-2 TO friend-key-1
      MOVE temp-key TO friend-key-2
    END-IF.

EDIT SECTION.
  Edit-Friendship.
    PERFORM UNTIL ui-denied
      MOVE "ENTER CHARACTER 1" TO ui-prompt
      PERFORM UI-Ask
      PERFORM UI-Normalize-Response
      MOVE ui-response TO friend-key-1

      MOVE "ENTER CHARACTER 2" TO ui-prompt
      PERFORM UI-Ask
      PERFORM UI-Normalize-Response
      MOVE ui-response TO friend-key-2

      PERFORM Build-Pairing

      OPEN I-O Friend-List
        READ Friend-List KEY IS pairing
          INVALID KEY 
            DISPLAY "INVALID PAIR"
          NOT INVALID KEY
            DISPLAY SPACES
            PERFORM List-Pair
            DISPLAY SPACES

            DISPLAY "GUIDE: ACQUAINT.(000), FRIENDS(200), BEST FRIENDS(400), FAMILY(600), MORE(800)"
            MOVE "ENTER RELATIONSHIP (20)" TO ui-prompt
            PERFORM UI-Ask
            PERFORM UI-Normalize-Response

            IF ui-valid-text
              MOVE ui-response TO relationship
              DISPLAY "UPDATING RELATIONSHIP"
              REWRITE Friend-Record
            ELSE
              DISPLAY "NO CHANGE"
            END-IF

            MOVE "ENTER LEVEL(###)" TO ui-prompt
            PERFORM UI-Ask-Number

            IF ui-valid-number
              MOVE ui-number TO friendship-level
              DISPLAY "UPDATING LEVEL"
              REWRITE Friend-Record
            ELSE
              DISPLAY "NO CHANGE"
            END-IF

            DISPLAY "UPDATED FRIENDSHIP:"

            DISPLAY SPACES
            PERFORM List-Pair
            DISPLAY SPACES
        END-READ
      CLOSE Friend-List

      MOVE "EDIT ANOTHER FRIENDSHIP" TO ui-prompt
      PERFORM UI-Confirm
    END-PERFORM.

LISTING SECTION.
  List-All.
    OPEN INPUT Friend-List
      MOVE LOW-VALUE TO pairing
      START Friend-List KEY >= pairing
        INVALID KEY DISPLAY "NO FRIENDSHIPS FOUND"
        NOT INVALID KEY
          PERFORM UNTIL end-of-file
            READ Friend-List NEXT RECORD
              AT END
                SET end-of-file TO TRUE
              NOT AT END
                PERFORM List-Pair
            END-READ
          END-PERFORM
      END-START
    CLOSE Friend-List.

  List-Character.
    MOVE "WHICH CHARACTER" TO ui-prompt
    PERFORM UI-Ask
    PERFORM UI-Normalize-Response
    MOVE ui-response TO temp-key

    OPEN INPUT Friend-List
      MOVE LOW-VALUE TO pairing
      START Friend-List KEY >= pairing
        INVALID KEY DISPLAY "NO FRIENDSHIPS FOUND"
        NOT INVALID KEY
          PERFORM UNTIL end-of-file
            READ Friend-List NEXT RECORD
              AT END
                SET end-of-file TO TRUE
              NOT AT END
                IF temp-key = friend-key-1 OR temp-key = friend-key-2
                  PERFORM List-Pair
                END-IF
            END-READ
          END-PERFORM
      END-START
    CLOSE Friend-List.

  List-Pair.
    DISPLAY friend-key-1 "& " friend-key-2 "- " relationship " " friendship-level.

COPY "src/main/copy/procedure/user-interface.cpy".
COPY "src/main/copy/procedure/main-cast.cpy".

*> Build: `cobc -x -o build/friendship-editor src/main/cobol/friendship-editor.cob`
