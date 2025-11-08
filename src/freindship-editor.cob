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
  01 friend-file PIC 99.
    88 end-of-file VALUE 10.
  01 temp-key    PIC X(10).
  01 SELECTION-INDEX  PIC 99 VALUE 1.
  01 special-characters PIC 9.
    88 regular VALUE 0.
    88 yumi VALUE 1.

  01 Character-Table.
    02 Character-Entry OCCURS 26 TIMES INDEXED BY CHARACTER-INDEX.
      03 character-key PIC X(10) VALUE SPACES.

  01 Pair-Table.
    02 pair-counter PIC 9(4) VALUE 1.
    02 pair-status PIC 9.
      88 pair-exists VALUE 1.
      88 new-pair VALUE ZERO.
    02 Pair-Entry OCCURS 350 TIMES INDEXED BY PAIR-INDEX.
      03 pair-key PIC X(20) VALUE SPACES.

  COPY "copy/data/input-data.cpy".

PROCEDURE DIVISION.

Initialize-Table.
  MOVE "ALYSSA"     TO character-key(1)
  MOVE "BARNEY"     TO character-key(2)
  MOVE "CHARLES"    TO character-key(3)
  MOVE "DOUG"       TO character-key(4)
  MOVE "EMANON"     TO character-key(5)
  MOVE "FAZE"       TO character-key(6)
  MOVE "GEMMA"      TO character-key(7)
  MOVE "HARAMATSU"  TO character-key(8)
  MOVE "IVAN"       TO character-key(9)
  MOVE "J.J."       TO character-key(10)
  MOVE "KORI"       TO character-key(11)
  MOVE "LEIF"       TO character-key(12)
  MOVE "MEL"        TO character-key(13)
  MOVE "NIAMH"      TO character-key(14)
  MOVE "ORICK"      TO character-key(15)
  MOVE "PETRA"      TO character-key(16)
  MOVE "QUILL"      TO character-key(17)
  MOVE "RAY"        TO character-key(18)
  MOVE "SARA"       TO character-key(19)
  MOVE "TOMOE"      TO character-key(20)
  MOVE "UMBER"      TO character-key(21)
  MOVE "VERA"       TO character-key(22)
  MOVE "WINSTON"    TO character-key(23)
  MOVE "XIA"        TO character-key(24)
  MOVE "YUMI"       TO character-key(25)
  MOVE "ZYLO"       TO character-key(26).

Main-Logic.
  MOVE "TYPE COMMAND ([R]ESET | [E]DIT | [L]IST)" TO question.
  PERFORM Ask.
  PERFORM Normalize-Response.

  EVALUATE response
    WHEN "R"
      DISPLAY "RESETTING FRIEND-LIST..."
      PERFORM Reset-File
    WHEN "E"
      PERFORM Edit-Friendship
    WHEN "L"
      PERFORM List-All
    WHEN OTHER
      DISPLAY "GOOD-BYE"
  END-EVALUATE.
STOP RUN.

CREATION SECTION.
  Reset-File.
    OPEN OUTPUT Friend-List 
      PERFORM VARYING SELECTION-INDEX FROM 1 BY 1 UNTIL SELECTION-INDEX > 26
        PERFORM Write-Freindships VARYING CHARACTER-INDEX FROM 1 BY 1 UNTIL CHARACTER-INDEX > 26
      END-PERFORM
    CLOSE Friend-List.

  Write-Freindships.
    MOVE character-key(SELECTION-INDEX) TO friend-key-1
    MOVE character-key(CHARACTER-INDEX) TO friend-key-2

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
    PERFORM UNTIL denied
      MOVE "ENTER CHARACTER 1" TO question
      PERFORM Ask
      PERFORM Normalize-Response
      MOVE response TO friend-key-1

      MOVE "ENTER CHARACTER 2" TO question
      PERFORM Ask
      PERFORM Normalize-Response
      MOVE response TO friend-key-2

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
            MOVE "ENTER RELATIONSHIP (20)" TO question
            PERFORM Ask
            PERFORM Normalize-Response

            IF valid-text
              MOVE response TO relationship
              DISPLAY "UPDATING RELATIONSHIP"
              REWRITE Friend-Record
            ELSE
              DISPLAY "NO CHANGE"
            END-IF

            MOVE "ENTER LEVEL(###)" TO question
            PERFORM Ask-Number

            IF valid-number
              MOVE input-number TO friendship-level
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

      MOVE "EDIT ANOTHER FRIENDSHIP" TO question
      PERFORM Confirm
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

  List-Pair.
    DISPLAY friend-key-1 " + " friend-key-2 " => " relationship " (" friendship-level ")".

COPY "copy/procedure/input-section.cpy".
