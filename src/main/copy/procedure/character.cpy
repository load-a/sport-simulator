CHARACTER-FUCTION SECTION.

CHARACTER-PREVIEW SECTION.
  Preview-Character.
    DISPLAY SPACES
    DISPLAY long-name " (" FUNCTION TRIM(short-name) ")"
    DISPLAY "Age: " age " - " birth-month "/" birth-day
    DISPLAY height-feet "'" height-inches QUOTE " - " gender
    DISPLAY "Race: " race
    DISPLAY "Original Team: " original-team
    DISPLAY SPACES
    DISPLAY description
    DISPLAY SPACES
    DISPLAY "Task: " job
    DISPLAY "Per Diem: $" per-diem
    DISPLAY SPACES
    DISPLAY "Lv. " level " (" experience " / 100)"
    DISPLAY SPACES
    DISPLAY "POTENTIAL                               EFFECTIVENESS                           "
    DISPLAY "Power: " power-stat " +" power-bonus "                            Body:   " body
    DISPLAY "Focus: " focus-stat " +" focus-bonus "                            Mind:   " mind
    DISPLAY "Speed: " speed-stat " +" speed-bonus "                            Spirit: " spirit
    DISPLAY SPACES.

  Developer-View-Character.
    DISPLAY SPACES
    DISPLAY long-name " (" FUNCTION TRIM(short-name) ") " "<" FUNCTION TRIM(personality) ">"
    DISPLAY "Age: " age " - " birth-month "/" birth-day
    DISPLAY height-feet "'" height-inches QUOTE " - " gender
    DISPLAY "Race: " race
    DISPLAY "Original Team: " original-team
    DISPLAY SPACES
    DISPLAY description
    DISPLAY SPACES
    DISPLAY "Task: " job
    DISPLAY "Per Diem: $" per-diem " ($" salary-need "/$" salary-want ")"
    DISPLAY "Skill: " skill
    DISPLAY "Hobby: " hobby
    DISPLAY SPACES
    DISPLAY "Lv. " level " (" experience " / 100)"
    DISPLAY SPACES
    DISPLAY "POTENTIAL                               EFFECTIVENESS                           "
    DISPLAY "Power: " power-stat " +" power-bonus "                            Body:   " body
    DISPLAY "Focus: " focus-stat " +" focus-bonus "                            Mind:   " mind
    DISPLAY "Speed: " speed-stat " +" speed-bonus "                            Spirit: " spirit
    DISPLAY SPACES.

T-TABLE SECTION. 
  Initialize-Trait-Table.
    SET TRAIT-INDEX TO 1
    MOVE "NAME"           TO trait-label    (1)
    MOVE "NO NAME"        TO trait-default  (1)
    MOVE "LONG-NAME"      TO trait-code     (1)

    MOVE "AGE"            TO trait-label    (2)
    MOVE "30"             TO trait-default  (2)
    MOVE "AGE"            TO trait-code     (2)

    MOVE "BIRTH MONTH"    TO trait-label    (3)
    MOVE "01"             TO trait-default  (3)
    MOVE "BIRTH-MONTH"    TO trait-code     (3)

    MOVE "BIRTH DAY"      TO trait-label    (4)
    MOVE "01"             TO trait-default  (4)
    MOVE "BIRTH-DAY"      TO trait-code     (4)

    MOVE "HEIGHT"         TO trait-label    (5)
    MOVE "508"            TO trait-default  (5)
    MOVE "HEIGHT"         TO trait-code     (5)

    MOVE "GENDER"         TO trait-label    (6)
    MOVE "NONE"           TO trait-default  (6)
    MOVE "GENDER"         TO trait-code     (6)

    MOVE "RACE"           TO trait-label    (7)
    MOVE "EOSIAN"         TO trait-default  (7)
    MOVE "RACE"           TO trait-code     (7)

    MOVE "INFO"           TO trait-label    (8)
    MOVE "NO DESCRIPTION" TO trait-default  (8)
    MOVE "DESCRIPTION"    TO trait-code     (8)

    MOVE "TEAM"           TO trait-label    (9)
    MOVE "NO TEAM"        TO trait-default  (9)
    MOVE "ORIGINAL-TEAM"  TO trait-code     (9)

    MOVE "NEED"           TO trait-label    (10)
    MOVE "30"             TO trait-default  (10)
    MOVE "SALARY-NEED"    TO trait-code     (10)

    MOVE "WANT"           TO trait-label    (11)
    MOVE "60"             TO trait-default  (11)
    MOVE "SALARY-WANT"    TO trait-code     (11)

    MOVE "PAY"            TO trait-label    (12)
    MOVE "45"             TO trait-default  (12)
    MOVE "PER-DIEM"       TO trait-code     (12)

    MOVE "JOB"            TO trait-label    (13)
    MOVE "REST"           TO trait-default  (13)
    MOVE "JOB"            TO trait-code     (13)

    MOVE "SKILL"          TO trait-label    (14)
    MOVE "NONE"           TO trait-default  (14)
    MOVE "SKILL"          TO trait-code     (14)

    MOVE "HOBBY"          TO trait-label    (15)
    MOVE "VACATION"       TO trait-default  (15)
    MOVE "HOBBY"          TO trait-code     (15)

    MOVE "LV"             TO trait-label    (16)
    MOVE "1"              TO trait-default  (16)
    MOVE "LEVEL"          TO trait-code     (16)

    MOVE "EXP"            TO trait-label    (17)
    MOVE "0"              TO trait-default  (17)
    MOVE "EXPERIENCE"     TO trait-code     (17)

    MOVE "POWER"          TO trait-label    (18)
    MOVE "8"              TO trait-default  (18)
    MOVE "POWER-STAT"     TO trait-code     (18)

    MOVE "FOCUS"          TO trait-label    (19)
    MOVE "8"              TO trait-default  (19)
    MOVE "FOCUS-STAT"     TO trait-code     (19)

    MOVE "SPEED"          TO trait-label    (20)
    MOVE "8"              TO trait-default  (20)
    MOVE "SPEED-STAT"     TO trait-code     (20)

    MOVE "POWER+"         TO trait-label    (21)
    MOVE "0"              TO trait-default  (21)
    MOVE "POWER-BONUS"    TO trait-code     (21)

    MOVE "FOCUS+"         TO trait-label    (22)
    MOVE "0"              TO trait-default  (22)
    MOVE "FOCUS-BONUS"    TO trait-code     (22)

    MOVE "SPEED+"         TO trait-label    (23)
    MOVE "0"              TO trait-default  (23)
    MOVE "SPEED-BONUS"    TO trait-code     (23)

    MOVE "BODY"           TO trait-label    (24)
    MOVE "100"            TO trait-default  (24)
    MOVE "BODY"           TO trait-code     (24)

    MOVE "MIND"           TO trait-label    (25)
    MOVE "100"            TO trait-default  (25)
    MOVE "MIND"           TO trait-code     (25)

    MOVE "SPIRIT"         TO trait-label    (26)
    MOVE "100"            TO trait-default  (26)
    MOVE "SPIRIT"         TO trait-code     (26)

    MOVE "TYPE"           TO trait-label    (27)
    MOVE "NORMAL"         TO trait-default  (27)
    MOVE "PERSONALITY"    TO trait-code     (27).

  Reset-Index.
    SET TRAIT-INDEX TO 1.

  Increment-Index.
    SET TRAIT-INDEX UP BY 1.

  Table-help.
    PERFORM Reset-Index

    PERFORM UNTIL TRAIT-INDEX > TRAIT-TABLE-LENGTH
      DISPLAY trait-label(TRAIT-INDEX) "->" trait-code(TRAIT-INDEX) "(" trait-default(TRAIT-INDEX) ")"
      PERFORM Increment-Index
    END-PERFORM.
