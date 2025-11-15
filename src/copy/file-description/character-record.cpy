01 character-record.
  02 short-name       PIC X(10).
  02 long-name        PIC X(21).
  02 age              PIC 99.
  02 birthday.
    03 birth-month    PIC 99.
    03 birth-day      PIC 99.
  02 height.
    03 height-feet    PIC 9.
    03 height-inches  PIC 99.
  02 gender           PIC X(10).
  02 race             PIC X(20).
  02 description      PIC X(80).
  02 original-team    PIC X(20).

  02 salary.
    03 salary-need  PIC 99 VALUE 30.
    03 salary-want  PIC 99 VALUE 60.
    03 per-diem     PIC 99 VALUE 45.
    03 job          PIC X(10) VALUE "NONE".
    03 skill        PIC X(10) VALUE "NONE".
    02 hobby        PIC X(10) VALUE "NONE".

  02 performance.
    03 level          PIC 9 VALUE 1.
    03 experience     PIC 99 VALUE ZEROS.

    03 power-stat     PIC 99 VALUE 8.
    03 power-bonus    PIC 9 VALUE ZEROS.
    03 focus-stat     PIC 99 VALUE 8.
    03 focus-bonus    PIC 9 VALUE ZEROS.
    03 speed-stat     PIC 99 VALUE 8.
    03 speed-bonus    PIC 9 VALUE ZEROS.
    03 full-potential PIC 99 VALUE ZEROS.

    03 body                     PIC 999 VALUE 100.
      88 perfect-body           VALUES 125 THROUGH 101.
      88 excellent-body         VALUES 100 THROUGH 91.
      88 good-body              VALUES 90  THROUGH 71.
      88 okay-body              VALUES 70  THROUGH 51.
      88 sick-body              VALUES 50  THROUGH 31.
      88 injured-body           VALUES 30  THROUGH 0.
    03 mind                     PIC 999 VALUE 100.
      88 exceptional-mind       VALUES 200 THROUGH 101.
      88 sharp-mind             VALUES 100 THROUGH 91.
      88 calm-mind              VALUES 90  THROUGH 71.
      88 tired-mind             VALUES 70  THROUGH 51.
      88 exhausted-mind         VALUES 50  THROUGH 0.
    03 spirit                   PIC 999 VALUE 100.
      88 happy-spirit           VALUES 150 THROUGH 81.
      88 neutral-spirit         VALUES 80  THROUGH 61.
      88 annoyed-spirit         VALUES 60  THROUGH 41.
      88 unhappy-spirit         VALUES 40  THROUGH 0.
    03 effectiveness-multiplier PIC 9V99 VALUE 1.00.

  02 character-type PIC X(6).
     88 teammate VALUE "PLAYER".
     88 opponent VALUE "NPC".
