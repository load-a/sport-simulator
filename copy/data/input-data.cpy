01 question     PIC X(40).
01 response     PIC X(60).
  88 confirmed  VALUE "Y", "YES".
  88 denied     VALUE "X", "EXIT", "N", "NO", "Q", "QUIT".
01 input-number PIC 999.
  88 valid-die VALUE 2, 4, 6, 8, 10, 12, 20.
01 input-status PIC 9 VALUE ZERO.
  88 empty-input    VALUE 0.
  88 valid-text     VALUE 1.
  88 valid-number   VALUE 2.
  88 valid-key      VALUE 3.
  88 invalid-text   VALUE 7.
  88 invalid-number VALUE 8.
  88 invalid-key    VALUE 9.
