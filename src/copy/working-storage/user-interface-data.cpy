01 ui-prompt     PIC X(80).
01 ui-answer     PIC X(80).
  88 ui-confirmed  VALUE "Y", "YES".
  88 ui-denied     VALUE "N", "NO".
  88 ui-quitted    VALUE "Q", "QUIT", "X", "EXIT".
01 ui-number PIC 999.
  88 valid-die VALUE 2, 4, 6, 8, 10, 12, 20.
01 ui-status PIC 9 VALUE ZERO.
  88 ui-empty-answer    VALUE 0.
  88 ui-valid-text      VALUE 1.
  88 ui-valid-number    VALUE 2.
  88 ui-valid-key       VALUE 3.
  88 ui-invalid-text    VALUE 7.
  88 ui-invalid-number  VALUE 8.
  88 ui-invalid-key     VALUE 9.
