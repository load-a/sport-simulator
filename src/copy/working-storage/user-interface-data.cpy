01 ui-prompt     PIC X(80). *> GENERAL OUTPUT
01 ui-response. *> GENERAL INPUT
  02 ui-head     PIC X.
  02 ui-tail     PIC X(79).
01 ui-answer     PIC X(4). *> FOR CONFIRMATIONS ONLY
  88 ui-confirmed  VALUES "Y", "YES".
  88 ui-denied     VALUES "N", "NO".
  88 ui-quitted    VALUES "Q", "QUIT".
  88 ui-exited     VALUES "X", "EXIT".
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
