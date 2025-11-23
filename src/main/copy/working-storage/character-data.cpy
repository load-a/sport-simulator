78 TRAIT-TABLE-LENGTH   VALUE 27.
01 Trait-Table.
  02 trait-buffer       PIC X(10).
  02 Trait-Entry        OCCURS TRAIT-TABLE-LENGTH TIMES INDEXED BY TRAIT-INDEX.
    03 trait-label      PIC X(10).
    03 trait-default    PIC X(15).
    03 trait-code       PIC X(15).
