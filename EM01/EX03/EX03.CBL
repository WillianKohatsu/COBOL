       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EX03.
       AUTHOR.        WILLIAN KOHATSU.
       INSTALLATION.  HOME.
       DATE-WRITTEN.  16-02-2021.
       DATE-COMPILED.
       SECURITY.      APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.       LER AS NOTAS E CALCULAR A MEDIA.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
          SOURCE-COMPUTER.   DELL-PC.
          OBJECT-COMPUTER.   DELL-PC.
          SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          SELECT  CADALU  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.

          SELECT  CADATU  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD CADALU
          LABEL  RECORD  ARE  STANDARD
          VALUE  OF FILE-ID  IS  "CADALU.DAT".

       01 REG-ENT.
          02 NUM-ENT      PIC 9(05).
          02 NOME         PIC X(20).
          02 NOTA1        PIC 9(2)V99.
          02 NOTA2        PIC 9(2)V99.
          02 NOTA3        PIC 9(2)V99.

       FD CADATU
          LABEL  RECORD  ARE  STANDARD
          VALUE  OF FILE-ID  IS  "CADATU.DAT".

       01 REG-SAI.
          02 NUM-SAI      PIC 9(05).
          02 MEDIA        PIC 9(2)V99.

       WORKING-STORAGE SECTION.

       77 FIM-ARQ      PIC X(03)         VALUE "NAO".
       77 SOMA-NOTA    PIC 9(02)V9(02)   VALUE ZEROS.
       77 WMEDIA       PIC 99V99         VALUE ZEROS.

       PROCEDURE DIVISION.
       PGM-EXE03.

          PERFORM INICIO.

          PERFORM PRINCIPAL
             UNTIL FIM-ARQ EQUAL  "SIM".
          PERFORM TERMINO.
          STOP RUN.

       INICIO.
          OPEN INPUT CADALU
               OUTPUT CADATU.

          PERFORM LEITURA.

       LEITURA.

          READ CADALU AT END
             MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
          PERFORM CALCMEDIA.
          PERFORM GRAVACAO.
          PERFORM LEITURA.

       CALCMEDIA.
          ADD  NOTA1
               NOTA2
               NOTA3
                  GIVING  SOMA-NOTA.
          DIVIDE SOMA-NOTA BY 3
             GIVING  WMEDIA.

       GRAVACAO.
          MOVE  NUM-ENT  TO  NUM-SAI.
          MOVE  WMEDIA   TO  MEDIA.
          WRITE REG-SAI.

       TERMINO.
          CLOSE  CADALU
                 CADATU.
