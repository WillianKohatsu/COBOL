       IDENTIFICATION DIVISION.
       PROGRAM-ID.       EX03.
       AUTHOR.           REBECCA MURAD WILLIAN KOHATSU.
       INSTALLATION.     FATEC-SP.
       DATE-WRITTEN.     23/02/2021.
       DATE-COMPILED.
       SECURITY. APENAS O AUTOR PODE MODIFICA-LO.    

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  LATITUDE-PC.
       OBJECT-COMPUTER.  LATITUDE-PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADALU ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADATU ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD CADALU
          LABEL RECORD ARE STANDARD
          VALUE OF FILE-ID IS "CADALU.DAT".
       01 REG-ENT.
           02 NUMERO-ENT   PIC 9(05).
           02 NOME-ENT     PIC X(20).
           02 NOTA1-ENT    PIC 99V99.
           02 NOTA2-ENT    PIC 99V99.
           02 NOTA3-ENT    PIC 99V99.
       FD CADATU
          LABEL RECORD ARE STANDARD
          VALUE OF FILE-ID IS "CADATU.DAT".     
       01 REG-SAI.
           02 NUMERO-SAI   PIC 9(05).
           02 NOME-SAI     PIC X(20).
           02 MEDIA-SAI    PIC 99V99.

       WORKING-STORAGE SECTION.
       77 FIM-ARQ   PIC X(03)       VALUE "NAO".
       77 SOMA-NOTA PIC 99V99       VALUE ZEROS.
       77 WMEDIA    PIC 99V99       VALUE ZEROS.

       PROCEDURE DIVISION.

       MAIN.
           PERFORM INICIO.
           PERFORM PRINCIPAL
                   UNTIL  FIM-ARQ  EQUAL  "SIM".
           PERFORM TERMINO.
           STOP RUN.
       
       INICIO.
           OPEN INPUT  CADALU
                OUTPUT CADATU.
           PERFORM LEITURA.

       LEITURA.
           READ CADALU
                AT END
                MOVE "SIM" TO FIM-ARQ.
       
       PRINCIPAL.
           PERFORM CALCMEDIA.
           PERFORM GRAVACAO.
           PERFORM LEITURA.

       CALCMEDIA.
           ADD NOTA1-ENT
               NOTA2-ENT
               NOTA3-ENT GIVING  SOMA-NOTA.
           DIVIDE SOMA-NOTA BY 3 GIVING  WMEDIA.

       GRAVACAO.
           MOVE NUMERO-ENT TO NUMERO-SAI.
           MOVE NOME-ENT   TO NOME-SAI.
           MOVE WMEDIA     TO MEDIA-SAI.
           WRITE REG-SAI.
       
       TERMINO.
           CLOSE CADALU
                 CADATU.
