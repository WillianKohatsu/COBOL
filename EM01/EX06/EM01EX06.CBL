       IDENTIFICATION DIVISION.
       PROGRAM-ID.       EX05.
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
           SELECT CADENT ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADSAI ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD CADENT
          LABEL RECORD ARE STANDARD
          VALUE OF FILE-ID IS "CADENT.DAT".
       01 REG-ENT.
           02 MATRICULA-ENT      PIC 9(05).
           02 NOME-ENT           PIC X(20).
           02 SALARIO-BRUTO      PIC 9(5)V99.
       FD CADSAI
          LABEL RECORD ARE STANDARD
          VALUE OF FILE-ID IS "CADSAI.DAT".     
       01 REG-SAI.
           02 MATRICULA-SAI      PIC 9(05).
           02 NOME-SAI           PIC X(20).
           02 SALARIO-BRUTO-SAI  PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       77 FIM-ARQ   PIC X(03)       VALUE "NAO".

       PROCEDURE DIVISION.

       MAIN.
           PERFORM INICIO.
           PERFORM PRINCIPAL
                   UNTIL  FIM-ARQ  EQUAL  "SIM".
           PERFORM TERMINO.
           STOP RUN.
       
       INICIO.
           OPEN INPUT  CADENT
                OUTPUT CADSAI.
           PERFORM LEITURA.

       LEITURA.
           READ CADENT
                AT END
                MOVE "SIM" TO FIM-ARQ.
       
       PRINCIPAL.
           IF SALARIO-BRUTO > 3000 THEN
              PERFORM GRAVACAO.
           PERFORM LEITURA.

       GRAVACAO.
           MOVE MATRICULA-ENT TO MATRICULA-SAI.
           MOVE NOME-ENT      TO NOME-SAI.
           MOVE SALARIO-BRUTO TO SALARIO-BRUTO-SAI.
           WRITE REG-SAI.
       
       TERMINO.
           CLOSE CADENT
                 CADSAI.
