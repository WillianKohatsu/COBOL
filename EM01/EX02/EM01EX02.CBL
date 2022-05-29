       IDENTIFICATION DIVISION.
       PROGRAM-ID.       EX02.
       AUTHOR.           REBECCA MURAD WILLIAN KOHATSU.
       INSTALLATION.     FATEC-SP.
       DATE-WRITTEN.     16/02/2021.
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
           02 SEXO-ENT     PIC X(01).
           02 DATA-NASCIMENTO.
               03 DD-ENT   PIC 99.
               03 MM-ENT   PIC 99.
               03 AAAA-ENT PIC 9999.  
       FD CADATU
          LABEL RECORD ARE STANDARD
          VALUE OF FILE-ID IS "CADATU.DAT".     
       01 REG-SAI.
           02 NUMERO-SAI   PIC 9(05).
           02 NOME-SAI     PIC X(20).
           02 DATA-NASCIMENTO.
               03 DD-SAI   PIC 99.
               03 MM-SAI   PIC 99.
               03 AAAA-SAI PIC 9999.

       WORKING-STORAGE SECTION.
       77 FIM-ARQ   PIC X(03)   VALUE "NAO".

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
           PERFORM GRAVACAO.
           PERFORM LEITURA.

       GRAVACAO.
           MOVE NUMERO-ENT TO NUMERO-SAI.
           MOVE NOME-ENT TO NOME-SAI.
           MOVE DD-ENT TO DD-SAI.
           MOVE MM-ENT TO MM-SAI.
           MOVE AAAA-ENT TO AAAA-SAI.
           WRITE REG-SAI.
       
       TERMINO.
           CLOSE CADALU
                 CADATU.
