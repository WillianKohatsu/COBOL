       IDENTIFICATION DIVISION.
       PROGRAM-ID.     EX04.
       AUTHOR.         WILLIAN KOHATSU.
       INSTALLATION.   HOME.
       DATE-WRITTEN.   23-02-2021.
       DATE-COMPILED.
       SECURITY.       APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.        LER TODOS OS REGISTROS DE CADCLI1 E
      *                GRAVAR SOMENTE O SEXO EM CADCLI2.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

          SOURCE-COMPUTER.   DELL-PC.
          OBJECT-COMPUTER.   DELL-PC.
          SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          SELECT  CADCLI1  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.

          SELECT  CADCLI2  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

          FD CADCLI1
             LABEL RECORD ARE STANDARD
             VALUE OF FILE-ID IS "CADCLI1.DAT".

          01 REG-ENT.
             02 COD-ENT       PIC 9(05).
             02 NOME-ENT      PIC X(20).
             02 SEXO-ENT      PIC X(01).

          FD CADCLI2
             LABEL RECORD ARE STANDARD
             VALUE OF FILE-ID IS "CADCLI2.DAT".

          01 REG-SAI.
             02 SEXO-SAI      PIC X(01).

       WORKING-STORAGE SECTION.

       77 FIM-ARQ            PIC X(03)    VALUE "NAO".

       PROCEDURE DIVISION.

       PGM-EXE04.

          PERFORM INICIO.

          PERFORM PRINCIPAL
             UNTIL  FIM-ARQ  EQUAL  "SIM".
          PERFORM TERMINO.
             STOP RUN.

       INICIO.
          OPEM INPUT  CADCLI1
               OUTPUT CADCLI2.

          PERFORM LEITURA.

       LEITURA.
          READ  CADCLI1  AT END
          MOVE  "SIM"  TO  FIM-ARQ.

       PRINCIPAL.
          PERFORM  GRAVACAO.
          PERFORM  LEITURA.

       GRAVACAO.
          MOVE  SEXO-ENT  TO  SEXO-SAI.
          WRITE  REG-SAI.
       TERMINO.
          COLSE  CADCLI1
                 CADCLI2.








             

