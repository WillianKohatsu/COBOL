       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EX02.
       AUTHOR.          WILLIAN KOHATSU.
       INSTALLATION.    HOME.
       DATE-WRITTEN.    15-02-2021.
       DATE-COMPILED.
       SECURITY.        APENAS O AUTOR.
      *REMARKS.     LER OS REGISTROS DO ARQUIO DE ENTRADA CIDCLI01
      *             (CODIGO E NOME) E GRAVAR NO ARQUIVO DE SAIDA CIDCLI02.

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

       01  REG-ENT.
          02 NUM-ENT           PIC 9(05).
          02 NOME-ENT          PIC X(20).
          02 SEXO              PIC X(01).
          02 DATA-ENT.
             04 DD-ENT        PIC 9(02).
             04 MM-ENT        PIC 9(02).
             04 AAAA-ENT      PIC 9(04).

       FD CADATU
          LABEL  RECORD  ARE STANDARD
          VALUE  OF FILE-ID IS  "CADATU.DAT".

       01  REG-SAI.
          03 NUM-SAI       PIC 9(05).
          03 NOME-SAI         PIC X(20).
          03 DATA-SAI.
             05 DD-SAI        PIC 9(02).
             05 MM-SAI        PIC 9(02).
             05 AAAA-SAI      PIC 9(04).

       WORKING-STORAGE SECTION.

       77  FIM-ARQ        PIC X(03)     VALUE  "NAO".

       PROCEDURE DIVISION.

       PGM-EXERCICIO02.

       PERFORM INICIO.

       PERFORM PRINCIPAL
          UNTIL FIM-ARQ  EQUAL  "SIM".
       PERFORM TERMINO.
          STOP RUN.

       INICIO.
          OPEN INPUT   CADALU
               OUTPUT  CADATU.

          PERFORM LEITURA.

       LEITURA.
          READ  CADALU
             AT END
             MOVE  "SIM"  TO   FIM-ARQ.

       PRINCIPAL.
       PERFORM  GRAVACAO.
       PERFORM  LEITURA.

       GRAVACAO.
          MOVE  NUM-ENT   TO  NUM-SAI.
          MOVE  NOME-ENT  TO  NOME-SAI.
          MOVE  DATA-ENT  TO   DATA-SAI.
          WRITE REG-SAI.
       TERMINO.
          CLOSE   CADALU
                  CADATU.



