       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EX01.
       AUTHOR.          REBECCA M M MURAD
                        WILLIAN KOHATSU.
       INSTALLATION.    HOME.
       DATE-WRITTEN.    02-03-2021.
       DATE-COMPILED.
       SECURITY.        APENAS O AUTOR.
      *REMARKS.     LER TODOS OS REGISTROS DO ARQUIO DE ENTRADA CADFUN
      *             E GRAVAR O SALARIO REAJUSTADO NO ARQUIVO DE SAIDA CADSAI.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
          SOURCE-COMPUTER.   DELL-PC.
          OBJECT-COMPUTER.   DELL-PC.
          SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          SELECT  CADFUN  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.

          SELECT  CADSAI  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD CADFUN
          LABEL  RECORD  ARE  STANDARD
          VALUE  OF FILE-ID  IS  "CADFUN.DAT".

       01  REG-ENT.
          02 COD-ENT      PIC 9(05).
          02 NOME-ENT     PIC X(20).
          02 SALARIO-BR   PIC 9(05)V99.

       FD CADSAI
          LABEL  RECORD  ARE STANDARD
          VALUE  OF FILE-ID IS  "CADSAI.DAT".

       01  REG-SAI.
          03 COD-SAI       PIC 9(05).
          03 NOME-SAI      PIC X(20).
          03 SALARIO-REAJ  PIC 9(05)V9(02).

       WORKING-STORAGE SECTION.

       77  FIM-ARQ        PIC X(03)     VALUE   "NAO".
       77  REAJ           PIC 9(03)     VALUE   ZEROS.
       77  AUX            PIC 9(10)V99  VALUE   ZEROS.
       77  SALARIO-NOVO   PIC 9(10)V99  VALUE   ZEROS.
                                
       PROCEDURE DIVISION.

       PGM-EX08.

       PERFORM INICIO.

       PERFORM PRINCIPAL
          UNTIL FIM-ARQ  EQUAL  "SIM".
       PERFORM TERMINO.
          STOP RUN.

       INICIO.
          OPEN INPUT   CADFUN
               OUTPUT  CADSAI.

          PERFORM LEITURA.

       LEITURA.
          READ  CADFUN
             AT END
             MOVE  "SIM"  TO   FIM-ARQ.

       PRINCIPAL.

       PERFORM  REAJUSTE.
       PERFORM  GRAVACAO.
       PERFORM  LEITURA.

       REAJUSTE.
     
          IF SALARIO-BR  NOT GREATER THAN 1000
             MOVE  112   TO REAJ.

          IF SALARIO-BR GREATER THAN 1000
          AND SALARIO-BR  NOT GREATER THAN  2000
             MOVE  111  TO REAJ.

          IF SALARIO-BR  GREATER  THAN  2000
                MOVE  110  TO REAJ.
           PERFORM REAJ-SALARIO.

       REAJ-SALARIO.
          MULTIPLY  SALARIO-BR  BY  REAJ
                 GIVING  AUX.
          DIVIDE  AUX  BY 100
                 GIVING SALARIO-NOVO.

       GRAVACAO.
          MOVE  COD-ENT  TO  COD-SAI.
          MOVE  NOME-ENT  TO NOME-SAI.
          MOVE  SALARIO-NOVO  TO  SALARIO-REAJ.
          WRITE REG-SAI.
        
       TERMINO.
          CLOSE   CADFUN
                  CADSAI.



