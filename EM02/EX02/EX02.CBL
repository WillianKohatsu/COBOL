       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EX02.
       AUTHOR.          GABRIEL FRANKLIN
		                REBECCA M M MURAD
                        WILLIAN KOHATSU.
       INSTALLATION.    HOME.
       DATE-WRITTEN.    16-03-2021.
       DATE-COMPILED.
       SECURITY.        APENAS O AUTOR.
      *REMARKS.     LER ARQUIVO CADENT E IMPRIMIR UM RELATORIO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
          SOURCE-COMPUTER.   DELL-PC.
          OBJECT-COMPUTER.   DELL-PC.
          SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          SELECT  CADENT  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.

          SELECT  RELSAI  ASSIGN TO DISK.

       DATA DIVISION.
       FILE SECTION.

       FD CADENT
          LABEL  RECORD  ARE  STANDARD
          VALUE  OF FILE-ID  IS  "CADENT.DAT".

       01  REG-ENT.
          02 COD-ENT                PIC 9(07).
          02 NOME-ENT               PIC X(25).
          02 CARGO                  PIC X(10).
          02 GRUPO-RISCO            PIC X(01).
          02 FATOR-ENT              PIC X(10).

       FD RELSAI
          LABEL  RECORD  IS  OMITTED.

       01  REG-ATR        PIC X(80).

       WORKING-STORAGE SECTION.

       77  FIM-ARQ          PIC X(03)     VALUE   "NAO".
       77  CT-LIN           PIC 9(002)    VALUE   20.
       77  CT-PAG           PIC 9(002)    VALUE   ZEROS.
       77  TOTAL-FUNC       PIC ZZZ9      VALUE   ZEROS.
       
       01  CAB-01.
          02  FILLER        PIC X(70)     VALUE   SPACES.
          02  FILLER        PIC X(05)     VALUE   "PAG.".
          02  VAR-PAG       PIC Z9.
          02  FILLER        PIC X(03)     VALUE   SPACES.

       01  CAB-02.
          02  FILLER        PIC X(26)     VALUE   SPACES.
          02  FILLER        PIC X(16)     VALUE
                              "FUNCIONARIOS QUE".
          02  FILLER        PIC X(01)     VALUE   SPACES.
          02  FILLER        PIC X(12)     VALUE
                              "PERTENCEM AO".
          02  FILLER        PIC X(25)     VALUE   SPACES.

       01  CAB-03.
          02  FILLER        PIC X(27)     VALUE   SPACES.
          02  FILLER        PIC X(14)     VALUE
                              "GRUPO DE RISCO".
          02  FILLER        PIC X(01)     VALUE   SPACES.
          02  FILLER        PIC X(11)     VALUE
                              "DA COVID-19".
          02  FILLWE        PIC X(27)     VALUE   SPACES.

       01  CAB-04.
          02  FILLER        PIC X(10)     VALUE   SPACES.
          02  FILLER        PIC X(06)     VALUE   "CODIGO".
          02  FILLER        PIC X(15)     VALUE   SPACES.
          02  FILLER        PIC X(04)     VALUE   "NOME".
          02  FILLER        PIC X(23)     VALUE   SPACES.
          02  FILLER        PIC X(14)     VALUE   "FATOR DE RISCO".
          02  FILLER        PIC X(08)     VALUE   SPACES.

       01  DETALHE.
          02  FILLER        PIC X(10)     VALUE   SPACES.
          02  COD-SAI       PIC 9(07).
          02  FILLER        PIC X(06)     VALUE   SPACES.
          02  NOME-SAI      PIC X(25).
          02  FILLER        PIC X(12)     VALUE   SPACES.
          02  FATOR-SAI     PIC X(10).
          02  FILLER        PIC X(10)     VALUE   SPACES.

       01  ROD.
          02  FILLER        PIC X(05)     VALUE   SPACES.
          02  FILLER        PIC X(22)     VALUE
                               "TOTAL DE FUNCIONARIOS:".
          02  FILLER        PIC X(05)     VALUE   SPACES.
          02  T-FUNC        PIC ZZZ9.
          02  FILLER        PIC X(44)     VALUE   SPACES.

       PROCEDURE DIVISION.

       PGM-EX05.

       PERFORM INICIO.

       PERFORM PRINCIPAL
          UNTIL FIM-ARQ  EQUAL  "SIM".
       PERFORM TERMINO.
          STOP RUN.

       INICIO.
          OPEN INPUT   CADENT
               OUTPUT  RELSAI.

          PERFORM LEITURA.

       LEITURA.
          READ  CADENT
             AT END
             MOVE  "SIM"  TO   FIM-ARQ.

       PRINCIPAL.

       PERFORM  IMPRESSAO.
       PERFORM  LEITURA.

       IMPRESSAO.     
          IF  CT-LIN  GREATER THAN  19
             PERFORM  CABECALHO.
          PERFORM  IMPDET.

       IMPDET.
          IF GRUPO-RISCO  EQUAL TO   "S"
             MOVE  COD-ENT     TO  COD-SAI
             MOVE  NOME-ENT    TO  NOME-SAI
             MOVE  FATOR-ENT   TO  FATOR-SAI

             WRITE  REG-ATR  FROM  DETALHE
                AFTER  ADVANCING  1  LINE
             ADD  1   TO  TOTAL-FUNC
             ADD  1   TO  CT-LIN.

       CABECALHO.
          MOVE  SPACES   TO  REG-ATR.
          ADD   1        TO  CT-PAG.
          MOVE  CT-PAG   TO  VAR-PAG.

          WRITE  REG-ATR
             AFTER  ADVANCING  PAGE.
          WRITE  REG-ATR  FROM  CAB-01
             AFTER  ADVANCING  1  LINE.
          WRITE  REG-ATR  FROM  CAB-02
             AFTER  ADVANCING  1  LINES.
          WRITE  REG-ATR  FROM  CAB-03
             AFTER  ADVANCING  1  LINES.
          WRITE  REG-ATR  FROM  CAB-04
             AFTER  ADVANCING  2  LINES.

          MOVE  ZEROS  TO CT-LIN.

       RESULTADO.
          MOVE  TOTAL-FUNC   TO  T-FUNC.
          WRITE  REG-ATR  FROM  ROD
             AFTER  ADVANCING  2  LINES.

       TERMINO.
          PERFORM  RESULTADO.
          CLOSE   CADENT
                  RELSAI.
