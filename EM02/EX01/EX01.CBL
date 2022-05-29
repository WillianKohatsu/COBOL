       IDENTIFICATION DIVISION.
       PROGRAM-ID.     EX01.
       AUTHOR.         GABRIEL FRANKLIN
                       REBECCA M M MURAD
                       WILLIAN KOHATSU.
       INSTALLATION.   HOME.
       DATE-WRITTEN.   16-03-2021.
       DATE-COMPILED.
       SECURITY.       NAO HA, NAO ALTERAR.
      *REMARKS.        LER CADSOC1 E GERAR CADSOC2 E RELSOCIO.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. DELL-PC.
       OBJECT-COMPUTER. DELL-PC.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
          SELECT CADSOC1 ASSIGN TO DISK
          ORGANIZATION IS LINE SEQUENTIAL.
           
          SELECT CADSOC2 ASSIGN TO DISK
          ORGANIZATION IS LINE SEQUENTIAL.
           
          SELECT REL-ATR ASSIGN TO DISK.
           

       DATA DIVISION.
       FILE SECTION.
       
       FD CADSOC1
          LABEL RECORD ARE STANDARD                                    
          VALUE OF FILE-ID IS "CADSOC1.DAT".
       
       01 REG-SOC1.
          02 COD-ENT            PIC 9(02).
          02 NUM-ENT            PIC 9(06).
          02 NOME-ENT           PIC X(30).
          02 VALOR-ENT          PIC 9(09)V9(02).

       FD CADSOC2
           LABEL RECORD ARE STANDARD                                    
           VALUE OF FILE-ID IS "CADSOC2.DAT".
       
       01 REG-SOC2.
          02 NUM-SAI            PIC 9(06).
          02 NOME-SAI           PIC X(30).
          02 VALOR-SAI          PIC 9(09)V9(02).

       FD REL-ATR
          LABEL RECORD IS OMITTED.
       
       01 REG-ATR PIC X(80).

                                                                                                                                                                                                                                                                                                                
       WORKING-STORAGE SECTION.
       77 FIM-ARQ      PIC X(003)    VALUE "NAO".
       77 CT-LIN       PIC 9(002)    VALUE 30.
       77 CT-PAG       PIC 9(002)    VALUE ZEROS.
       77 TOTAL-VALOR  PIC 9(15)V99  VALUE ZEROS.
       77 TOTAL-SOCIO  PIC 9(05)     VALUE ZEROS.

       01 CAB-01.
          02 FILLER          PIC X(70) VALUE SPACES.
          02 FILLER          PIC X(05) VALUE "PAG. ".
          02 VAR-PAG         PIC  ZZ9.
          02 FILLER          PIC X(02) VALUE SPACES.

       01 CAB-02.
          02 FILLER          PIC X(23) VALUE SPACES.
          02 FILLER          PIC X(12) VALUE "RELATORIO DE".
          02 FILLER          PIC X(01) VALUE SPACES.
          02 FILLER          PIC X(21) VALUE "SOCIOS ATRASADOS".
          02 FILLER          PIC X(23) VALUE SPACES.
       
       01 CAB-03.
          02 FILLER          PIC X(02)  VALUE SPACES.
          02 FILLER          PIC X(15)  VALUE "NUMERO DO SOCIO".
          02 FILLER          PIC X(09)  VALUE SPACES.
          02 FILLER          PIC X(13)  VALUE "NOME DO SOCIO".
          02 FILLER          PIC X(15)  VALUE SPACES.
          02 FILLER          PIC X(18)  VALUE "VALOR DO PAGAMENTO".
          02 FILLER          PIC X(08)  VALUE SPACES.
       
        01 ROD-01.
          02 FILLER          PIC X(09) VALUE "TOTAL DE ".
          02 FILLER          PIC X(19) VALUE "SOCIOS ATRASADOS:  ".
          02 TSOCIO          PIC ZZZZ9.
          02 FILLER          PIC X(47) VALUE SPACES.

       01 ROD-02.
          02 FILLER          PIC X(12) VALUE "VALOR TOTAL ".
          02 FILLER          PIC X(15) VALUE "ATRASADO:      ".
          02 TVALOR          PIC ZZZZZZZZZZ9,99.
          02 FILLER          PIC X(39) VALUE SPACES.

       01 DETALHE.
          02 FILLER         PIC X(06) VALUE SPACES.
          02 NUM-SOC        PIC 9(06).
          02 FILLER         PIC X(06) VALUE SPACES.
          02 NOME-SOC       PIC X(30).
          02 FILLER         PIC X(11) VALUE SPACES.
          02 VAL            PIC ZZZZZZZZ9,99.
          02 FILLER         PIC X(10) VALUE SPACES.
                             
       PROCEDURE DIVISION.
       EXEMPLO.
          PERFORM INICIO.
          PERFORM PRINCIPAL
             UNTIL FIM-ARQ EQUAL "SIM".
          PERFORM TERMINO.
          STOP RUN.

       INICIO.
          OPEN INPUT CADSOC1
             OUTPUT CADSOC2
             OUTPUT REL-ATR.
          PERFORM LEITURA.
       LEITURA.
          READ CADSOC1
          AT END
          MOVE "SIM" TO FIM-ARQ.
       PRINCIPAL.
          PERFORM IMPRESSAO.
          PERFORM LEITURA.
           
       IMPRESSAO.
          IF CT-LIN GREATER THAN 29
             PERFORM CABECALHO.
          IF COD-ENT EQUAL 2
             ADD 1 TO TOTAL-SOCIO
             ADD VALOR-ENT TO TOTAL-VALOR
             PERFORM IMPDET
          ELSE
             PERFORM PAGOS.
       
       PAGOS.
          MOVE NUM-ENT   TO NUM-SAI
          MOVE NOME-ENT  TO NOME-SAI
          MOVE VALOR-ENT TO VALOR-SAI
          WRITE REG-SOC2.
       
       IMPDET.
          MOVE NUM-ENT     TO  NUM-SOC .
          MOVE NOME-ENT    TO  NOME-SOC.
          MOVE VALOR-ENT   TO  VAL.
           
          WRITE REG-ATR FROM DETALHE
             AFTER ADVANCING 1 LINES.
          ADD 1 TO CT-LIN.
       
       CABECALHO.
          MOVE SPACES TO REG-ATR.
          ADD 1 TO CT-PAG.
          MOVE CT-PAG TO VAR-PAG.
     
          WRITE REG-ATR
             AFTER ADVANCING PAGE.
          WRITE REG-ATR FROM CAB-01
             AFTER ADVANCING 2 LINE.
          WRITE REG-ATR FROM CAB-02
             AFTER ADVANCING 2 LINES.
          WRITE REG-ATR FROM CAB-03
             AFTER ADVANCING 3 LINES.
          MOVE SPACES TO REG-ATR.
          WRITE REG-ATR.
          MOVE ZEROES TO CT-LIN.    
       
       TERMINO.
          MOVE TOTAL-SOCIO TO TSOCIO.
          MOVE TOTAL-VALOR TO TVALOR.
          WRITE REG-ATR FROM ROD-01
             AFTER ADVANCING 4 LINE.
          WRITE REG-ATR FROM ROD-02
             AFTER ADVANCING 1 LINE.
          
          CLOSE  CADSOC1
                 CADSOC2
                 REL-ATR.
