       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EX04.
       AUTHOR.          GABRIEL FRANKLIN
		                REBECCA M M MURAD
                        WILLIAN KOHATSU.
       INSTALLATION.    HOME.
       DATE-WRITTEN.    09-04-2021.
       DATE-COMPILED.
       SECURITY.        APENAS O AUTOR.
      *REMARKS.     LER UM ARQUIVO E IMPRIMIR UM RELATORIO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
          SOURCE-COMPUTER.   DELL-PC.
          OBJECT-COMPUTER.   DELL-PC.
          SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          SELECT  CADPROD  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.

          SELECT  RELPROD  ASSIGN TO DISK.

       DATA DIVISION.
       FILE SECTION.

       FD CADPROD
          LABEL  RECORD  ARE  STANDARD
          VALUE  OF FILE-ID  IS  "CADPROD.DAT".

       01  REG-PROD.
          02 COD-PRODUTO            PIC 9(07).
          02 NOME-PRODUTO           PIC X(20).
          02 PRECO-PRODUTO          PIC 9(05)V99.

       FD RELPROD
          LABEL  RECORD  IS  OMITTED.

       01  REG-ATR        PIC X(80).

       WORKING-STORAGE SECTION.

       77  FIM-ARQ        PIC X(03)     VALUE   "NAO".
       77  CT-LIN         PIC 9(002)    VALUE   21.
       77  CT-PAG         PIC 9(002)    VALUE   ZEROS.
       77  SOMA-PROD      PIC 9(08)V99  VALUE   ZEROS.
       77  QUANTIDADE     PIC 9(10)     VALUE   ZEROS.
       77  MEDIA          PIC 9(05)V99  VALUE   ZEROS.
       77  MAIOR          PIC 9(05)V99  VALUE   ZEROS.
       77  MENOR          PIC 9(05)V99  VALUE   99999.           
	   
       01  CAB-01.
          02  FILLER      PIC X(20)     VALUE   SPACES.
          02  FILLER      PIC X(22)     VALUE   
             "SUPERMERCADO COMPREBEM".
          02  FILLER      PIC X(23)     VALUE   SPACES.
          02  FILLER      PIC X(05)     VALUE   "PAG.".
          02  VAR-PAG     PIC ZZ9.
          02  FILLER      PIC X(07)     VALUE   SPACES.

       01  CAB-02.
          02  FILLER      PIC X(09)     VALUE   SPACES.
          02  FILLER      PIC X(06)     VALUE   "CODIGO".
          02  FILLER      PIC X(22)     VALUE   SPACES.
          02  FILLER      PIC X(04)     VALUE   "NOME".
          02  FILLER      PIC X(22)     VALUE   SPACES.
          02  FILLER      PIC X(05)     VALUE   "PRECO".
          02  FILLER      PIC X(12)     VALUE   SPACES.
       
       01  CAB-03.
          02  FILLER      PIC X(80)     VALUE   SPACES.
		  
       01  DETALHE.
          02  FILLER      PIC X(09)     VALUE   SPACES.
          02  CODIGO      PIC 9(07).
          02  FILLER      PIC X(12)     VALUE   SPACES.
          02  NOME        PIC X(20).
          02  FILLER      PIC X(16)     VALUE   SPACES.
          02  PRECO       PIC ZZZZ9,99.
          02  FILLER      PIC X(09)     VALUE   SPACES.
                                             
       01  RD-1.
          02  FILLER       PIC X(09)    VALUE   SPACES.
          02  FILLER       PIC X(31)    VALUE
             "MEDIA DOS PRECOS DOS PRODUTOS: ".
          02  MEDIA-PROD   PIC ZZZZ9,99 VALUE   ZEROS.
          02  FILLER       PIC X(33)    VALUE   SPACES.
	   
       01  RD-2.
          02  FILLER      PIC X(09)     VALUE   SPACES.
          02  FILLER      PIC X(18)     VALUE 
             "PRECO MAIS BAIXO: ".
          02  MENOR-P     PIC ZZZZ9,99  VALUE   ZEROS.
          02  FILLER      PIC X(46)     VALUE   SPACES.
		  
       01  RD-3.
          02  FILLER      PIC X(09)     VALUE   SPACES.
          02  FILLER      PIC X(17)     VALUE
             "PRECO MAIS ALTO: ".
          02  MAIOR-P     PIC ZZZZ9,99  VALUE   ZEROS.
          02  FILLER      PIC X(47)     VALUE   SPACES.
		  
       PROCEDURE DIVISION.

       PGM-EX04.

       PERFORM INICIO.

       PERFORM PRINCIPAL
          UNTIL FIM-ARQ  EQUAL  "SIM".
       PERFORM CALC-MEDIA.
       PERFORM TERMINO.
          STOP RUN.

       INICIO.
          OPEN INPUT   CADPROD
               OUTPUT  RELPROD.

          PERFORM LEITURA.

       LEITURA.
          READ  CADPROD
             AT END
             MOVE  "SIM"  TO   FIM-ARQ.

       PRINCIPAL.

       PERFORM  SOMA-PRODUTO.
       PERFORM  IMPRESSAO.
       PERFORM  LEITURA.

       SOMA-PRODUTO.
          ADD  PRECO-PRODUTO
             TO  SOMA-PROD.
          ADD  1  TO  QUANTIDADE.
      
       CALC-MEDIA.
          DIVIDE  SOMA-PROD  BY  QUANTIDADE
             GIVING  MEDIA.        
	   
       IMPRESSAO.     
          IF  CT-LIN  GREATER THAN  19
             PERFORM  CABECALHO.
          PERFORM  IMPDET.

       IMPDET.
          IF  PRECO-PRODUTO  LESS THAN  MENOR
             MOVE  PRECO-PRODUTO   TO   MENOR. 
          IF  PRECO-PRODUTO  GREATER THAN  MAIOR 
             MOVE  PRECO-PRODUTO   TO   MAIOR.
				
          MOVE  COD-PRODUTO      TO  CODIGO.
          MOVE  NOME-PRODUTO     TO  NOME.
          MOVE  PRECO-PRODUTO    TO  PRECO.

          WRITE  REG-ATR  FROM  DETALHE
             AFTER  ADVANCING  1  LINES.
          ADD  1   TO  CT-LIN.

       CABECALHO.
          MOVE  SPACES   TO  REG-ATR.
          ADD   1        TO  CT-PAG.
          MOVE  CT-PAG   TO  VAR-PAG.

          WRITE  REG-ATR
             AFTER  ADVANCING  PAGE.
          WRITE  REG-ATR  FROM  CAB-01
             AFTER  ADVANCING  2  LINES.
          WRITE  REG-ATR  FROM  CAB-02
             AFTER  ADVANCING  4  LINES.
          WRITE  REG-ATR  FROM  CAB-03
             AFTER  ADVANCING  1  LINE.

          MOVE  ZEROS  TO CT-LIN.
        
       TERMINO.
		  MOVE  MEDIA     TO    MEDIA-PROD.
          WRITE  REG-ATR  FROM  RD-1
             AFTER ADVANCING  2  LINES.
          MOVE  MENOR     TO   MENOR-P.
          WRITE  REG-ATR  FROM  RD-2
              AFTER ADVANCING  1  LINE.
          MOVE  MAIOR     TO   MAIOR-P.
          WRITE  REG-ATR  FROM  RD-3
              AFTER ADVANCING  1  LINE.		  
          CLOSE   CADPROD
                  RELPROD.
