       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EX01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
          SOURCE-COMPUTER.   DELL-PC.
          OBJECT-COMPUTER.   DELL-PC.
          SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   
		  SELECT CADCONS ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.
		  
		  SELECT MOVCONS ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.
		  
		  SELECT ATUCONS ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.
		  
		  SELECT RELERRO ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.
		  
		  SELECT RELDEVE ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.
		  
	   DATA DIVISION.
	   FILE SECTION.
	   
	   FD CADCONS
		  LABEL RECORD ARE STANDARD
		  VALUE OF FILE-ID IS "CADCONS.DAT".
	   
	   01 REG-A.
		  02 COD-CAD		PIC 9(03).
		  02 NOME-CAD		PIC X(20).
		  02 TELE-CAD		PIC 9(11).
		  02 DATA-CAD		PIC 9(08).
		  02 PREC-CAD		PIC 99V99.
		  02 PAGO-CAD		PIC X(01).
		  
	   FD MOVCONS
		  LABEL RECORD ARE STANDARD
		  VALUE OF FILE-ID IS "MOVCONS.DAT".
		  
	   01 REG-B.
		  02 COD-MOV		PIC 9(03).
		  02 NOME-MOV		PIC X(20).
		  02 TELE-MOV		PIC 9(11).
		  02 DATA-MOV		PIC 9(08).
		  02 PREC-MOV		PIC 99V99.
		  02 PAGO-MOV		PIC X(01).
		  02 TIPO-MOV		PIC X(01).
		  
	   FD ATUCONS
		  LABEL RECORD ARE STANDARD
		  VALUE OF FILE-ID IS "ATUCONS.DAT".
		  
	   01 REG-C.
		  02 COD-ATU		PIC 9(03).
		  02 NOME-ATU		PIC X(20).
		  02 TELE-ATU		PIC 9(11).
		  02 DATA-ATU		PIC 9(08).
		  02 PREC-ATU		PIC 99V99.
		  02 PAGO-ATU		PIC X(01).
		  
	   FD RELERRO
		  LABEL RECORD IS OMMITED.
		  
	   01 REG-ATR			PIC X(80).
	   
	   FD RELDEVE
		  LABEL RECORD IS OMMITED.
		  
	   01 REG-ATI			PIC X(80).
	   
	   WORKING-STORAGE SECTION.
	   77 FIM-ARQ			PIC X(03)	VALUE "NAO".
	   77 CH-CAD			PIC X(03)	VALUE SPACES.
	   77 CH-MOV			PIC X(03)	VALUE SPACES.
	   77 CT-LIN			PIC 9(002)	VALUE 20.
	   77 CT-PAG			PIC 9(002)	VALUE ZEROS.
       77 CT-LIN2			PIC 9(002)	VALUE 20.
	   77 CT-PAG2			PIC 9(002)	VALUE ZEROS.
	   
	   01 JUMP-LINHA.
	      02 FILLER         PIC X(80)   VALUE SPACES.
          
       01 CAB-01.
          02 FILLER         PIC X(13)   VALUE SPACES.
          02 FILLER         PIC X(20)   VALUE
             "RELACAO DE CONSULTAS".
          02 FILLER         PIC X(37)   VALUE SPACES.
          02 FILLER         PIC X(05)   VALUE "PAG. ".
          02 VAR-PAG2       PIC ZZ9.
          02 FILLER         PIC X(02)   VALUE SPACES.
       
       01 CAB-02.
          02 FILLER         PIC X(16)   VALUE SPACES.
          02 FILLER         PIC X(20)   VALUE
             "ERROS DE ATUALIZACAO".
          02 FILLER         PIC X(44)   VALUE SPACES.
          
       01 CAB-03.
          02 FILLER         PIC X(03)   VALUE SPACES.
          02 FILLER         PIC X(06)   VALUE
             "CODIGO".
          02 FILLER         PIC X(03)   VALUE SPACES.
          02 FILLER         PIC X(16)   VALUE
             "NOME DO PACIENTE".
          02 FILLER         PIC X(04)   VALUE SPACES.
          02 FILLER         PIC X(16)   VALUE
             "DATA DA CONSULTA".
          02 FILLER         PIC X(07)   VALUE SPACES.
          02 FILLER         PIC X(08)   VALUE
             "MENSAGEM".
          02 FILLER         PIC X(20)   VALUE SPACES.
          
       01 DETALHE-01.
          02 FILLER         PIC X(03)   VALUE SPACES.
          02 COD-SAI-01     PIC 9(03)   VALUE ZEROS.
          02 FILLER         PIC X(03)   VALUE SPACES.
          02 NOME-SAI-01    PIC X(20)   VALUE SPACES.
          02 FILLER         PIC X(03)   VALUE SPACES.
          02 DATA-SAI       PIC 99/99/9999.
          02 FILLER         PIC X(01)   VALUE SPACES.
          02 MENSAGEM-SAI   PIC X(34)   VALUE SPACES.
          02 FILLER         PIC X(02)   VALUE SPACES.
          
       01 CAB-11.
          02 FILLER         PIC X(13)   VALUE SPACES.
          02 FILLER         PIC X(30)   VALUE
             "RELACAO DE PACIENTES DEVEDORES".
          02 FILLER         PIC X(27)   VALUE SPACES.
          02 FILLER         PIC X(05)   VALUE "PAG. ".
          02 VAR-PAG        PIC ZZ9.
          02 FILLER         PIC X(01)   VALUE SPACES.
       
       01 CAB-12.
          02 FILLER         PIC X(03)   VALUE SPACES.
          02 FILLER         PIC X(06)   VALUE
             "CODIGO".
          02 FILLER         PIC X(10)   VALUE SPACES.
          02 FILLER         PIC X(04)   VALUE
             "NOME".
          02 FILLER         PIC X(20)   VALUE SPACES.
          02 FILLER         PIC X(08)   VALUE
             "TELEFONE".
          02 FILLER         PIC X(10)   VALUE SPACES.
          02 FILLER         PIC X(05)   VALUE
             "PRECO".
          02 FILLER         PIC X(14)   VALUE SPACES.
       
       01 DETALHE-02.
          02 FILLER         PIC X(03)   VALUE SPACES.
          02 COD-SAI-02     PIC 9(03)   VALUE ZEROS.
          02 FILLER         PIC X(10)   VALUE SPACES.
          02 NOME-SAI-02    PIC X(20)   VALUE SPACES.
          02 FILLER         PIC X(07)   VALUE SPACES.
          02 TELEFONE-SAI   PIC 9(09).
          02 FILLER         PIC X(08)   VALUE SPACES.
          02 PREC-SAI       PIC Z9,99   VALUE ZEROS.
          02 FILLER         PIC X(14)   VALUE SPACES.
          
       PROCEDURE DIVISION.
       
       EM04-EX03.
       
       PERFORM INICIO.
       PERFORM PRINCIPAL UNTIL
          CH-CAD EQUAL CH-MOV AND
          CH-MOV EQUAL HIGH-VALUES.
       PERFORM FIM.
       STOP RUN.
       
       INICIO.
          OPEN INPUT     CADCONS
                         MOVCONS
               OUTPUT    ATUCONS
                         RELERRO
                         RELDEVE.
          
          PERFORM LER-CAD.
          PERFORM LER-MOV.
       
       LER-CAD.
          READ CADCONS
              AT END
              MOVE HIGH-VALUES TO CH-CAD.
              
          IF CH-CAD EQUAL TO HIGH-VALUES
              NEXT SENTENCE
          ELSE
              MOVE COD-CAD TO CH-CAD.
             
       LER-MOV.
          READ MOVCONS
              AT END
              MOVE HIGH-VALUES TO CH-MOV.
              
          IF CH-MOV EQUAL TO HIGH-VALUES
              NEXT SENTENCE
          ELSE
              MOVE COD-MOV TO CH-MOV.
             
       PRINCIPAL.
          IF CH-CAD EQUAL TO CH-MOV
              PERFORM IGUAL
              PERFORM LER-CAD
              PERFORM LER-MOV
          ELSE IF CH-CAD LESS THAN CH-MOV
              PERFORM GRAVA-CAD
              PERFORM LER-CAD
          ELSE
              PERFORM DECISAO
              PERFORM LER-MOV.
       
       IGUAL.
          IF TIPO-MOV EQUAL TO "A"
              PERFORM GRAVA-MOV
          ELSE IF TIPO-MOV EQUAL TO "I"
              PERFORM GRAVA-CAD
              MOVE 
                "INCLUSÃO DE REG. JÁ EXISTENTE" TO MENSAGEM-SAI
              PERFORM ERRO-D0.
       
       GRAVA-CAD.
          MOVE COD-CAD  TO COD-ATU.
          MOVE NOME-CAD TO NOME-ATU.
          MOVE TELE-CAD TO TELE-ATU.
          MOVE DATA-CAD TO DATA-ATU.
          MOVE PREC-CAD TO PREC-ATU.
          MOVE PAGO-CAD TO PAGO-ATU.
          WRITE REG-C.
          IF PAGO-ATU EQUAL TO "N"
              PERFORM DEVE-D0.
       
       DECISAO.
          IF TIPO-MOV EQUAL TO "I"
              PERFORM GRAVA-MOV
          ELSE IF TIPO-MOV EQUAL TO "A"
              MOVE 
                "ALTERAÇÃO DE REG. NÃO EXISTENTE" TO MENSAGEM-SAI
              PERFORM ERRO-D0
          ELSE IF TIPO-MOV EQUAL TO "E"
              MOVE 
                "EXCLUSÃO DE REG. NÃO EXISTENTE" TO MENSAGEM-SAI
              PERFORM ERRO-D0.
       
       GRAVA-MOV.
          MOVE COD-MOV  TO COD-ATU.
          MOVE NOME-MOV TO NOME-ATU.
          MOVE TELE-MOV TO TELE-ATU.
          MOVE DATA-MOV TO DATA-ATU.
          MOVE PREC-MOV TO PREC-ATU.
          MOVE PAGO-MOV TO PAGO-ATU.
          WRITE REG-C.
          IF PAGO-ATU EQUAL TO "N"
              PERFORM DEVE-D0.
       
       DEVE-D0.
          IF CT-LIN2 GREATER THAN 19
              PERFORM DEVE-CAB.
          
          PERFORM DEVE-D1.
       
       DEVE-D1.
          MOVE COD-ATU  TO COD-SAI-02.
          MOVE NOME-ATU TO NOME-SAI-02.
          MOVE TELE-ATU TO TELEFONE-SAI.
          MOVE PREC-ATU TO PREC-SAI.
          
          WRITE REG-ATI FROM DETALHE-02
              AFTER ADVANCING 1 LINE.
          ADD 1 TO CT-LIN2.
          
       DEVE-CAB.
          MOVE SPACES TO REG-ATI.
          ADD  1      TO CT-PAG2.
          MOVE CT-PAG2 TO VAR-PAG2.
          
          WRITE REG-ATI
              AFTER ADVANCING PAGE.
          WRITE REG-ATI FROM CAB-11
              AFTER ADVANCING 2 LINES.
          WRITE REG-ATI FROM CAB-12
              AFTER ADVANCING 3 LINES.
          WRITE REG-ATI FROM JUMP-LINHA
              AFTER ADVANCING 1 LINE.
          
          MOVE ZEROS TO CT-LIN2.
          
       ERRO-D0.
          IF CT-LIN GREATER THAN 19
              PERFORM ERRO-CAB.
          
          PERFORM ERRO-D1.
       
       ERRO-D1.
          MOVE COD-MOV  TO COD-SAI-01.
          MOVE NOME-MOV TO NOME-SAI-01.
          MOVE DATA-MOV TO DATA-SAI.
          
          WRITE REG-ATR FROM DETALHE-01
              AFTER ADVANCING 1 LINE.
          ADD 1 TO CT-LIN.
          
       ERRO-CAB.
          MOVE SPACES TO REG-ATR.
          ADD  1      TO CT-PAG.
          MOVE CT-PAG TO VAR-PAG.
          
          WRITE REG-ATR
              AFTER ADVANCING PAGE.
          WRITE REG-ATR FROM CAB-01
              AFTER ADVANCING 2 LINES.
          WRITE REG-ATR FROM CAB-02
              AFTER ADVANCING 3 LINES.
          WRITE REG-ATR FROM CAB-03
              AFTER ADVANCING 3 LINES.
          WRITE REG-ATR FROM JUMP-LINHA
              AFTER ADVANCING 1 LINE.
          
          MOVE ZEROS TO CT-LIN.
          
       FIM.
          CLOSE CADCONS
                MOVCONS
                ATUCONS
                RELDEVE
                RELERRO.