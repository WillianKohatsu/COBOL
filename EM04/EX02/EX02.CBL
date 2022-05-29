       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EX02.
       AUTHOR.        GABRIEL FRANKLIN
                      REBECCA M M MURAD  
                      WILLIAN KOHATSU.
       INSTALLATION.  HOME.
       DATE-WRITTEN.  25-05-2021.
       DATE-COMPILED.
       SECURITY.      APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.       INTERCALAR ARQ-A E ARQ-B GERANDO ARQ-C.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
          SOURCE-COMPUTER.   DELL-PC.
          OBJECT-COMPUTER.   DELL-PC.
          SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          SELECT  CADANT  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.
		  
          SELECT  ARQMOV  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.
		  
          SELECT  CADATU  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.
		  
		  SELECT  RELOCORR  ASSIGN TO DISK.

       DATA DIVISION.
       FILE SECTION.
	   
       FD CADANT 
          LABEL  RECORD ARE STANDARD
          VALUE OF FILE-ID IS "CADANT.DAT".
		  
       01 REG-ANT.
          02 COD-ANT          PIC 9(03).
          02 TIPO-ANT         PIC X(01).
          02 ENDER-ANT        PIC X(50).
		  
			 
       FD ARQMOV
          LABEL RECORD ARE  STANDARD
          VALUE  OF FILE-ID IS "ARQMOV.DAT".
		  
       01 REG-MOV.
          02 COD-MOV          PIC 9(03).
          02 TIPO-MOV         PIC X(01).
          02 ENDER-MOV        PIC X(50).
		  

       FD CADATU
          LABEL RECORD ARE  STANDARD
          VALUE  OF FILE-ID IS "CADATU.DAT".
		  
       01 REG-ATU.
          02 COD-ATU          PIC 9(03).
          02 TIPO-ATU         PIC X(01).
          02 ENDER-ATU        PIC X(50).

       FD RELOCORR
          LABEL RECORD IS OMMITED.
       
       01 REG-ATR            PIC X(80).
	   
       WORKING-STORAGE SECTION.
       77 FIM-ARQ            PIC X(03)    VALUE "NAO".
       77 CH-ANT             PIC X(03)    VALUE SPACES.
       77 CH-MOV             PIC X(03)    VALUE SPACES.
	   77 CT-LIN             PIC 9(002)   VALUE 45.
       77 CT-PAG             PIC 9(002)   VALUE ZEROS.

       01 JUMP-LINHA.
	      02 FILLER          PIC X(80)   VALUE SPACES.
       
       01 CAB-01.
          02 FILLER          PIC X(24) VALUE SPACES.
		  02 FILLER          PIC X(22) VALUE 
		     "RELACAO DE ATUALIZACAO".
		  02 FILLER          PIC X(24) VALUE SPACES.
          02 FILLER          PIC X(05) VALUE "PAG. ".
          02 VAR-PAG         PIC ZZ9.
          02 FILLER          PIC X(02) VALUE SPACES.

       01 CAB-02.
          02 FILLER          PIC X(05) VALUE SPACES.
          02 FILLER          PIC X(06) VALUE "CODIGO".
          02 FILLER          PIC X(09) VALUE SPACES.
          02 FILLER          PIC X(06) VALUE "TIPO".
          02 FILLER          PIC X(25) VALUE SPACES.
		  02 FILLER          PIC X(08) VALUE "MENSAGEM".
		  02 FILLER          PIC X(21) VALUE SPACES.
       
       01 DETALHE.
          02 FILLER          PIC X(06)  VALUE SPACES.
          02 COD-REL         PIC X(03)  VALUE SPACES.
          02 FILLER          PIC X(13)  VALUE SPACES.
          02 TIPO-REL        PIC X(01)  VALUE SPACES.
          02 FILLER          PIC X(12)  VALUE SPACES.
          02 MENSAGEM-SAI    PIC X(40)  VALUE SPACES.
          02 FILLER          PIC X(05)  VALUE SPACES.
       
	   PROCEDURE  DIVISION.
		
       EM04-EX02.
		
       PERFORM  INICIO.
       PERFORM  PRINCIPAL  UNTIL
          CH-ANT  EQUAL  CH-MOV  AND 
          CH-MOV  EQUAL  HIGH-VALUES.
       PERFORM  FIM.
       STOP RUN.
	   
       INICIO.
          OPEN INPUT  CADANT
                      ARQMOV
               OUTPUT CADATU
                      RELOCORR.
 
          PERFORM LER-ANT.
          PERFORM LER-MOV.
 
       LER-ANT.
          READ CADANT
             AT END
             MOVE HIGH-VALUES TO CH-ANT.
 
          IF CH-ANT  EQUAL TO  HIGH-VALUES
             NEXT SENTENCE
          ELSE
             MOVE COD-ANT TO CH-ANT.
      
       LER-MOV.
          READ ARQMOV
             AT END
             MOVE HIGH-VALUES TO CH-MOV.
			 
          IF CH-MOV  EQUAL TO  HIGH-VALUES
             NEXT SENTENCE
          ELSE
             MOVE COD-MOV TO CH-MOV.
 
       PRINCIPAL.
          IF CH-ANT  EQUAL TO  CH-MOV
             PERFORM IGUAL
             PERFORM LER-ANT
             PERFORM LER-MOV
          ELSE IF CH-ANT  LESS THAN  CH-MOV
             PERFORM GRAVA-ANT
             PERFORM LER-ANT
          ELSE
             PERFORM DECISAO
             PERFORM LER-MOV.
       
       DECISAO.
          IF TIPO-MOV EQUAL TO "I"
              PERFORM GRAVA-MOV
          ELSE IF TIPO-MOV EQUAL TO "A"
              MOVE COD-MOV  TO COD-REL
              MOVE TIPO-MOV TO TIPO-REL
              MOVE 
                "ALTERAÇÃO P/REG. INEXISTENTE" TO MENSAGEM-SAI
              PERFORM ERRO-D0
          ELSE IF TIPO-MOV EQUAL TO "E"
              MOVE COD-MOV  TO COD-REL
              MOVE TIPO-MOV TO TIPO-REL
              MOVE 
                "EXCLUSÃO P/REG. INEXISTENTE" TO MENSAGEM-SAI
              PERFORM ERRO-D0.
 
       IGUAL.
          IF TIPO-MOV EQUAL TO "A"
              MOVE COD-ANT  TO COD-REL
              MOVE TIPO-ANT TO TIPO-REL
              MOVE
                "REGISTRO SUBSTITUIDO" TO MENSAGEM-SAI
              PERFORM ERRO-D0
              PERFORM GRAVA-MOV
          ELSE IF TIPO-MOV EQUAL TO "I"
              PERFORM GRAVA-ANT
              MOVE COD-MOV  TO COD-REL
              MOVE TIPO-MOV TO TIPO-REL              
              MOVE 
                "INCLUSÃO P/REG. JÁ EXISTENTE" TO MENSAGEM-SAI
              PERFORM ERRO-D0
          ELSE IF TIPO-MOV EQUAL TO "E"
              MOVE COD-ANT  TO COD-REL
              MOVE TIPO-ANT TO TIPO-REL
              MOVE
                "REGISTRO EXCLUIDO" TO MENSAGEM-SAI
              PERFORM ERRO-D0.
          
       
       GRAVA-MOV.
          MOVE COD-MOV     TO   COD-ATU.
          MOVE TIPO-MOV    TO   TIPO-ATU.
          MOVE ENDER-MOV   TO   ENDER-ATU.
          WRITE REG-ATU.
          IF TIPO-MOV EQUAL TO "I"
              MOVE COD-MOV  TO COD-REL
              MOVE TIPO-MOV TO TIPO-REL              
              MOVE
                "REGISTRO INCLUIDO" TO MENSAGEM-SAI
              PERFORM ERRO-D0
          ELSE IF TIPO-MOV EQUAL TO "A"
              MOVE COD-MOV  TO COD-REL
              MOVE TIPO-MOV TO TIPO-REL
              MOVE
                "REGISTRO SUBSTITUTO" TO MENSAGEM-SAI
              PERFORM ERRO-D0.
 
       GRAVA-ANT.
          MOVE COD-ANT     TO   COD-ATU
          MOVE TIPO-ANT    TO   TIPO-ATU.
          MOVE ENDER-ANT   TO   ENDER-ATU.
          WRITE REG-ATU.
          
          
       ERRO-D0.
          IF CT-LIN GREATER THAN 39
              PERFORM ERRO-CAB.
          
          PERFORM ERRO-D1.
       
       ERRO-D1.          
          WRITE REG-ATR FROM DETALHE
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
          WRITE REG-ATR FROM JUMP-LINHA
              AFTER ADVANCING 1 LINE.
          
          MOVE ZEROS TO CT-LIN.
 
       FIM.
          CLOSE CADANT
                ARQMOV
                CADATU
                RELOCORR.
