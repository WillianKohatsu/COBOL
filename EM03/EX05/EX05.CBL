       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EX05.
       AUTHOR.        GABRIEL FRANKLIN
                      REBECCA M M MURAD  
                      WILLIAN KOHATSU.
       INSTALLATION.  HOME.
       DATE-WRITTEN.  04-05-2021.
       DATE-COMPILED.
       SECURITY.      APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.       LER TODOS REGISTROS E ORDENAR CRESCENTE 
      *               POR ESTADO E CIDADE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
          SOURCE-COMPUTER.   DELL-PC.
          OBJECT-COMPUTER.   DELL-PC.
          SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          SELECT  CADENT  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.

          SELECT  TRAB  ASSIGN  TO DISK.
		  
          SELECT  RELCURA  ASSIGN TO DISK.

       DATA DIVISION.
       FILE SECTION.
	   
       FD CADENT 
          LABEL  RECORD ARE STANDARD
          VALUE OF FILE-ID IS "CADENT.DAT".
		  
       01 REG-ENT.
          02 ESTADO-ENT       PIC X(02).
          02 CIDADE-ENT       PIC X(30).
          02 DATA-ENT.
             03 DIA-ENT       PIC 9(02).
             03 MES-ENT       PIC 9(02).
             03 ANO-ENT       PIC 9(04).
          02 CASOS-ENT        PIC 9(05).
          02 CURADOS-ENT      PIC 9(05).
       
       SD TRAB.
       01 REG-TRAB.
          02 ESTADO-TRAB      PIC X(02).
          02 CIDADE-TRAB      PIC X(30).
          02 DATA-TRAB.
             03 DIA-TRAB      PIC 9(02).
             03 MES-TRAB      PIC 9(02).
             03 ANO-TRAB      PIC 9(04).
          02 CASOS-TRAB       PIC 9(05).
          02 CURADOS-TRAB     PIC 9(05).		  
		  
       FD RELCURA
          LABEL RECORD IS  OMITTED.
		  
       01 REG-ATR PIC X(80).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ            PIC X(03)    VALUE "NAO".
       77 CT-LIN             PIC 9(02)    VALUE 25.
       77 CT-PAG             PIC 9(02)    VALUE ZEROS.
       77 TOTAL-CASOS        PIC 9(10)    VALUE ZEROS.
       77 TOTAL-CURADOS      PIC 9(10)    VALUE ZEROS.
       77 CIDADE-CONTROL     PIC X(30)    VALUE SPACES.
       77 ESTADO-CONTROL     PIC X(02)    VALUE SPACES.
       77 ANO-RED            PIC 9(02)    VALUE ZEROS.
       77 AUX                PIC 9(05)    VALUE ZEROS.	   

       01 JUMP-LINHA.
          02 FILLER          PIC X(80) VALUE SPACES.
		  
       01 CAB-01.
          02 FILLER          PIC X(13) VALUE SPACES.
          02 FILLER          PIC X(27) VALUE
             "RELACAO DE PESSOAS CURADAS ".
          02 FILLER          PIC X(17) VALUE
             "POR ESTADO/CIDADE".
          02 FILLER          PIC X(13) VALUE SPACES.
          02 FILLER          PIC X(05) VALUE "PAG. ".
          02 VAR-PAG         PIC  ZZ9.
          02 FILLER          PIC X(02) VALUE SPACES.

       01 CAB-02.
          02 FILLER          PIC X(05) VALUE SPACES.
          02 FILLER          PIC X(08) VALUE "ESTADO: ".
          02 ESTADO-SAI      PIC X(02) VALUE SPACES.
          02 FILLER          PIC X(65) VALUE SPACES.
       
       01 CAB-03.
          02 FILLER          PIC X(08)  VALUE SPACES.
          02 FILLER          PIC X(08)  VALUE "CIDADE: ".
          02 CIDADE-SAI      PIC X(30)  VALUE SPACES.
          02 FILLER          PIC X(34)  VALUE SPACES.
		  
       01 CAB-04.
          02 FILLER          PIC X(11)  VALUE SPACES.
          02 FILLER          PIC X(04)  VALUE "DATA".
          02 FILLER          PIC X(14)  VALUE SPACES.
          02 FILLER          PIC X(21)  VALUE 
             "CASOS CONFIRMADOS".
          02 FILLER          PIC X(09)  VALUE SPACES.
          02 FILLER          PIC X(15)  VALUE
             "PESSOAS CURADAS".
          02 FILLER          PIC X(06)  VALUE SPACES.
       
       01 ROD-01.
          02 FILLER          PIC X(05)  VALUE SPACES.
          02 FILLER          PIC X(28)  VALUE 
             "TOTAL DE CASOS CONFIRMADOS: ".
          02 CASOS-RD        PIC ZZZZZZZZZ9.
          02 FILLER          PIC X(37)  VALUE SPACES.

       01 ROD-02.
          02 FILLER          PIC X(05)  VALUE SPACES.
          02 FILLER          PIC X(26)  VALUE 
             "TOTAL DE PESSOAS CURADAS: ".
          02 CURADOS-RD      PIC ZZZZZZZZZ9.
          02 FILLER          PIC X(39) VALUE SPACES.

       01 DETALHE.
          02 FILLER           PIC X(09) VALUE SPACES.
          02 DIA-SAI          PIC 9(02).
          02 FILLER           PIC X(01) VALUE "/".
          02 MES-SAI          PIC 9(02).
          02 FILLER           PIC X(01) VALUE "/".
          02 ANO-SAI          PIC X(02).
          02 FILLER           PIC X(20) VALUE SPACES.
          02 CASOS-SAI        PIC ZZZZ9.
          02 FILLER           PIC X(22) VALUE SPACES.
          02 CURADOS-SAI      PIC ZZZZ9.
          02 FILLER           PIC X(11) VALUE SPACES.
	   
       PROCEDURE  DIVISION.
		
       EM03-EX05.
		
       SORT    TRAB
          ASCENDING  KEY  ESTADO-TRAB 
          ASCENDING  KEY  CIDADE-TRAB
          ASCENDING  KEY  ANO-TRAB
          ASCENDING  KEY  MES-TRAB
          ASCENDING  KEY  DIA-TRAB
          USING      CADENT
          OUTPUT     PROCEDURE  ROT-SAIDA.
       STOP RUN.
	   
	   
       ROT-SAIDA SECTION.
          PERFORM  INICIO-SAIDA.
          PERFORM  PRINCIPAL-SAIDA 
                   UNTIL FIM-ARQ EQUAL "SIM".
          PERFORM  FIM-SAIDA.
	   
	   
       INICIO-SAIDA SECTION.
          OPEN OUTPUT RELCURA.
          PERFORM LEITURA-SAIDA.
          MOVE  ESTADO-TRAB  TO  ESTADO-CONTROL.
          MOVE  CIDADE-TRAB  TO  CIDADE-CONTROL.
       
	   
       LEITURA-SAIDA SECTION.
          RETURN TRAB
          AT END
          MOVE "SIM" TO FIM-ARQ.
       
	   
       PRINCIPAL-SAIDA SECTION.
          PERFORM IMPRESSAO-SAIDA.
          PERFORM LEITURA-SAIDA.
		
		
       IMPRESSAO-SAIDA SECTION.
          IF CT-LIN  GREATER THAN 19
             PERFORM  CABECALHO-SAIDA.
		  
          IF CIDADE-TRAB  NOT EQUAL TO  CIDADE-CONTROL
             PERFORM RODAPE-SAIDA
          ELSE IF  ESTADO-TRAB  NOT EQUAL TO  ESTADO-CONTROL
             PERFORM RODAPE-SAIDA
          ELSE 			 
             PERFORM  IMPDET-SAIDA.
		
		
       IMPDET-SAIDA SECTION.
          DIVIDE  ANO-TRAB  BY  100  
             GIVING AUX REMAINDER  ANO-RED.
			 
          ADD  CASOS-TRAB     TO  TOTAL-CASOS.
          ADD  CURADOS-TRAB   TO  TOTAL-CURADOS.
		  
          MOVE  DIA-TRAB     TO  DIA-SAI.
          MOVE  MES-TRAB     TO  MES-SAI.
          MOVE  ANO-RED      TO  ANO-SAI.
          MOVE  CASOS-TRAB   TO  CASOS-SAI.
          MOVE  CURADOS-TRAB TO  CURADOS-SAI.
		  
          WRITE  REG-ATR  FROM  DETALHE
             AFTER  ADVANCING  1  LINES.
          ADD  1   TO  CT-LIN.
		
		
       RODAPE-SAIDA SECTION.         
          MOVE  TOTAL-CASOS  TO  CASOS-RD.
          MOVE  TOTAL-CURADOS TO  CURADOS-RD.
          MOVE  CIDADE-TRAB  TO  CIDADE-CONTROL.
          MOVE  ESTADO-TRAB  TO  ESTADO-CONTROL.
          
          WRITE  REG-ATR  FROM  ROD-01
             AFTER ADVANCING	 2 LINES.
          WRITE  REG-ATR  FROM  ROD-02
             AFTER ADVANCING  2 LINES.	

          MOVE  ZEROS  TO  TOTAL-CASOS.
          MOVE  ZEROS  TO  TOTAL-CURADOS.

          PERFORM CABECALHO-SAIDA.
          PERFORM IMPDET-SAIDA.		  
	   
	   
       CABECALHO-SAIDA SECTION.
          MOVE  SPACES   TO  REG-ATR.
          ADD   1        TO  CT-PAG.
          MOVE  CT-PAG   TO  VAR-PAG.

          MOVE  ESTADO-TRAB  TO  ESTADO-SAI.
          MOVE  CIDADE-TRAB  TO  CIDADE-SAI.
		  
          WRITE  REG-ATR
             AFTER  ADVANCING  PAGE.
          WRITE  REG-ATR  FROM  CAB-01
             AFTER  ADVANCING  2  LINES.
          WRITE  REG-ATR  FROM  CAB-02
             AFTER  ADVANCING  3  LINES.
          WRITE  REG-ATR  FROM  CAB-03
             AFTER  ADVANCING  2  LINE.
          WRITE  REG-ATR  FROM  CAB-04
             AFTER  ADVANCING  2  LINE.
          WRITE  REG-ATR  FROM JUMP-LINHA
             AFTER  ADVANCING  1  LINE.

          MOVE  ZEROS  TO CT-LIN.
		
		
       FIM-SAIDA SECTION.
          MOVE  TOTAL-CASOS    TO  CASOS-RD.
          MOVE  TOTAL-CURADOS  TO  CURADOS-RD.
          WRITE  REG-ATR  FROM  ROD-01
             AFTER  ADVANCING  2  LINES.
          WRITE  REG-ATR  FROM  ROD-02
             AFTER  ADVANCING  2  LINES.

          CLOSE RELCURA.
