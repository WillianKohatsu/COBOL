        IDENTIFICATION DIVISION.
		PROGRAM-ID.    EX06.
        AUTHOR.        GABRIEL FRANKLIN
                       REBECCA M M MURAD  
                       WILLIAN KOHATSU.
        INSTALLATION.   HOME.
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
		  
           SELECT  CADSAI  ASSIGN TO DISK
		   ORGANIZATION  IS  LINE SEQUENTIAL.

        DATA DIVISION.
        FILE SECTION.
	   
        FD CADENT 
           LABEL  RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADENT.DAT".
		  
        01 REG-ENT.
           02 NOME-ENT         PIC X(30).
           02 HORAS-ENT        PIC 9(03).
           02 VALOR-ENT        PIC 9(02)V99.
           02 DESPESA-ENT      PIC 9(03)V99.
           02 IMPOSTO-ENT      PIC 9(03)V99.
       
        SD TRAB.
        01 REG-TRAB.
           02 NOME-TRAB        PIC X(30).
           02 HORAS-TRAB       PIC 9(03).
           02 VALOR-TRAB       PIC 9(02)V99.
           02 DESPESA-TRAB     PIC 9(03)V99.
           02 IMPOSTO-TRAB     PIC 9(03)V99.		  
		  
        FD RELCURA
           LABEL  RECORD ARE STANDARD
		   VALUE OF FILE-ID IS "CADSAI.DAT".
		  
        01 REG-SAI.
		   02 NOME-SAI         PIC X(30).
		   02 SALARIO-LIQ      PIC 9(05)V99.
		
		WORKING-STORAGE SECTION.
		
		77 FIM-ARQ             PIC X(03)    VALUE "NAO".
		77 AUX                 PIC 9(05)    VALUE ZEROS.
        77 SALARIO-BRUTO       PIC 9(10)    VALUE ZEROS.
		77 SALARIO-FINAL       PIC 9(05)    VALUE ZEROS.

        PROCEDURE  DIVISION.
		
        EM03-EX06.
		
        SORT    TRAB
           ASCENDING  KEY  NOME-TRAB 
           USING      CADENT
           OUTPUT     PROCEDURE  ROT-SAIDA.
        STOP RUN.
		
		ROT-SAIDA SECTION.
          PERFORM  INICIO-SAIDA.
          PERFORM  PRINCIPAL-SAIDA 
                   UNTIL FIM-ARQ EQUAL "SIM".
          PERFORM  FIM-SAIDA.
	   
	   
        INICIO-SAIDA SECTION.
           OPEN OUTPUT CADSAI.
           PERFORM LEITURA-SAIDA.
       
	   
        LEITURA-SAIDA SECTION.
           RETURN TRAB
           AT END
           MOVE "SIM" TO FIM-ARQ.
       
	   
        PRINCIPAL-SAIDA SECTION.
	       PERFORM CALCULO-SALARIO.
           PERFORM IMPRESSAO-SAIDA.
           PERFORM LEITURA-SAIDA.
		
		
		CALCULO-SALARIO SECTION.
		   MULTIPLY  HORAS-TRAB   BY   VALOR-TRAB
		      GIVING  SALARIO-BRUTO.
		   ADD  DESPESA-TRAB  IMPOSTO-TRAB
		      GIVING  AUX.
		   SUBTRACT AUX  FROM  SALARIO-BRUTO
		      GIVING  SALARIO-FINAL.
		
		
        IMPRESSAO-SAIDA SECTION.
           MOVE  NOME-TRAB     TO   NOME-SAI.
		   MOVE  SALARIO-FINAL TO   SALARIO-LIQ.
		   WRITE  REG-SAI.
			 
		
       FIM-SAIDA SECTION.
          CLOSE CADSAI.