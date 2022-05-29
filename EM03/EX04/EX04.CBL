       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EX04.
       AUTHOR.        GABRIEL FRANKLIN
                      REBECCA M M MURAD  
                      WILLIAN KOHATSU.
       INSTALLATION.  HOME.
       DATE-WRITTEN.  27-04-2021.
       DATE-COMPILED.
       SECURITY.      APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.       LER TODOS REGISTROS E ORDENAR DECRESCENTE 
      *               SOMENTE PONTUACAO MAIOR OU IGUAL A 70.

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
		  
          SELECT  CADVEST  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
	   
       FD CADENT 
          LABEL  RECORD ARE STANDARD
          VALUE OF FILE-ID IS "CADENT.DAT".
		  
       01 REG-ENT.
          02 CPF-ENT         PIC 9(11).
          02 NOME-ENT        PIC X(30).
          02 PONTO-ENT       PIC 9(03).
       
       SD TRAB.
       01 REG-TRAB.
          02 CPF-TRAB     PIC 9(11).
          02 NOME-TRAB    PIC X(30).
          02 PONTO-TRAB   PIC 9(03).
		  
       FD CADVEST
          LABEL RECORD ARE  STANDARD
          VALUE  OF FILE-ID IS "CADVEST.DAT".
		  
       01 REG-SAI.
          02 CPF-SAI         PIC 9(11).
          02 NOME-SAI        PIC X(30).
          02 PONTO-SAI       PIC 9(03).

       WORKING-STORAGE SECTION.
       77  FIM-ARQ           PIC X(03)   VALUE "NAO".

       PROCEDURE  DIVISION.
		
       EM03-EX04.
		
       SORT    TRAB
          DESCENDING KEY  PONTO-TRAB
          INPUT  PROCEDURE  ROT-ENTRADA
          GIVING CADVEST.
       STOP RUN.
	   
       ROT-ENTRADA SECTION.
          PERFORM  INICIO-ENTRADA.
          PERFORM  PRINCIPAL-ENTRADA 
                   UNTIL FIM-ARQ EQUAL "SIM".
          PERFORM  FIM-ENTRADA.
	   
       INICIO-ENTRADA SECTION.
          OPEN INPUT CADENT.
          PERFORM LEITURA-ENTRADA.
       
       LEITURA-ENTRADA SECTION.
          READ CADENT
          AT END
          MOVE "SIM" TO FIM-ARQ.
       
       PRINCIPAL-ENTRADA SECTION.
          PERFORM SELECAO-ENTRADA.
          PERFORM LEITURA-ENTRADA.
		  
       SELECAO-ENTRADA SECTION.
          IF PONTO-ENT  NOT LESS THAN 70
             PERFORM GRAVACAO-ENTRADA.
			 
       GRAVACAO-ENTRADA SECTION.
          MOVE  CPF-ENT    TO  CPF-TRAB.
          MOVE  NOME-ENT   TO  NOME-TRAB.
          MOVE  PONTO-ENT  TO  PONTO-TRAB.
          RELEASE  REG-TRAB.
		  
       FIM-ENTRADA SECTION.
          CLOSE CADENT.
		  
