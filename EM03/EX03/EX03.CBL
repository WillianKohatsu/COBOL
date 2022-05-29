       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EX03.
       AUTHOR.        GABRIEL FRANKLIN
                      REBECCA M M MURAD  
                      WILLIAN KOHATSU.
       INSTALLATION.  HOME.
       DATE-WRITTEN.  27-04-2021.
       DATE-COMPILED.
       SECURITY.      APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.       LER TODOS REGISTROS E ORDENAR CRESCENTE 
      *               SOMENTE IDADE MAIOR OU IGUAL A 18.

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
		  
          SELECT  CADFESTA  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
	   
       FD CADENT 
          LABEL  RECORD ARE STANDARD
          VALUE OF FILE-ID IS "CADENT.DAT".
		  
       01 REG-ENT.
          02 NOME-ENT        PIC X(30).
          02 IDADE-ENT       PIC 9(02).
          02 RG-ENT          PIC X(10).
       
       SD TRAB.
       01 REG-TRAB.
          02 NOME-TRAB    PIC X(30).
          02 IDADE-TRAB   PIC 9(02).
          02 RG-TRAB      PIC X(10).
		  
       FD CADFESTA
          LABEL RECORD ARE  STANDARD
          VALUE  OF FILE-ID IS "CADFESTA.DAT".
		  
       01 REG-SAI.
          02 NOME-SAI        PIC X(30).
          02 IDADE-SAI       PIC 9(02).
          02 RG-SAI          PIC X(10).

       WORKING-STORAGE SECTION.
       77  FIM-ARQ           PIC X(03)   VALUE "NAO".

       PROCEDURE  DIVISION.
		
       EM03-EX03.
		
       SORT    TRAB
          ASCENDING KEY  IDADE-TRAB
          INPUT  PROCEDURE  ROT-ENTRADA
          GIVING CADFESTA.
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
          IF IDADE-ENT  NOT LESS THAN 18
             PERFORM GRAVACAO-ENTRADA.
			 
       GRAVACAO-ENTRADA SECTION.
          MOVE  NOME-ENT   TO  NOME-TRAB.
          MOVE  IDADE-ENT  TO  IDADE-TRAB.
          MOVE  RG-ENT     TO  RG-TRAB.
          RELEASE  REG-TRAB.
		  
       FIM-ENTRADA SECTION.
          CLOSE CADENT.
		  
