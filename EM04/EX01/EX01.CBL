       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EX01.
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

          SELECT  ARQ-A  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.
		  
          SELECT  ARQ-B  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.
		  
          SELECT  ARQ-C  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
	   
       FD ARQ-A 
          LABEL  RECORD ARE STANDARD
          VALUE OF FILE-ID IS "ARQA.DAT".
		  
       01 REG-A.
          02 COD-A           PIC 9(03).
          02 NOME-A          PIC X(30).
          02 TURMA-A         PIC X(20).
		  
			 
       FD ARQ-B
          LABEL RECORD ARE  STANDARD
          VALUE  OF FILE-ID IS "ARQB.DAT".
		  
       01 REG-B.
          02 COD-B           PIC 9(03).
          02 NOME-B          PIC X(30).
          02 TURMA-B         PIC X(20).
		  

       FD ARQ-C
          LABEL RECORD ARE  STANDARD
          VALUE  OF FILE-ID IS "ARQC.DAT".
		  
       01 REG-C.
          02 COD-C           PIC 9(03).
          02 NOME-C          PIC X(30).
          02 TURMA-C         PIC X(20).
	   
       WORKING-STORAGE SECTION.
       77 FIM-ARQ            PIC X(03)    VALUE "NAO".
       77 CH-A               PIC X(03)    VALUE SPACES.
       77 CH-B               PIC X(03)    VALUE SPACES.
	   
       PROCEDURE  DIVISION.
		
       EM04-EX01.
		
       PERFORM  INICIO.
       PERFORM  PRINCIPAL  UNTIL
          CH-A  EQUAL  CH-B  AND 
          CH-B  EQUAL  HIGH-VALUES.
       PERFORM  FIM.
       STOP RUN.
	   
       INICIO.
          OPEN INPUT  ARQ-A
                      ARQ-B
               OUTPUT ARQ-C.
 
          PERFORM LER-A.
          PERFORM LER-B.
 
       LER-A.
          READ ARQ-A
             AT END
             MOVE HIGH-VALUES TO CH-A.
 
          IF CH-A  EQUAL TO  HIGH-VALUES
             NEXT SENTENCE
          ELSE
             MOVE COD-A TO CH-A.
      
       LER-B.
          READ ARQ-B
             AT END
             MOVE HIGH-VALUES TO CH-B.
			 
          IF CH-B  EQUAL TO  HIGH-VALUES
             NEXT SENTENCE
          ELSE
             MOVE COD-B TO CH-B.
 
       PRINCIPAL.
          IF CH-A  EQUAL TO  CH-B
             PERFORM IGUAL
             PERFORM LER-A
             PERFORM LER-B
          ELSE IF CH-A  LESS THAN  CH-B
             PERFORM GRAVA-A
             PERFORM LER-A
          ELSE
             PERFORM GRAVA-B
             PERFORM LER-B.
 
       IGUAL.
          PERFORM GRAVA-A.
          PERFORM GRAVA-B.
       
       GRAVA-B.
          MOVE COD-B     TO   COD-C.
          MOVE NOME-B    TO   NOME-C.
          MOVE TURMA-B   TO   TURMA-C.
          WRITE REG-C.
 
       GRAVA-A.
          MOVE COD-A     TO   COD-C.
          MOVE NOME-A    TO   NOME-C.
          MOVE TURMA-A   TO   TURMA-C.
          WRITE REG-C.
 
       FIM.
          CLOSE ARQ-A
                ARQ-B
                ARQ-C.
