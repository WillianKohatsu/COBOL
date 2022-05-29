       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EX02.
       AUTHOR.        GABRIEL FRANKLIN
                      REBECCA M M MURAD  
                      WILLIAN KOHATSU.
       INSTALLATION.  HOME.
       DATE-WRITTEN.  27-04-2021.
       DATE-COMPILED.
       SECURITY.      APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.       LER TODOS REGISTROS E ORDENAR DECRESCENTE.

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
		  
          SELECT  CADLIVRO  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.

        DATA DIVISION.
        FILE SECTION.
	   
       FD CADENT 
          LABEL  RECORD ARE STANDARD
          VALUE OF FILE-ID IS "CADENT.DAT".
		  
       01 REG-ENT.
          02 COD-ENT         PIC 9(05).
          02 NOME-ENT        PIC X(30).
          02 PRECO-ENT       PIC 9(02)V99.
		  
       SD TRAB.
       01 REG-TRAB.
          02 COD-TRAB     PIC 9(05).
          02 NOME-TRAB    PIC X(30).
          02 PRECO-TRAB   PIC 9(02)V99.
			 
       FD CADLIVRO
          LABEL RECORD ARE  STANDARD
          VALUE  OF FILE-ID IS "CADLIVRO.DAT".
		  
       01 REG-SAI.
          02 COD-SAI         PIC 9(05).
          02 NOME-SAI        PIC X(30).
          02 PRECO-SAI       PIC 9(02)V99.
		  
       PROCEDURE  DIVISION.
		
       EM03-EX02.
		
          SORT    TRAB
          DESCENDING KEY  COD-TRAB
             USING  CADENT
             GIVING CADLIVRO.
       STOP RUN.
