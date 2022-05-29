       IDENTIFICATION DIVISION.
       PROGRAM-ID.     EX07.
       AUTHOR.         REBECCA M M MURAD  
                       WILLIAN KOHATSU.
       INSTALLATION.   HOME.
       DATE-WRITTEN.   02-03-2021.
       DATE-COMPILED.
       SECURITY.       APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.        LER TODOS OS REGISTROS DE CADALU E
      *                GRAVAR NUMERO, NOME E MEDIA EM CADATU.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

          SOURCE-COMPUTER.   DELL-PC.
          OBJECT-COMPUTER.   DELL-PC.
          SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          SELECT  CADALU  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.

          SELECT  CADATU  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

          FD CADALU
             LABEL RECORD ARE STANDARD
             VALUE OF FILE-ID IS "CADALU.DAT".

          01 REG-ENT.
             02 NUM-ENT       PIC 9(05).
             02 NOME-ENT      PIC X(20).
             02 NOTA1         PIC 9(02)V99.
             02 NOTA2         PIC 9(02)V99.
             02 NOTA3         PIC 9(02)V99.
             02 NOTA4         PIC 9(02)V99.
             02 SEXO-ENT      PIC X(01).

          FD CADATU
             LABEL RECORD ARE STANDARD
             VALUE OF FILE-ID IS "CADATU.DAT".

          01 REG-SAI.
             02 NUM-SAI       PIC 9(05).
             02 NOME-SAI      PIC X(20).
             02 MEDIA         PIC 9(02)V99.
             02 SEXO-SAI      PIC X(01).

       WORKING-STORAGE SECTION.

       77 FIM-ARQ            PIC X(03)        VALUE "NAO".
       77 SOMA-NOTA          PIC 9(02)V9(02)  VALUE   ZEROS.
       77 CMEDIA             PIC 99V99        VALUE   ZEROS.
       
       PROCEDURE DIVISION.

       PGM-EXE07.

          PERFORM INICIO.

          PERFORM PRINCIPAL
             UNTIL  FIM-ARQ  EQUAL  "SIM".
          PERFORM TERMINO.
             STOP RUN.

       INICIO.
          OPEN INPUT  CADALU
               OUTPUT CADATU.

          PERFORM LEITURA.

       LEITURA.
          READ  CADALU  AT END
          MOVE  "SIM"  TO  FIM-ARQ.

       PRINCIPAL.
          PERFORM  CALCMEDIA.
		  PERFORM  GRAVACAO.
          PERFORM  LEITURA.
       
          CALCMEDIA.
	      ADD  NOTA1
                   NOTA2
                   NOTA3
                   NOTA4
                     GIVING  SOMA-NOTA.
          DIVIDE  SOMA-NOTA   BY  4
                     GIVING  CMEDIA.
				
       GRAVACAO.
          MOVE   NUM-ENT   TO   NUM-SAI.
		  MOVE   NOME-ENT  TO   NOME-SAI.
                  MOVE   CMEDIA    TO   MEDIA.
		  MOVE   SEXO-ENT  TO   SEXO-SAI.
          WRITE  REG-SAI.
       
	   TERMINO.
          CLOSE  CADALU
                 CADATU.








             

