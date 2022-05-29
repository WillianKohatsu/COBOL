       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EX05.
       AUTHOR.          GABRIEL FRANKLIN
		                REBECCA M M MURAD
                        WILLIAN KOHATSU.
       INSTALLATION.    HOME.
       DATE-WRITTEN.    09-03-2021.
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

          SELECT  CADALU  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.

          SELECT  CADREP  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.

          SELECT  RELAPROV  ASSIGN TO DISK.

       DATA DIVISION.
       FILE SECTION.

       FD CADALU
          LABEL  RECORD  ARE  STANDARD
          VALUE  OF FILE-ID  IS  "CADALU.DAT".

       01  REG-ALU.
          02 MATRICULA-ALU          PIC 9(07).
          02 NOME-ALU               PIC X(20).
          02 NOTA1-ALU 	            PIC 9(02)V99.
          02 NOTA2-ALU              PIC 9(02)V99.
          02 FALTAS                 PIC 9(02).

       FD CADREP
          LABEL  RECORD  ARE  STANDARD
          VALUE  OF FILE-ID  IS  "CADREP.DAT".

       01  REG-REP.
          02 MATRICULA-REP          PIC 9(07).
          02 NOME-REP               PIC X(20).
          02 MEDIA-REP              PIC 9(02)V99.
          02 CONCEITO-REP           PIC X(01).

       FD RELAPROV
          LABEL  RECORD  IS  OMITTED.

       01  REG-ATR        PIC X(80).

       WORKING-STORAGE SECTION.

       77  FIM-ARQ        PIC X(03)     VALUE   "NAO".
       77  CT-LIN         PIC 9(002)    VALUE   21.
       77  CT-PAG         PIC 9(002)    VALUE   ZEROS.
       77  SOMA-NOTA      PIC 9(03)V99  VALUE   ZEROS.
       77  MEDIA          PIC 9(02)V99  VALUE   ZEROS.
       77  CONCEITO       PIC X(01).
       77  TOTAL-E        PIC 9(03)     VALUE   ZEROS.
       77  TOTAL-A        PIC 9(03)     VALUE   ZEROS.
       77  TOTAL-B        PIC 9(03)     VALUE   ZEROS.	   
	   
       01  CAB-01.
          02  FILLER      PIC X(10)     VALUE   SPACES.
          02  FILLER      PIC X(24)     VALUE   
             "FACULDADE DE TECNOLOGIA ".
          02  FILLER      PIC X(12)     VALUE
             "DE SAO PAULO".
          02  FILLER      PIC X(21)     VALUE   SPACES.
          02  FILLER      PIC X(05)     VALUE   "PAG.".
          02  VAR-PAG     PIC ZZ9.
          02  FILLER      PIC X(07)     VALUE   SPACES.

       01  CAB-02.
          02  FILLER      PIC X(08)     VALUE   SPACES.
          02  FILLER      PIC X(09)     VALUE   "MATRICULA".
          02  FILLER      PIC X(21)     VALUE   SPACES.
          02  FILLER      PIC X(04)     VALUE   "NOME".
          02  FILLER      PIC X(22)     VALUE   SPACES.
          02  FILLER      PIC X(08)     VALUE   "CONCEITO".
          02  FILLER      PIC X(08)     VALUE   SPACES.

       01  DETALHE.
          02  FILLER      PIC X(09)     VALUE   SPACES.
          02  MAT-REG     PIC 9(07).
          02  FILLER      PIC X(15)     VALUE   SPACES.
          02  NOME-REG    PIC X(20).
          02  FILLER      PIC X(16)     VALUE   SPACES.
          02  CON-REG     PIC X(01).
          02  FILLER      PIC X(12)     VALUE   SPACES.
                                             
       01  RD-1.
          02  FILLER       PIC X(17)     VALUE
             "NUMERO DE ALUNOS ".
          02  FILLER       PIC X(16)     VALUE
             "COM CONCEITO E: ".
          02  CON-E        PIC ZZ9       VALUE  ZEROS.
          02  FILLER       PIC X(44)     VALUE  SPACES.
	   
       01  RD-2.
          02  FILLER      PIC X(33)     VALUE 
             "NUMERO DE ALUNOS COM CONCEITO A: ".
          02  CON-A       PIC ZZ9       VALUE  ZEROS.
          02  FILLER      PIC X(44)     VALUE  SPACES.
		  
       01  RD-3.
          02  FILLER      PIC X(33)     VALUE
             "NUMERO DE ALUNOS COM CONCEITO B: ".
          02  CON-B       PIC ZZ9       VALUE  ZEROS.
          02  FILLER      PIC X(44)     VALUE  SPACES.
		  
       PROCEDURE DIVISION.

       PGM-EX03.

       PERFORM INICIO.

       PERFORM PRINCIPAL
          UNTIL FIM-ARQ  EQUAL  "SIM".
       PERFORM TERMINO.
          STOP RUN.

       INICIO.
          OPEN INPUT   CADALU
               OUTPUT  CADREP
                       RELAPROV.

          PERFORM LEITURA.

       LEITURA.
          READ  CADALU
             AT END
             MOVE  "SIM"  TO   FIM-ARQ.

       PRINCIPAL.

       PERFORM  MEDIA-ALUNO.
       PERFORM  IMPRESSAO.
       PERFORM  LEITURA.

       MEDIA-ALUNO.
          ADD  NOTA1-ALU
               NOTA2-ALU
             GIVING  SOMA-NOTA.
          DIVIDE  SOMA-NOTA  BY  2
             GIVING  MEDIA. 
	   
       IMPRESSAO.     
          IF  CT-LIN  GREATER THAN  19
             PERFORM  CABECALHO.
          PERFORM  VERIFICACAO.
		
       VERIFICACAO.
          IF  FALTAS  GREATER  THAN  15  OR  
              MEDIA   LESS     THAN  6
             PERFORM  IMPCAD-REP
          ELSE
             PERFORM  IMPDET.
			 
       IMPCAD-REP.
          IF  FALTAS GREATER  THAN  15 
             MOVE "F"   TO   CONCEITO
          ELSE 
             MOVE "C"   TO   CONCEITO.
			 
          MOVE  MATRICULA-ALU   TO   MATRICULA-REP.
          MOVE  NOME-ALU        TO   NOME-REP.
          MOVE  MEDIA           TO   MEDIA-REP.
          MOVE  CONCEITO        TO   CONCEITO-REP.
          WRITE  REG-REP.

       IMPDET.
          IF  MEDIA  NOT GREATER THAN  8
             MOVE  "B"  TO  CONCEITO
             ADD  1  TO  TOTAL-B
          ELSE IF  MEDIA  NOT GREATER THAN  9
             MOVE  "A"   TO CONCEITO
             ADD  1  TO  TOTAL-A
          ELSE
             MOVE  "E"  TO  CONCEITO
             ADD 1  TO TOTAL-E.
				
          MOVE  MATRICULA-ALU   TO  MAT-REG.
          MOVE  NOME-ALU        TO  NOME-REG.
          MOVE  CONCEITO        TO  CON-REG.

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
             AFTER  ADVANCING  3  LINES.

          MOVE  ZEROS  TO CT-LIN.
        
       TERMINO.
          MOVE  TOTAL-E   TO   CON-E.
          WRITE  REG-ATR  FROM  RD-1
             AFTER ADVANCING  2  LINES.
          MOVE  TOTAL-A   TO   CON-A.
          WRITE  REG-ATR  FROM  RD-2
              AFTER ADVANCING  1  LINE.
          MOVE  TOTAL-B   TO   CON-B.
          WRITE  REG-ATR  FROM  RD-3
              AFTER ADVANCING  1  LINE.		  
          CLOSE   CADALU
                  CADREP
                  RELAPROV.
