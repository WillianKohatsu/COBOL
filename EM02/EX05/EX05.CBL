       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EX05.
       AUTHOR.          GABRIEL FRANKLIN
                        REBECCA M M MURAD
                        WILLIAN KOHATSU.
       INSTALLATION.    HOME.
       DATE-WRITTEN.    23-03-2021.
       DATE-COMPILED.
       SECURITY.        APENAS O AUTOR.
      *REMARKS.         LER ARQUIVO REGENT E GERAR REGOK E RELOCOR.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
          SOURCE-COMPUTER.   DELL-PC.
          OBJECT-COMPUTER.   DELL-PC.
          SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          SELECT  REGENT  ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.

          SELECT  CADOK   ASSIGN TO DISK
          ORGANIZATION  IS  LINE SEQUENTIAL.

          SELECT  RELOCOR  ASSIGN TO DISK.

       DATA DIVISION.
       FILE SECTION.

       FD REGENT
          LABEL  RECORD  ARE  STANDARD
          VALUE  OF FILE-ID  IS  "CADEMP.DAT".

       01  REG-ENT.
          02 COD-ENT                PIC 9(05).
          02 RG-ENT                 PIC X(10).
             88 RG-INVALIDO VALUE SPACES.
          02 NOME-ENT               PIC X(30).
             88 NOME-INVALIDO VALUE SPACES.
          02 ESTADO-ENT             PIC X(02).
             88 ESTADO-VALIDO VALUE "AC" "AL" "AP" "AM" "BA"
                "CE" "DF" "ES" "GO" "MA" "MT" "MS" "MG" 
                "PA" "PB" "PR" "PE" "PI" "RJ" "RN" "RS" 
                "RO" "RR" "SC" "SP" "SE" "TO".
          02 CIDADE-ENT             PIC X(30).
             88 CIDADE-INVALIDA VALUE SPACES.
          02 EMAIL-ENT              PIC X(30).
             88 EMAIL-INVALIDO VALUE SPACES.

       FD CADOK
          LABEL  RECORD  ARE  STANDARD
          VALUE  OF FILE-ID  IS  "CADOK.DAT".

       01  REG-SAI.
          02 RG-SAI                 PIC X(10).
          02 NOME-SAI               PIC X(30).
          02 ESTADO-SAI             PIC X(02).
          02 CIDADE-SAI             PIC X(30).
          02 EMAIL-SAI              PIC X(30).
                                                                                                 
       FD RELOCOR
          LABEL  RECORD  IS  OMITTED.

       01  REG-ATR        PIC X(80).

       WORKING-STORAGE SECTION.

       77  FIM-ARQ        PIC X(03)     VALUE   "NAO".
       77  CT-LIN         PIC 9(002)    VALUE   41.
       77  CT-PAG         PIC 9(002)    VALUE   ZEROS.
	   77  CODE-CONTROL   PIC 9(05)     VALUE   ZEROS.
       
       01  CAB-01.
          02  FILLER      PIC X(10)      VALUE   SPACES.
          02  FILLER      PIC X(33)     VALUE   
		          "RELATORIO DE DADOS INCONSISTENTES".
          02  FILLER      PIC X(27)     VALUE   SPACES.
          02  FILLER      PIC X(05)     VALUE   "PAG.".
          02  VAR-PAG     PIC Z9.
          02  FILLER      PIC X(03)     VALUE   SPACES.

       01  CAB-02.
          02  FILLER      PIC X(10)     VALUE   SPACES.
          02  FILLER      PIC X(06)     VALUE   "CODIGO".
          02  FILLER      PIC X(30)     VALUE   SPACES.
          02  FILLER      PIC X(08)     VALUE   "MENSAGEM".
          02  FILLER      PIC X(26)     VALUE   SPACES.

       01  CAB-03.
          02  FILLER        PIC X(40)     VALUE   
		  "----------------------------------------".
          02  FILLER        PIC X(40)     VALUE   
		  "----------------------------------------".

       01  DETALHE.
          02  FILLER        PIC X(10)     VALUE   SPACES.
          02  COD-REL       PIC ZZ999.
          02  FILLER        PIC X(30)     VALUE   SPACES.
          02  MENSAGEM      PIC X(25).
          02  FILLER        PIC X(10)     VALUE   SPACES.

       01 MENSAGEM1.
          02  FILLER        PIC X(45)     VALUE   SPACES.
          02  MSG           PIC X(25)     VALUE   SPACES.
          02  FILLER        PIC X(10)     VALUE   SPACES.

       PROCEDURE DIVISION.

       PGM-EX05.

       PERFORM INICIO.

       PERFORM PRINCIPAL
          UNTIL FIM-ARQ  EQUAL  "SIM".
       PERFORM TERMINO.
       STOP RUN.

       INICIO.
          OPEN INPUT   REGENT
               OUTPUT  CADOK
                       RELOCOR.

          PERFORM LEITURA.

       LEITURA.
          READ  REGENT
             AT END
             MOVE  "SIM"  TO   FIM-ARQ.

       PRINCIPAL.

       PERFORM  IMPRESSAO.
       PERFORM  LEITURA.

       IMPRESSAO.     
          IF  CT-LIN  GREATER THAN  39
             PERFORM  CABECALHO.
          PERFORM  VERIFICA-RG.

       VERIFICA-RG.
          IF  RG-INVALIDO
             MOVE  COD-ENT     TO  CODE-CONTROL
             MOVE  COD-ENT     TO  COD-REL
             MOVE  "RG NAO INFORMADO"   TO  MENSAGEM
             ADD   1           TO  CT-LIN
             WRITE  REG-ATR  FROM  DETALHE
                AFTER  ADVANCING  2  LINES.
	   PERFORM  VERIFICA-NOME.
		  
       VERIFICA-NOME.
          IF  NOME-INVALIDO
             IF  CT-LIN  GREATER THAN  39
                PERFORM  CABECALHO
                PERFORM  VERIFICA-RG

             ELSE IF  CODE-CONTROL  EQUAL TO   COD-ENT
                MOVE  "NOME NAO INFORMADO"   TO  MSG
                ADD   1           TO  CT-LIN
                WRITE  REG-ATR  FROM  MENSAGEM1
                   AFTER  ADVANCING  1  LINE

             ELSE
                MOVE  COD-ENT     TO  CODE-CONTROL
                MOVE  COD-ENT     TO  COD-REL
                MOVE  "NOME NAO INFORMADO"   TO  MENSAGEM
                ADD   2           TO  CT-LIN
                WRITE  REG-ATR  FROM  DETALHE
                   AFTER  ADVANCING  2  LINE.
          PERFORM  VERIFICA-ESTADO.
		  
       VERIFICA-ESTADO.
          IF  NOT ESTADO-VALIDO
             IF  CT-LIN  GREATER THAN  39
                PERFORM  CABECALHO
                PERFORM  VERIFICA-RG

             ELSE IF  CODE-CONTROL  EQUAL TO   COD-ENT
                MOVE  "ESTADO INVALIDO"   TO  MSG
                ADD   1           TO  CT-LIN
                WRITE  REG-ATR  FROM  MENSAGEM1
                   AFTER  ADVANCING  1  LINE

             ELSE
                MOVE  COD-ENT     TO  CODE-CONTROL
                MOVE  COD-ENT     TO  COD-REL
                MOVE  "ESTADO INVALIDO"   TO  MENSAGEM
                ADD   2           TO  CT-LIN
                WRITE  REG-ATR  FROM  DETALHE
                   AFTER  ADVANCING  2  LINE.
          PERFORM  VERIFICA-CIDADE.
	      
       VERIFICA-CIDADE.
          IF  CIDADE-INVALIDA
             IF  CT-LIN  GREATER THAN  39
                PERFORM  CABECALHO
                PERFORM  VERIFICA-RG
				
             ELSE IF  CODE-CONTROL  EQUAL TO   COD-ENT
                MOVE  "CIDADE NAO INFORMADA"   TO  MSG
                ADD   1           TO  CT-LIN
                WRITE  REG-ATR  FROM  MENSAGEM1
                   AFTER  ADVANCING  1  LINE

             ELSE
                MOVE  COD-ENT     TO  CODE-CONTROL
                MOVE  COD-ENT     TO  COD-REL
                MOVE  "CIDADE NAO INFORMADA"   TO  MENSAGEM
                ADD   2           TO  CT-LIN
                WRITE  REG-ATR  FROM  DETALHE
                   AFTER  ADVANCING  2  LINE.		
          PERFORM  VERIFICA-EMAIL.          
		 
       VERIFICA-EMAIL.		 
          IF  EMAIL-INVALIDO
             IF  CT-LIN  GREATER THAN  39
                PERFORM  CABECALHO
                PERFORM  VERIFICA-RG

             ELSE IF  CODE-CONTROL  EQUAL TO   COD-ENT
                MOVE  "EMAIL SEM DESCRICAO"   TO  MSG
                ADD   1           TO  CT-LIN
                WRITE  REG-ATR  FROM  MENSAGEM1
                   AFTER  ADVANCING  1  LINE

             ELSE
                MOVE  COD-ENT     TO  CODE-CONTROL
                MOVE  COD-ENT     TO  COD-REL
                MOVE  "EMAIL SEM DESCRICAO"   TO  MENSAGEM
                ADD   2           TO  CT-LIN
                WRITE  REG-ATR  FROM  DETALHE
                   AFTER  ADVANCING  2  LINE
          ELSE
             IF  COD-ENT  NOT EQUAL TO  CODE-CONTROL
                PERFORM REGISOK.

       CABECALHO.
          MOVE  SPACES   TO  REG-ATR.
          ADD   1        TO  CT-PAG.
          MOVE  CT-PAG   TO  VAR-PAG.

          WRITE  REG-ATR
             AFTER  ADVANCING  PAGE.
          WRITE  REG-ATR  FROM  CAB-01
             AFTER  ADVANCING  3  LINE.
          WRITE  REG-ATR  FROM  CAB-02
             AFTER  ADVANCING  2  LINES.
          WRITE  REG-ATR  FROM  CAB-03
             AFTER  ADVANCING  1  LINE.

          MOVE  ZEROS  TO CT-LIN.

       REGISOK.
          MOVE   RG-ENT     TO   RG-SAI.
          MOVE   NOME-ENT   TO   NOME-SAI.
          MOVE   ESTADO-ENT TO   ESTADO-SAI.
          MOVE   CIDADE-ENT TO   CIDADE-SAI.
          MOVE   EMAIL-ENT  TO   EMAIL-SAI.
          WRITE  REG-SAI.		  
	  
       TERMINO.
          CLOSE   REGENT
                  CADOK
                  RELOCOR.
