       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EX06.
       AUTHOR.          GABRIEL FRANKLIN
                        REBECCA M M MURAD
                        WILLIAN KOHATSU.
       INSTALLATION.    HOME.
       DATE-WRITTEN.    14-04-2021.
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
          02 COD-ENT                PIC 9(03).
          02 CNPJ-ENT.
             05 DGT1                PIC 9(01).
             05 DGT2                PIC 9(01).
             05 DGT3                PIC 9(01).
             05 DGT4                PIC 9(01).
             05 DGT5                PIC 9(01).
             05 DGT6                PIC 9(01).
             05 DGT7                PIC 9(01).
             05 DGT8                PIC 9(01).
             05 DGT9                PIC 9(01).
             05 DGT10               PIC 9(01).
             05 DGT11               PIC 9(01).
             05 DGT12               PIC 9(01).
             05 DGT13               PIC 9(01).
             05 DGT14               PIC 9(01).
          02 RAZAO-ENT              PIC X(30).
             88 RAZAO-INVALIDO  VALUE SPACES.
          02 ESTADO-ENT             PIC X(02).
             88 ESTADO-VALIDO   VALUE "AC" "AL" "AP" "AM" "BA"
                "CE" "DF" "ES" "GO" "MA" "MT" "MS" "MG" 
                "PA" "PB" "PR" "PE" "PI" "RJ" "RN" "RS" 
                "RO" "RR" "SC" "SP" "SE" "TO".
          02 CIDADE-ENT             PIC X(30).
             88 CIDADE-INVALIDA VALUE SPACES.
          02 EMAIL-ENT              PIC X(30).
             88 EMAIL-INVALIDO  VALUE SPACES.
          02 TELEFONE               PIC 9(10).
          02 DATA-ENT.
             03 DIA                 PIC 9(02).
                88 DIA-31       VALUE  1 THRU 31.
                88 DIA-30       VALUE  1 THRU 30.
                88 DIA-29       VALUE  1 THRU 29.
                88 DIA-28       VALUE  1 THRU 28.
             03 MES                 PIC 9(02).
                88 MES-30       VALUE  4 6 9 11.
                88 MES-31       VALUE  1 3 5 7 8 10 12.
                88 MES-VALIDO   VALUE  1 THRU  12.
             03 ANO                 PIC 9(04).
             
       FD CADOK
          LABEL  RECORD  ARE  STANDARD
          VALUE  OF FILE-ID  IS  "CADOK.DAT".

       01  REG-SAI.
          02 CNPJ-SAI               PIC 9(14).
          02 RAZAO-SAI              PIC X(30).
          02 ESTADO-SAI             PIC X(02).
          02 CIDADE-SAI             PIC X(30).
          02 EMAIL-SAI              PIC X(30).
          02 TELEFONE-SAI           PIC 9(10).
          02 DATA-SAI               PIC 9(08).
                                                                                                 
       FD RELOCOR
          LABEL  RECORD  IS  OMITTED.

       01  REG-ATR        PIC X(80).

       WORKING-STORAGE SECTION.

       77  FIM-ARQ        PIC X(03)     VALUE   "NAO".
       77  CT-LIN         PIC 9(002)    VALUE   41.
       77  CT-PAG         PIC 9(002)    VALUE   ZEROS.
       77  CODE-CONTROL   PIC 9(05)     VALUE   ZEROS.
       77  AUX            PIC 9(05)     VALUE   ZEROS.
       77  RESTO          PIC 9(02)     VALUE   ZEROS.
       
       77 WS-CALCULAR     PIC 9(05)     VALUE   ZEROS.
       77 WS-C            PIC 99        VALUE   ZEROS.
       77 WS-RESTO        PIC 99        VALUE   ZEROS.
       77 ADGT13          PIC 9         VALUE   ZEROS.
       77 ADGT14          PIC 9         VALUE   ZEROS.
	   
       01  CAB-01.
          02  FILLER      PIC X(27)     VALUE   SPACES.
          02  FILLER      PIC X(16)     VALUE   
             "CONVENIO BRASMED".
          02  FILLER      PIC X(27)     VALUE   SPACES.
          02  FILLER      PIC X(05)     VALUE   "PAG.".
          02  VAR-PAG     PIC Z9.
          02  FILLER      PIC X(03)     VALUE   SPACES.

       01  CAB-02.
          02  FILLER      PIC X(17)     VALUE   SPACES.
          02  FILLER      PIC X(19)     VALUE 
             "RELATORIO DE DADOS ". 
          02  FILLER      PIC X(26)     VALUE 
             "INCONSISTENTES DE EMPRESAS".
          02  FILLER      PIC X(18)     VALUE   SPACES.
	   
       01  CAB-03.
          02  FILLER      PIC X(10)     VALUE   SPACES.
          02  FILLER      PIC X(06)     VALUE   "CODIGO".
          02  FILLER      PIC X(30)     VALUE   SPACES.
          02  FILLER      PIC X(08)     VALUE   "MENSAGEM".
          02  FILLER      PIC X(26)     VALUE   SPACES.

       01  CAB-04.
          02  FILLER        PIC X(40)     VALUE   
             "----------------------------------------".
          02  FILLER        PIC X(40)     VALUE   
             "----------------------------------------".

       01  DETALHE.
          02  FILLER        PIC X(10)     VALUE   SPACES.
          02  COD-REL       PIC ZZ999.
          02  FILLER        PIC X(25)     VALUE   SPACES.
          02  MENSAGEM      PIC X(30).
          02  FILLER        PIC X(10)     VALUE   SPACES.

       01 MENSAGEM1.
          02  FILLER        PIC X(40)     VALUE   SPACES.
          02  MSG           PIC X(30)     VALUE   SPACES.
          02  FILLER        PIC X(10)     VALUE   SPACES.

       PROCEDURE DIVISION.

       PGM-EX06.

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
          PERFORM  VERIFICA-CNPJ.

       VERIFICA-CNPJ.
          COMPUTE WS-CALCULAR = (DGT1 * 5) + (DGT2 * 4) + 
          (DGT3 * 3) + (DGT4 * 2) + (DGT5 * 9) + (DGT6 * 8) + 
          (DGT7 * 7) + (DGT8 * 6) + (DGT9 * 5) + (DGT10 * 4) + 
          (DGT11 * 3) + (DGT12 * 2).
			
          DIVIDE WS-CALCULAR BY 11 GIVING WS-C REMAINDER WS-RESTO.
			
          IF WS-RESTO LESS THAN 2
             MOVE 0 TO ADGT13
          ELSE
             SUBTRACT WS-RESTO  FROM  11  GIVING ADGT13.
			
          COMPUTE WS-CALCULAR = (DGT1 * 6) + (DGT2 * 5) + 
          (DGT3 * 4) + (DGT4 * 3) + (DGT5 * 2) + (DGT6 * 9) + 
          (DGT7 * 8) + (DGT8 * 7) + (DGT9 * 6) + (DGT10 * 5) + 
          (DGT11 * 4) + (DGT12 * 3) + (ADGT13 * 2).
			
          DIVIDE WS-CALCULAR BY 11 GIVING WS-C REMAINDER WS-RESTO.
			
          IF WS-RESTO LESS THAN 2
             MOVE 0 TO ADGT14
          ELSE
             SUBTRACT WS-RESTO FROM  11 GIVING  ADGT14.
			 
          IF (ADGT13 NOT EQUAL TO DGT13) OR
             (ADGT14 NOT EQUAL TO DGT14)
             MOVE  COD-ENT     TO  CODE-CONTROL
             MOVE  COD-ENT     TO  COD-REL
             MOVE  "CNPJ INVALIDO"   TO  MENSAGEM
             ADD   1           TO  CT-LIN
             WRITE  REG-ATR  FROM  DETALHE
                AFTER  ADVANCING  2  LINES.
          PERFORM  VERIFICA-RAZAO.
		  
       VERIFICA-RAZAO.
          IF  RAZAO-INVALIDO
             IF  CT-LIN  GREATER THAN  39
                PERFORM  CABECALHO
                PERFORM  VERIFICA-CNPJ

             ELSE IF  CODE-CONTROL  EQUAL TO   COD-ENT
                MOVE  "RAZAO SOCIAL NAO INFORMADO"   TO  MSG
                ADD     1         TO  CT-LIN
                WRITE  REG-ATR  FROM  MENSAGEM1
                   AFTER  ADVANCING  1  LINE

             ELSE
                MOVE  COD-ENT     TO  CODE-CONTROL
                MOVE  COD-ENT     TO  COD-REL
                MOVE  "RAZAO SOCIAL NAO INFORMADO"   TO  MENSAGEM
                ADD   2           TO  CT-LIN
                WRITE  REG-ATR  FROM  DETALHE
                   AFTER  ADVANCING  2  LINE.
          PERFORM  VERIFICA-ESTADO.
		  
       VERIFICA-ESTADO.
          IF  NOT ESTADO-VALIDO
             IF  CT-LIN  GREATER THAN  39
                PERFORM  CABECALHO
                PERFORM  VERIFICA-CNPJ

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
                PERFORM  VERIFICA-CNPJ

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
                PERFORM  VERIFICA-CNPJ

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
                   AFTER  ADVANCING  2  LINE.
          PERFORM VERIFICA-DATA.
		 
       VERIFICA-DATA.
          IF  NOT MES-VALIDO  OR  NOT DIA-31
             PERFORM DATA-INVALIDA
          ELSE IF  MES-30  AND  DIA-30
             PERFORM DATA-VALIDA
          ELSE IF  MES-31  AND  DIA-31
             PERFORM DATA-VALIDA
          ELSE IF  MES  EQUAL TO  2
             IF  DIA  GREATER THAN 29
                PERFORM  DATA-INVALIDA
             ELSE IF DIA  EQUAL TO  29
                PERFORM  VERIFICA-ANO
             ELSE
                PERFORM  DATA-VALIDA.

       VERIFICA-ANO.
          DIVIDE ANO  BY  4  GIVING AUX  REMAINDER  RESTO.

          IF  RESTO  EQUAL  TO  0
             DIVIDE  ANO  BY  100  GIVING  AUX  REMAINDER  RESTO
             IF  RESTO  NOT EQUAL TO  0
                DIVIDE ANO BY 400 GIVING AUX REMAINDER RESTO
                IF  RESTO  EQUAL TO  0
                   PERFORM  DATA-VALIDA
                ELSE 
                   PERFORM  DATA-INVALIDA
             ELSE 
                PERFORM  DATA-INVALIDA
          ELSE
             PERFORM  DATA-INVALIDA.
	
       DATA-INVALIDA.
          IF  CT-LIN  GREATER THAN  39
             PERFORM  CABECALHO
             PERFORM  VERIFICA-CNPJ

          ELSE IF  CODE-CONTROL  EQUAL TO   COD-ENT
             MOVE  "DATA INVALIDA"   TO  MSG
             ADD   1           TO  CT-LIN
             WRITE  REG-ATR  FROM  MENSAGEM1
                AFTER  ADVANCING  1  LINE

          ELSE
             MOVE  ZEROS       TO  CODE-CONTROL
             MOVE  COD-ENT     TO  COD-REL
             MOVE  "DATA INVALIDA"   TO  MENSAGEM
             ADD   2           TO  CT-LIN
             WRITE  REG-ATR  FROM  DETALHE
                AFTER  ADVANCING  2  LINE.
				
       DATA-VALIDA.
          IF  COD-ENT  NOT EQUAL TO  CODE-CONTROL
             PERFORM  REGISOK.		  
	   
       CABECALHO.
          MOVE  SPACES   TO  REG-ATR.
          ADD   1        TO  CT-PAG.
          MOVE  CT-PAG   TO  VAR-PAG.

          WRITE  REG-ATR
             AFTER  ADVANCING  PAGE.
          WRITE  REG-ATR  FROM  CAB-01
             AFTER  ADVANCING  3  LINES.
          WRITE  REG-ATR  FROM  CAB-02
             AFTER  ADVANCING  2  LINES.
          WRITE  REG-ATR  FROM  CAB-03
             AFTER  ADVANCING  3  LINES.
          WRITE  REG-ATR  FROM  CAB-04
             AFTER  ADVANCING  1  LINE.

          MOVE  ZEROS  TO CT-LIN.

       REGISOK.
          MOVE   CNPJ-ENT   TO   CNPJ-SAI.
          MOVE   RAZAO-ENT  TO   RAZAO-SAI.
          MOVE   ESTADO-ENT TO   ESTADO-SAI.
          MOVE   CIDADE-ENT TO   CIDADE-SAI.
          MOVE   EMAIL-ENT  TO   EMAIL-SAI.
          MOVE   TELEFONE   TO   TELEFONE-SAI.
          MOVE   DATA-ENT   TO   DATA-SAI.
          WRITE  REG-SAI.		  
	  
       TERMINO.
          CLOSE   REGENT
                  CADOK
                  RELOCOR.
