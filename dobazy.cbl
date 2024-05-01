       IDENTIFICATION DIVISION.                                
       PROGRAM-ID. DOBAZY.                                     
      * PROGRAM MA OGARNAC WSZYSTKIE DANE Z PLIKU DO BAZY      
       ENVIRONMENT DIVISION.                                   
       INPUT-OUTPUT SECTION.                                   
       FILE-CONTROL.                                           
           SELECT PLIK ASSIGN TO DD1                           
           ORGANIZATION IS INDEXED                             
           ACCESS MODE IS DYNAMIC                              
           RECORD KEY IS FS-PESEL                              
           FILE STATUS IS PLIK-STATUS.                         
       DATA DIVISION.                                          
       FILE SECTION.                                           
       FD PLIK                                                 
           DATA RECORD IS FS-PLIK                              
           RECORD CONTAINS 61 CHARACTERS.                      
       01 FS-PLIK.                                             
           05 FS-DATE PIC X(10).                       
           05 FS-IMIE PIC X(20).                       
           05 FS-NAZWISKO PIC X(20).                   
           05 FS-PESEL PIC X(11).                      
       WORKING-STORAGE SECTION.                        
           EXEC SQL  INCLUDE SQLCA END-EXEC.           
           EXEC SQL  INCLUDE DCLPIER END-EXEC.         
       01 TEMP-STRING PIC X(20).                       
       01 LICZNIK-SPACJI PIC S9(4).                    
       01 NULL-INDICATORS.                             
           05 IMIE-IND PIC S9(4) COMP.                 
           05 NAZWISKO-IND PIC S9(4) COMP.             
       01 PLIK-STATUS PIC 99.                          
       01 WS-EOF PIC X VALUE 'N'.                      
       01 CZY-POWAZNY-BLAD PIC X.                      
          88 POWAZNY-BLAD VALUE 'Y'.                   
       PROCEDURE DIVISION.                             
       MAIN.                                                       
           PERFORM OTWORZ-PLIK                                     
           PERFORM ZAPISZ-DO-BAZY UNTIL WS-EOF = 'Y' OR            
           POWAZNY-BLAD                                            
           PERFORM ZAMKNIJ-PLIK                                    
           STOP RUN.                                               
       OTWORZ-PLIK.                                                
           OPEN I-O PLIK                                           
           IF PLIK-STATUS = 0 THEN CONTINUE                        
           ELSE                                                    
           DISPLAY 'ERROR WHILE OPENING A FILE' SPACE PLIK-STATUS  
           PERFORM ABEND-PGM-PARA                                  
           END-IF                                                  
           EXIT.                                                   
       ABEND-PGM-PARA.                                             
           CALL 'ABENDPGM' USING SQLCODE                                     
           GOBACK.
                                                       
       ZAMKNIJ-PLIK.                                                
           CLOSE PLIK                                               
           EXIT.                                                    
       ZAPISZ-DO-BAZY.                                              
           INITIALIZE FS-PLIK                                       
           INITIALIZE DCL-PIER                                      
             MOVE 0 TO IMIE-IND                                     
             MOVE 0 TO NAZWISKO-IND                                 
           PERFORM CZYTAJ-1-REKORD                                  
           IF WS-EOF NOT = 'Y'                                      
                                                                    
           THEN                                                     
              PERFORM MOVE-DATA                                     
              PERFORM REPAIR-THE-DATE                               
              PERFORM CHECK-LENGTH-OF-FIELDS                        
              EXEC SQL                                              
              INSERT INTO PIERMAJ(DATA_UR,IMIE,NAZWISKO,PESEL)      
              VALUES(                                        
              :DCL-DATA-UR,                                  
              :DCL-IMIE:IMIE-IND,                            
              :DCL-NAZWISKO:NAZWISKO-IND,                    
              :DCL-PESEL)                                    
              END-EXEC                                       
               PERFORM CHECK-FOR-SQLCODE                     
                IF NOT POWAZNY-BLAD                          
                THEN                                         
                   PERFORM USUN-REKORD-Z-PLIKU               
                END-IF                                       
           ELSE                                              
           DISPLAY 'END OF DATA'                             
           END-IF                                            
           EXIT.                                             
       CZYTAJ-1-REKORD.                                      
           READ PLIK NEXT RECORD                             
           AT END                                                
              MOVE 'Y' TO WS-EOF                                 
           NOT AT END                                            
             CONTINUE                                            
           END-READ                                              
           IF PLIK-STATUS = 0 OR PLIK-STATUS = 10 THEN CONTINUE  
           ELSE                                                  
            DISPLAY 'BLAD OPCZYTU PLIKU' SPACE PLIK-STATUS       
            PERFORM ABEND-PGM-PARA                               
           END-IF                                                
           EXIT.                                                 
       MOVE-DATA.                                                
           MOVE FS-IMIE     TO DCL-IMIE-TEXT                     
           MOVE FS-NAZWISKO TO DCL-NAZWISKO-TEXT                 
           MOVE FS-PESEL    TO DCL-PESEL.                        
       CHECK-LENGTH-OF-FIELDS.                                   
      * PARAGRAF LICZY JAKA DLUGOSC MA IMIE, NAZWISKO            
      * ZAPISUJE TO DO ODPOWIEDNICH POL                            
      * JEZELI DLUGOSC ZERO TO ZAPISUJE -1 DO NULL INDICATOROW     
           INITIALIZE LICZNIK-SPACJI, DCL-NAZWISKO-LEN,            
            DCL-IMIE-LEN, TEMP-STRING                              
           MOVE FUNCTION REVERSE(DCL-NAZWISKO-TEXT) TO             
           TEMP-STRING                                             
           INSPECT TEMP-STRING TALLYING LICZNIK-SPACJI             
           FOR LEADING SPACES                                      
           COMPUTE DCL-NAZWISKO-LEN = 20 - LICZNIK-SPACJI          
           INITIALIZE TEMP-STRING, LICZNIK-SPACJI                  
           MOVE FUNCTION REVERSE(DCL-IMIE-TEXT) TO TEMP-STRING     
           INSPECT TEMP-STRING TALLYING LICZNIK-SPACJI             
           FOR LEADING SPACES                                      
           COMPUTE DCL-IMIE-LEN = 20 - LICZNIK-SPACJI              
           IF DCL-IMIE-LEN = 0 THEN                                
           MOVE -1 TO IMIE-IND END-IF                              
           IF DCL-NAZWISKO-LEN = 0 THEN                            
           MOVE -1 TO NAZWISKO-IND END-IF                          
           EXIT.                                                   
       CHECK-FOR-SQLCODE.                                          
           EVALUATE SQLCODE                                        
           WHEN 00  CONTINUE                                       
           WHEN OTHER                                              
               DISPLAY 'INSERT ERRROR SQLCODE' SPACE SQLCODE       
               MOVE 'Y' TO CZY-POWAZNY-BLAD                        
           END-EVALUATE                                            
           EXIT.                                                   
       REPAIR-THE-DATE.                                            
      * THE DATE IS GIVEN IN EUROPEAN FORMAT                       
      * WE WILL CHANGE THAT TO NORMAL FORMAT                       
      * INPUT DD-MM-YYYY OUTPUT YYYY-MM-DD                         
           DISPLAY 'DATA DCL PRZED: ' DCL-DATA-UR                  
           DISPLAY 'DATA FS PRZED: ' FS-DATE                       
           MOVE FS-DATE(1:2) TO DCL-DATA-UR(9:2)                   
           MOVE FS-DATE(4:2) TO DCL-DATA-UR(6:2)                 
           MOVE FS-DATE(7:4) TO DCL-DATA-UR(1:4)                 
           MOVE '-' TO DCL-DATA-UR(5:1)                          
           MOVE '-' TO DCL-DATA-UR(8:1)                          
           DISPLAY 'DATA FS PO: ' FS-DATE                        
           DISPLAY DCL-DATA-UR                                   
           EXIT.                                                 
       USUN-REKORD-Z-PLIKU.                                      
           DELETE PLIK                                           
           INVALID KEY                                           
           DISPLAY 'BLAD USUNIECIA REKORDU' SPACE FS-PESEL       
           NOT INVALID KEY                                       
           CONTINUE                                              
           END-DELETE                                            
           IF PLIK-STATUS  = 00 THEN CONTINUE                    
           ELSE                                                  
           DISPLAY 'BLAD USUNIECIA  STATUS' SPACE PLIK-STATUS    
           PERFORM ABEND-PGM-PARA       
           END-IF                       
           EXIT.                        