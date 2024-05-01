       IDENTIFICATION DIVISION.                                     
       PROGRAM-ID. USERP.                                           
       DATA DIVISION.                                               
       WORKING-STORAGE SECTION.                                     
           COPY USER.                                               
           COPY DFHAID.                                             
       01 RESPCODE PIC S9(8) COMP.                                  
       01 WS-COMMAREA PIC X VALUE 'A'.                              
       01 WHAT-MAP-FLAG PIC X.                                      
           88 SEND-WHOLE-MAP VALUE 'W'.                             
           88 SEND-DATAONLY VALUE 'D'.                              
           88 SEND-ERASE    VALUE 'E'.                              
       01 USER-MESSAGES.                                            
           05 INVALID-KEY-MSG PIC X(11) VALUE 'INVALID KEY'.        
           05 EXIT-MSG PIC X(22) VALUE 'TRANSACTION TERMINATED'.    
           05 OTHER-ERROR-MSG PIC X(11) VALUE 'OTHER ERROR'.        
           05 SUCCESS-MSG  PIC X(19) VALUE 'WRITEN SUCCESSFULLY'.   
           05 ABNORMAL-EXIT-MSG PIC X(13) VALUE 'ABNORMAL EXIT'.    
           05 SEND-MAP-ERROR-MSG PIC X(16) VALUE 'DISPLAYING ERROR'.  
           05 MAPFAIL-ERROR-MSG PIC X(16) VALUE 'PROVIDE DATA !!!'.   
           05 INVALID-PESEL-MSG PIC X(15) VALUE 'INVALID PESEL !'.    
           05 DUPKEY-ERROR-MSG  PIC X(16) VALUE 'DUPLICATED PESEL'.   
           05 INVALID-DATE-MSG PIC X(12) VALUE 'INVALID DATE'.        
       01 PROGRAM-VARIABLES.                                          
           05 WS-NAME PIC X(20).                                      
           05 WS-SURN PIC X(20).                                      
           05 WS-PESEL PIC X(11).                                     
           05 WS-DATE PIC X(10).                                      
       01 IF-RECEIVE-SUCCESSFULL PIC X.                               
           88 RECEIVE-SUCCESSFULL VALUE 'Y'.                          
       01 PESELCK-AREA.                                               
           05 PESELCK-PESEL PIC X(11).                                
           05 PESEL-STATUS PIC X.                                     
      * IF Y ALL GOOD                                                 
      * IF N NOT GUT                                                  
      * IF OTHER SUBPROG ERROR                                        
       01 DATECK-AREA.                                                
           05 DATECK-DATE PIC X(10).                                  
           05 DATE-STATUS PIC X.                                      
      * IF Y ALL GOOD                                                 
      * IF N NOT GUT                                                  
      * IF OTHER SUBPROG ERROR                                        
       01 FS-FUSER.                                                   
           05 FS-DATE  PIC X(10).                                     
           05 FS-NAME  PIC X(20).                                     
           05 FS-SURN  PIC X(20).                                     
           05 FS-PESEL PIC X(11).                                     
       PROCEDURE DIVISION.                                            
       MAIN.                                                          
           IF EIBCALEN = 0                                            
              PERFORM RUN-FIRST-TIME-PARA                             
           ELSE                                                       
              EVALUATE EIBAID                                 
              WHEN DFHENTER  PERFORM PROCESS-DATA-PARA        
              WHEN DFHPF1    PERFORM CLEAR-SCREEN-PARA        
              WHEN DFHPF3    PERFORM EXIT-PROGRAM-PARA        
              WHEN OTHER                                      
                 MOVE 'D' TO WHAT-MAP-FLAG                    
                 MOVE INVALID-KEY-MSG TO MSGO                 
                 PERFORM SEND-THE-MAP-PARA                    
              END-EVALUATE                                    
           END-IF                                             
           EXEC CICS                                          
           RETURN TRANSID('USER') COMMAREA(WS-COMMAREA)       
           END-EXEC                                           
           GOBACK.                                            
       SEND-THE-MAP-PARA.                                     
           EVALUATE TRUE                                      
           WHEN SEND-WHOLE-MAP                                
               EXEC CICS                                          
               SEND MAP('MAP1') MAPSET('USER')                    
               FROM(MAP1O)                                        
               ERASE                                              
               RESP(RESPCODE)                                     
               END-EXEC                                           
           WHEN SEND-DATAONLY                                     
               EXEC CICS                                          
               SEND MAP('MAP1') MAPSET('USER')                    
               FROM(MAP1O)                                        
               DATAONLY                                           
               FREEKB                                             
               RESP(RESPCODE)                                     
               END-EXEC                                           
           WHEN SEND-ERASE                                        
               EXEC CICS                                          
               SEND MAP('MAP1') MAPSET('USER')                    
               FROM(MAP1O)                                        
               DATAONLY                                           
               FREEKB                                             
               ERASEAUP                                           
               RESP(RESPCODE)                                     
               END-EXEC                                           
           WHEN OTHER                                             
               MOVE 'D' TO WHAT-MAP-FLAG                          
               MOVE SEND-MAP-ERROR-MSG TO MSGO                    
               PERFORM SEND-THE-MAP-PARA                          
           END-EVALUATE                                           
           IF RESPCODE NOT = DFHRESP(NORMAL)                      
           THEN                                                   
               MOVE SEND-MAP-ERROR-MSG TO MSGO                    
               MOVE 'D' TO WHAT-MAP-FLAG                          
               PERFORM SEND-THE-MAP-PARA                          
           END-IF                                                 
           EXIT.                                            
       RUN-FIRST-TIME-PARA.                                 
            MOVE LOW-VALUES TO MAP1O                        
            MOVE 'W' TO WHAT-MAP-FLAG                       
            PERFORM SEND-THE-MAP-PARA                       
           EXIT.                                            
       EXIT-PROGRAM-PARA.                                   
            EXEC CICS                                       
            SEND TEXT FROM(EXIT-MSG)                        
            ERASE                                           
            END-EXEC                                        
            EXEC CICS                                       
            RETURN                                          
            END-EXEC                                        
            GOBACK.                                         
       CLEAR-SCREEN-PARA.                                   
            MOVE 'D' TO WHAT-MAP-FLAG                       
            MOVE '____________________' TO NAMEO             
            MOVE '____________________' TO SURNO             
            MOVE '__________' TO DATEO                       
            MOVE '___________' TO PESELO                     
            PERFORM SEND-THE-MAP-PARA                        
            EXIT.                                            
       RECEIVE-DATA-FROM-SCRREN.                             
            MOVE LOW-VALUES TO MAP1I                         
            EXEC CICS                                        
            RECEIVE MAP('MAP1') MAPSET('USER')               
            INTO(MAP1I)                                      
            RESP(RESPCODE)                                   
            END-EXEC                                         
            EVALUATE RESPCODE                                
            WHEN DFHRESP(NORMAL)                             
                MOVE 'Y' TO IF-RECEIVE-SUCCESSFULL           
                MOVE NAMEI TO WS-NAME                        
                MOVE SURNI TO WS-SURN                            
                MOVE DATEI TO WS-DATE                            
                MOVE PESELI TO WS-PESEL                          
            WHEN DFHRESP(MAPFAIL)                                
                MOVE MAPFAIL-ERROR-MSG TO MSGO                   
                MOVE 'D' TO WHAT-MAP-FLAG                        
                PERFORM SEND-THE-MAP-PARA                        
            WHEN OTHER                                           
               PERFORM ABNORMAL-EXIT-PARA                        
            END-EVALUATE                                         
            EXIT.                                                
       PROCESS-DATA-PARA.                                        
            PERFORM RECEIVE-DATA-FROM-SCRREN                     
             IF RECEIVE-SUCCESSFULL THEN                         
      * WE NEED TO CHECK IF INPUT DATA IS CORRECT                
      * AND ALSO WE NEED TO GET RID OF '_' UNDERSCORES           
              INSPECT WS-NAME REPLACING ALL '_' BY ' '           
              INSPECT WS-SURN REPLACING ALL '_' BY ' '           
              MOVE 'Y' TO PESEL-STATUS                           
              MOVE WS-PESEL TO PESELCK-PESEL                     
              EXEC CICS                                          
              LINK PROGRAM('PESELCK') COMMAREA(PESELCK-AREA)     
              END-EXEC                                           
                  IF PESEL-STATUS = 'Y' THEN                     
               MOVE 'Y' TO DATE-STATUS                           
               MOVE WS-DATE TO DATECK-DATE                       
               EXEC CICS                                         
               LINK PROGRAM('DATECK2') COMMAREA(DATECK-AREA)     
               END-EXEC                                          
                      IF DATE-STATUS = 'Y'                       
      * ALL DATA IS SUCCESSFULL                                  
                      PERFORM SAVE-DATA-TO-VSAM                  
                      ELSE                                       
      *    INVALID DATE                                          
                        MOVE INVALID-DATE-MSG TO MSGO          
                        MOVE 'D' TO WHAT-MAP-FLAG              
                        PERFORM SEND-THE-MAP-PARA              
                      END-IF                                   
                  ELSE                                         
      * INVALID PESEL                                          
                    MOVE INVALID-PESEL-MSG TO MSGO             
                    MOVE 'D' TO WHAT-MAP-FLAG                  
                    PERFORM SEND-THE-MAP-PARA                  
                  END-IF                                       
              ELSE                                             
      * RECEIVE NOT SUCCESFULL                                 
                 MOVE OTHER-ERROR-MSG TO MSGO                  
                   MOVE 'D' TO WHAT-MAP-FLAG                   
                 PERFORM SEND-THE-MAP-PARA                     
             END-IF.                                           
             EXIT.                                             
        ABNORMAL-EXIT-PARA.                              
            EXEC CICS                                    
            SEND TEXT FROM(ABNORMAL-EXIT-MSG)            
            ERASE                                        
            END-EXEC                                     
            EXEC CICS                                    
            RETURN                                       
            END-EXEC                                     
            GOBACK.                                      
       SAVE-DATA-TO-VSAM.                                
            MOVE WS-DATE TO FS-DATE                      
            MOVE WS-NAME TO FS-NAME                      
            MOVE WS-SURN TO FS-SURN                      
            MOVE WS-PESEL TO FS-PESEL                    
            EXEC CICS                                    
            WRITE FILE('FUSER')                          
            FROM(FS-FUSER)                               
            RIDFLD(FS-PESEL)                              
            RESP(RESPCODE)                                
            END-EXEC                                      
            EVALUATE RESPCODE                             
            WHEN DFHRESP(NORMAL)                          
               MOVE SUCCESS-MSG TO MSGO                   
            WHEN DFHRESP(DUPKEY)                          
               MOVE DUPKEY-ERROR-MSG TO MSGO              
            WHEN OTHER                                    
               MOVE OTHER-ERROR-MSG TO MSGO               
            END-EVALUATE                                  
               MOVE 'D' TO WHAT-MAP-FLAG                  
               PERFORM SEND-THE-MAP-PARA                  
            EXIT.                                         