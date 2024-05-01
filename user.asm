USER   DFHMSD TYPE=&SYSPARM,LANG=COBOL,MODE=INOUT,TERM=ALL,            X
               STORAGE=AUTO,DSATTS=COLOR,MAPATTS=COLOR,TIOAPFX=YES,    X
               CTRL=(FREEKB,FRSET)                                      
MAP1   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1                              
       DFHMDF POS=(1,1),ATTRB=ASKIP,LENGTH=23,                         X
               INITIAL='WELCOME TO THIS PROGRAM'                        
       DFHMDF POS=(2,1),ATTRB=ASKIP,INITIAL='NAME: ',LENGTH=6           
NAME   DFHMDF POS=(2,8),ATTRB=(IC,UNPROT),                             X
               INITIAL='____________________',                         X
               LENGTH=20                                                
       DFHMDF POS=(2,29),ATTRB=ASKIP,INITIAL=' ',LENGTH=1               
       DFHMDF POS=(3,1),ATTRB=ASKIP,INITIAL='SURN: ',LENGTH=6           
SURN   DFHMDF POS=(3,8),ATTRB=UNPROT,INITIAL='____________________',   X
               LENGTH=20                                                
       DFHMDF POS=(3,29),ATTRB=ASKIP,INITIAL=' ',LENGTH=1               
       DFHMDF POS=(4,1),ATTRB=ASKIP,INITIAL='PESEL: ',LENGTH=7          
PESEL  DFHMDF POS=(4,9),ATTRB=(NUM,UNPROT),INITIAL='___________',      X
               LENGTH=11                                                
       DFHMDF POS=(4,21),ATTRB=ASKIP,INITIAL=' ',LENGTH=1               
       DFHMDF POS=(5,1),ATTRB=ASKIP,INITIAL='DATE OF B.: ',LENGTH=13    
DATE   DFHMDF POS=(5,15),ATTRB=UNPROT,INITIAL='__________',LENGTH=10    
       DFHMDF POS=(5,26),ATTRB=ASKIP,LENGTH=20,                        X
               INITIAL='IN FORMAT DD-MM-YYYY'                           
MSG    DFHMDF POS=(7,1),ATTRB=ASKIP,INITIAL=' ',LENGTH=79               
       DFHMDF POS=(10,1),ATTRB=ASKIP,INITIAL='PRESS F1 TO CLEAR',      X
               LENGTH=17                                                
       DFHMDF POS=(10,19),ATTRB=ASKIP,INITIAL='PRESS F3 TO EXIT',      X
               LENGTH=16                                                
       DFHMDF POS=(10,36),ATTRB=ASKIP,INITIAL='PRES ENTER TO PROCEDE', X
               LENGTH=21                                                
       DFHMSD TYPE=FINAL                                                
       END                                                              