//
*+---------------------------------------------------------------------------------------------------------+
*| `                                                                                                       |
*|        Program: HBR910Rein.PRG         (REINDEX)                                                        |    
*|        CALLED BY start menu HBR102menu UTILITIES                                                        |
*|        System: HARBOUR STARTER CODE SAMPLE  NAME AND ADDRESS & SIMPLE TICKET SAMPLE PROGRAM CODE        |         
*| Copyright© W.Greenwood England 1983-2024                                                                |
*| This free software is released under the MIT License.                                                   |
*| See https://en.wikipedia.org/wiki/MIT_License                                                           |  
*|  Last modified: 6/5/24 :                                                                                |
*| This is a simple reindexing options screen for Utilities allowing the user to keep their index files    |
*| up to date. In the event of corruption, or where date is out of order they should reindex initially.    |                        
*+---------------------------------------------------------------------------------------------------------+
*| There isn't any Error Checking in Functions/Procs ot test for invalid user entry. If required then you  |
*| need to add it yourself. Normally I include Error tests but I thought you'd like to add your own!       |
*+---------------------------------------------------------------------------------------------------------+
//
CLOSE ALL
SET BELL OFF	// just to make sure the irritating beep is off
SET DATE BRITISH	// check out all of the date commands & functions : SET DATE, CSETDATE(), CDATEFORMAT() DATE() etc
SET DELE ON		// just to make sure deleted records are hidden
SET MESS TO 23
SET SCOR OFF	// just to make sure
SET TALK OFF	// just to make sure
SET WRAP ON
SET KEY -7 TO            && EDIT standing data
SET KEY -8 TO            && edit pds & dates
SET KEY 24 TO skipn      && moves to next #, program needs says following case
SET KEY  4 TO skipn      &&
SET KEY  5 TO skipb      &&
SET KEY 19 TO skipb      && next1 & back1 in locks
M_SAY = "SAY210"         && set to say proc. used in procs.next1 & back1 in locks
M_SAYDO =.T.
* ---------------------------------- OPEN FILES HERE ***********************************************
DBCloseAll()   // just to make sure    See Harbour Language Reference                             **
mExclOrNOt := .T.        // for reindex maybe preferred to open in EXCLUSIVE MODE                 **
**** DATAOPEN(mEXclOrNot)     //  in 101Start                                                          **
// *** DO NOT OPEN INDEXES HERE ****** SETINDEXES()   // in 101Start                              **
// *************************************************************************************************
*
*  --------------------------------- FILES OPENED PRIOR TO REINDEXING
*
PRGCOLOR := cWhiteScr1  // cCyanScrn1 // 
SETCOLOR(PRGCOLOR)
CLEAR SCREEN
*****************************
//
//
WBOX()
LEGENDBX(03," BACKUP & REINDEX DATAFILES ","S")

@ 23,08 say                         "Backup Datefiles         Reindex Data                          QUIT"
//       use ^ this in example
//--------------------------------------------for my programs-----
mLC = 5   // related to screen size
mRC = 70  // ditto
*****************************
//
//
M_RESP2 = " "

RESPTEMP = " "
DO WHILE AT(M_RESP2,"Q0") = 0
    // @23,10 say mF81
    *@ 23,76 SAY MEMORY(0) PICT "999"
    M_RESP2 = " "
    ARROWON2()
    *M_RESP2 =0
    @ 23,78 GET M_RESP2 PICT "!"
    READ
    CLEA GETS
    ARROWOFF()
    IF M_RESP2 = "?"
        // Open your Help file
    ENDIF
    /*
    IF M_RESP2 = "+"   NOT REQD
        M_RESP2 := "N"
    ENDIF
    IF M_RESP2 = "-"
        M_RESP2 := "D"
    ENDIF
    */ 
    DO CASE
    CASE M_RESP2 = "A"  && ALTER
        //
    CASE M_RESP2 = "B"  // Backup
        DO BackUp910    // in 910
    CASE M_RESP2 = "C"   && 
        **
    CASE M_RESP2 = "D"     &&   delete
        //
    CASE M_RESP2 = "E"   // edit
        //
    CASE M_RESP2 = "F"
        **
    CASE M_RESP2 = "I"   && Import/Export
        **
    CASE AT(M_RESP2,"N+") >0    && new
        **
    CASE M_RESP2 = "P"   && print
        **
    CASE M_RESP2 = "R"   && REINDEX   
        DO Reindex910   // in 910
    CASE M_RESP2 = "V"
        **
    CASE M_RESP2 = "S"  && 
        // 
    CASE M_RESP2 = "W"  && 
        //
    ENDCASE
   //
   //
ENDDO
Setcolor(cFrontScrn)
DBCLOSEALL()        // Harbour Fn
//
//
*
*!*********************************************************************
*!
*!      Procedure: BACKUP910
*!
*!     
*!*********************************************************************
PROCEDURE BACKUP910    // 
   TEMPSCR := SAVESCREEN(20,0,21,79)
   OK1 = 0
   ERRMSG1(20,12,"BACKUP ROUTINE NOT INCLUDED IN THIS ISSUE", " ** ADD YOUR OWN BACKUP PROCEDURE HERE ** ")
        INKEY(0)
        RESTSCREEN(20,0,21,79, TEMPSCR)
 *
RETURN  && BACKUP910
//
*|******************************************************
*|   PROCEDURE REINDEX910
*|******************************************************
//
PROCEDURE REINDEX910
    PRIVATE mOK 
    mOK := " "
    @ 09,10 Say "CONTINUE WITH REINDEXING DATAFILES  Y/N " 
    @ 09,51 Get mOK Pict "!"  
    READ
   
    IF mOK = "Y"
        @ 12,10 SAY "STARTING REINDEX " 
        @ 12,28 say TIME()
        DBCloseAll()   // just to make sure    See Harbour Language Reference
        mExclOrNot := .T.
        DATAOPEN(mExclOrNot)     //  in 101Start
        //
        PRIVATE mIndexFiles
        SELE 81
        
        mIndexFiles := "HBR81I1.NTX"
        INDEX ON CUSTID to HBR81I1
        SELE 11
        INDEX ON TICKET TO HBR11i1.NTX
        DBCLOSEALL()
       
        @ 16,10 SAY "INDEXING COMPLETE "
        @ 16,28 say TIME()
        @ 17,28 Say "Press any key.."
        INKEY(0)
        @ 05,09 CLEAR TO 17,70
    ELSE
        ERRMSG1(12,42,"REINDEX ABANDONDED", " ** Press any key ** ")
        INKEY(0) 
        @ 09,10 say Space(45)
        @ 12,42 say Space(22)
        @ 13,42 say Space(22)
    ENDIF
   //
RETURN  // Proc REINDEX910 
******
**
**                   *************************
*                    *       HBR910Rein      *
*                    *************************
//
//
// EOF: HBR910Rein.PRG
//\\

