//
*+---------------------------------------------------------------------------------------------------------+
*| `                                                                                                       |
*|        Program: HBR810Addr.PRG                                                                          |
*|        CALLED BY start menu HBR102menu from HBR101Start                                                 |
*|         System: HARBOUR STARTER CODE SAMPLE  NAME AND ADDRESS & SIMPLE TICKET SAMPLE PROGRAM CODE       |         
*| Copyright© W.Greenwood England 1983-2024                                                                |
*| This free software is released under the MIT License.                                                   |
*| See https://en.wikipedia.org/wiki/MIT_License                                                           |  
*|  Last modified: 5/5/24 :                                                                                |
*| This is a simple name & address screen. It does not include some of the necessary fields that a         |
*| commercial would need. However it should provide enough to show the way forward and includes various    |
*| advice from my eperience in developing a range of applications for commerce and industry. This software |
*| has been updated from Clipper87 into Harbour and has not been mouse enabled.                            |
*| You are free to amend and use this software in any way as you see fit. Except to copy, publish, part or |
*| all and claiming it to be your own copyright. This software is free of charge and license fees periodic |
*| or otherwise.  It is my intention to convert this example into a graphical interface using several of   |
*| the GUI software applications available for Harbour. There is no warranty offered or implied            |                                                  |
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
DBCloseAll()            // just to make sure    See Harbour Language Reference                    **
mExclOrNOt := .F.       //                                                                        **
DATAOPEN(mEXclOrNot)    // Fn() in 101Start                                                       **
SETINDEXES()            // Fn() in 101Start                                                       **
SELE 81                 // for this .prg only one table reqd                                      **
// *************************************************************************************************
//  For use in DBEDIT()
   A_F81[1] := "NAME"  
   A_F81[2] := "ADDR1"  // 
   A_F81[3] := "PCODE"  // 
   A_F81[4] := "TEL"    // 
   A_F81[5] := "EMAIL"  // 
   A_F81[6] := "CONTACT"  
   //
   A_F81h[1] := "  Name"
   A_F81h[2] := "  Address "
   A_F81h[3] := "  Postcode"
   A_F81h[4] := "  Telephone"
   A_F81h[5] := "  Email"
   A_F81h[6] := "  Contact"
//
//
GO TOP
*
*  --------------------------------- FILES OPENED
*
PRGCOLOR := cWhMagenta // 
SETCOLOR(PRGCOLOR)
CLEAR SCREEN
*****************************
//
SELE 99
IF F99->CODEPRFX  <2    // cannot have a prefix less than 2
    mCODEPRFX := 4      // you can change this to suit yourself (declared in 101) 
ELSE
    mCODEPRFX := CODEPRFX
ENDIF
SELE 81
//
@ 00,00 CLEAR TO 00,79
LEGEND(00," CUSTOMER FILE ")
@ 24,02 say Chr(27)+Chr(18)+Chr(26)+"   Delete-   Edit   Import/export   New+   Print   View        QUIT"
//       use ^ this in example
//--------------------------------------------for my programs-----
mLC = 5   // related to screen size
mRC = 70  // ditto
*****************************
//------- ACCOUNT AND CLIENT CODES --------------------------------------------------------------------------------------------------------------- read
// Following is an explanation of the customer/client code generation, consisting of an alpha prefix and a numeric suffix. If you prefer just 
// to have an alternative code entry, you will need to amend the section where a new customer is added. I have used the Prefix-Suffix generator 
// as an example of how you can control the codes used. Varying lengths of code can result in customers apparently shifting out of order.
//                                              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// The Client Codes here are used to sort the table in the alpha order of the Customer/Client Name. The code is 8 alpha/numeric characters long, 
// with the alpha section uppercase. The Codes are unique by client. The codes are made up of four Alpha and four numbers. The user will enter 
// with the alpha section uppercase. The Codes are unique by client. The codes are made up of four Alpha and four numbers. The user will enter 
// the four alpha characters and the system will add the number as follows: AAAA0001. In this example program you will avoid a duplicate codes.
// The codes are made up of four Alpha and four numbers. The user will enter the four alpha characters and the system will add the number as 
// follows: AAAA0001. In this example program you will avoid a duplicate codes. The numeric suffix allows up to 9,999. Clearly some companies 
// will have 10,000 clients but many more will not. If you needed a higher number then you can reduce the alpha prefix to three, allowing 99,999. 
// You could extend the number system by spliiting the alphabet into 3, 4, 5, or 6 with 9,999 to each, gives a possible 30,000 to 60,000 codes. 
// Of course, it is dependant on your user's market but I suspect that many will do well if they have 1,000 differemt clients over thirty years.
// Reducing the prefix to two will allow 999,999 or 1 million before you run out of numbers. I have always used eight because SAGE has always 
// used eight without any problems. As an example, if you had "The Granite Company" and "Granite Worktops" it is helpful if they are both in 
// the "G"s. You wouldn't expect one to be "T" for "The Granit ". In this system you would enter "GRAN" and the system will add (say) "0019". 
// For the second you would add "GRAN" again and the suffix (say) "0020" is added. Of course, if this method is not to your liking then you can 
// of course amend it to suit your own development methods. Try not to vary the prefix length, otherwise codes may appear to be out of order. 
// It is my opinion that codes should be the same length wherever practical.
// As stated this is a simplified system, with new users to Harbour in mind. There does appear to be few examples out there so this is my tiny 
// attempt to rectify that! In a commercial package you will probably need to export data for importing into an accounting system. In that case  
// you will need to have the accounts system codes in your system, which may well not be the same as in yours. Additionally, you may need more 
// accounts than the accounts department will require. For instance, there is a client with depots, or regional offices. If there are four depots, 
// each will require an account here but will only have one in the accounts system. You will create and issue invoices for the depots, which they 
// will approve. The invoices for each will be posted to your accounts each with the same accounts code. When they send out statements, all of the 
// charges for the depots will appear on the statement. Of course it may be that the client codes in accounts are satisfactory but you will 
// probably need additional accounts in your system. Accessing clients in alpha order is easier when sorting and filtering. You will no doubt 
// need more fields than I have included in the tables. As time passes accounts may not be used. You cannot delete them a you will leave orphaned 
// records. Instead have a HIDE field so you can filter out no longer needed accounts. Also from time to time some clients may be on-stop as they 
// have overdue accounts. You may need a field for ONSTOP to cover this. There can be other settings, discounts etc, that you may need.
// To transfer invoices, or charges to accounts I advise that you create an export file that is in the correct format for the accounts system. 
// You export your valid data that the accounts system will validate and then import. In that way you do not have any liability for any issues 
// that may arise. If you post directly into an accounts system and any issues arise, you will bein the unenviable situation of proving that you are not
// responsible. No doubt that you will not have access to any technical information on earlier operations and the accounts software company presumably 
// may be unhelpful. The import means this situation will never happen. If there are any errors in your export file, the accounts system will not 
// perform the import. Or it will exclude any incorrect items. Exporting ensures that you are protected from any issues arising. 
// ------------------------------------------------------------------------------------------------------------------------------- END CODES ETC----------
//
@ 02,05 to 18,75 DOUBLE 
@ 04-1,10 SAY ' CLIENT CODE '     
@ 07-1,10 SAY 'COMPANY NAME '
@ 08-1,10 SAY '   Address 1 '  
@ 09-1,10 SAY '           2 '    // not essential for @ 09-1, 10 but just showing you can set Column and Rows with a calculation as well as literals or variables 
@ 10-1,10 SAY '           3 '    // It can be handy to use calcs as you can easily move fields up or down a screen without heving to amend every line
@ 11-1,10 SAY '           4 '    // Also useful if new fields need to be inserted
@ 12-1,10 SAY '    POSTCODE '
@ 13-1,10 SAY '   TELEPHONE '
@ 14-1,10 Say "       EMAIL "
@ 15-1,10 Say "CONTACT NAME "
@ 16-1,10 Say "      NOTES "
**

SETCOLOR(PRGCOLOR)
*      +12 for gets
//
M_RESP2 := " "
DO SAY810
RESPTEMP := " "
// Remove Ucase1st() 
DO WHILE AT(M_RESP2,"Q0") = 0
    // @23,10 say mF81
    *@ 23,76 SAY MEMORY(0) PICT "999"
    M_RESP2 := " "
    ARROWON2()
    *
    @ 24,78 GET M_RESP2 PICT "!"
    READ
    CLEA GETS
    ARROWOFF()
    IF M_RESP2 = "?"
        // Open your Help file
    ENDIF
    IF M_RESP2 = "+"
        M_RESP2 := "N"
    ENDIF
    IF M_RESP2 = "-"
        M_RESP2 := "D"
    ENDIF
    *
    DO CASE
    CASE M_RESP2 = "A"  && ALTER
        *  alter /edit
        M_FIND := SUBSTR(CUSTID+SPACE(LEN(custid)),1,LEN(custid))
    CASE M_RESP2 = "B"
    CASE M_RESP2 = "C"   && skipCode
        **
        **/ ====================================================
    CASE M_RESP2 = "D"     &&   delete
        //@ 20,10 say"D"
        //
        // Deletes  require every file to be checked that uses a record in name & address 
        // You cannot delete any transactions that have a link, otherwise you will have 
        // orphaned records that cannot be recovered
        TEMPSCR := SAVESCREEN(20,0,21,79)
        POS     := RECNO()
        OK1     := 0
        OK2DELE := 0
        ERRMSG1(20,33,"DELETE NOT IN USE", " ** DELETES NOT ENABLED ** ")
        INKEY(0)
        RESTSCREEN(20,0,21,79, TEMPSCR)
        //
        //
      
        SETCOLOR(prgcolor)
        //RESTSCREEN(0,0,24,79,FIRST)
        RELE FIRST
        *
        //
    CASE M_RESP2 = "E"   // edit
        //
        DO GET810     // In 810
        //
    CASE M_RESP2 = "F"
        * locate
        M_CUSTID = SPACE(8)
        REC := RECNO()
        SET EXACT OFF
        //
        @ 04-1,25 GET M_CUSTID PICT "!!!!!!!!" VALID (.NOT. EMPTY(M_CUSTID))
        READ
        CLEA GETS
        M_FIND := TRIM(M_CUSTID)
        SEEK M_FIND
        IF .NOT. FOUND()
            GO REC
            FIRST := SAVESCREEN(0,0,24,79)
            @ 12,40 CLEA TO 16,60
            SETCOLOR(cBlkYellow)  && 
            @ 14,42 SAY "NOT FOUND"
            INKEY(5)
            //
            SETCOLOR(PRGCOLOR)
            RESTSCREEN(0,0,24,79,FIRST)
            RELE FIRST
        ELSE
            SETCOLOR(PRGCOLOR)
            //#DO SAY210
        ENDIF
        SET EXAC ON
    CASE M_RESP2 = "I"   && Import/Export
        TEMPSCR := SAVESCREEN(20,0,21,79)
        POS     := RECNO()
        OK1     := 0
        OK2DELE := 0
        ERRMSG1(20,12,"IMPORT/EXPORT OPTION TO RECEIVE/SEND DATA TO ACCOUNTS", " ** MAYBE INCLUDE THIS IN THE PRINT OPTIONS SCREEN? ** ")
        INKEY(0)
        RESTSCREEN(20,0,21,79, TEMPSCR)
      
    CASE AT(M_RESP2,"N+") >0    && new
        * code add
        /// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% modify to exclude account code & remove postcode lookup %%%%%%
        // you could set this into a DO WHILE which doesn't exit whilst an invalid entry is present
        // Here, if the user enters a shorter code than is valid, they need to repeat with N/+ to redo
        // Personally, I would normally do that but this works Ok
        //----------------------------------------------------------------------------------------------
        NEWSCR = SAVESCREEN(0,0,24,79)
        mCodeSuffix :="0000"
        @ 19,48 SAY "EXIT WITH  00  FOR CODE"
        M_RESP2  := "A"
        M_CUSTID := Space(8)
        M_OK     := "Y"
        //
        SET EXACT ON
        @ 18,03 to 23,77
        @ 19,6 Say "NEW CLIENT CODE"
        **@ 20,10 Say " NEW ALPHA CODE"
        @ 22,10 Say "SAVE CODE Y/N/Q"
        @ 19,25 GET M_CUSTID PICT "!!!!"    VALID (.NOT. EMPTY(M_CUSTID))     // set as max 4 for prefix if you retain this code system
        @ 22,28 Get M_OK     PICT "!"
        READ
        M_CUSTID := ALLTRIM(M_CUSTID)
        CLEA GETS
        If M_OK = "Y" 
            IF SUBSTR(M_CUSTID,1,1) = "0" .OR. EMPTY(M_CUSTID)
                * nothing
                M_CUSTID := CUSTID
                @ 02-1,25 GET M_CUSTID
                M_RESP2 = " "
                CLEA GETS
            ELSE
                IF LEN(M_CUSTID) < mCODEPRFX     // Too Short
                    //error
                    TEMPSCR := SAVESCREEN(20,0,21,79)
                    ERRMSG1(20,33,"ERROR! CODE PREFIX IS TOO SHORT", "The Code Alpha Prefix should be " +AllTrim(STR(mCODEPRFX)) + " Long")
                    INKEY(0)
                    RESTSCREEN(20,0,21,79, TEMPSCR)
                    // 
                ELSE
                    SELE 99  // SysMaster
                    GO 1
                    mCodeSuffix := RIGHT(STR(CODESUFX),(8-mCODEPRFX))       // if you change the CustID field size from 8 you will need to amend this line to suit
                    M_CUSTID = ALLTRIM(M_CUSTID) + mCodeSuffix
                    SELE 81    // 81addr
                    M_RECNO := RECNO()
                    @ 20,10 Say Space(20)
                    @ 20,06 Say "NEW CODE WILL BE : " + M_CUSTID
                    M_FIND = SUBSTR(M_CUSTID+SPACE(9),1,8)                  // Making sure search is on 8 characters  - ditto as above
                    SEEK M_FIND
                    //
                    IF .NOT. FOUND()
                        @ 22,30 SAY "ADD CODE FINAL OPTION   Y/N/Q " 
                        @ 22,61 GET M_OK PICT "!"
                        READ 
                        IF M_OK  = "Y" 
                            IF ADD_REC(0)
                                REPL CUSTID WITH M_CUSTID
                            ENDIF
                            UNLOCK
                            SELE 99     // need to update System Master with next Code Suffix
                            If REC_LOCK(0)
                                REPLACE CODESUFX with (CODESUFX +1)
                            Endif
                            SELE 81     // back to 81addr
                        ELSE 
                            SELE 81
                            GO M_RECNO
                        ENDIF      
                    ELSE
                        //
                        FIRST := SAVESCREEN(0,0,24,79)
                        @ 12,40 CLEA TO 16,60
                        SETCOLOR(cBlkYellow)
                        @ 14,42 SAY "DUPLICATE CODE ERROR"
                        INKEY(5)
                        SETCOLOR(PRGCOLOR)
                        RESTSCREEN(0,0,24,79,FIRST)
                        RELE FIRST
                        M_RESP2 := ' '
                    ENDIF     // !found
                ENDIF   // Len cusid
            ENDIF  // Ok=Y
            @ 19,10 say Space(40)
            @ 20,10 Say Space(40)
            @ 22,10 Say Space(40)
            M_RESP2 = " "
        ENDIF
      *@ 23,35 say SPACE(32)
      *@ 23,35 SAY REPLICATE(CHR(196),32)
      SETCOLOR(prgcolor)
      RESTSCREEN(0,0,24,79,NEWSCR)
      RELE NEWSCR
      /// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% one code only %%%%%%%%%%%%%%%%%
    CASE M_RESP2 = "P"   && print
        TEMPSCR := SAVESCREEN(20,0,21,79)
        POS     := RECNO()
        OK1     := 0
        OK2DELE := 0
        ERRMSG1(20,02,"YOUR STANDARD PRINT OPTIONS SCREEN CALLED HERE", "Options to Printer, PDF, EMAIL, maybe Import/Export via CSV or Textfile")
        INKEY(0)
        RESTSCREEN(20,0,21,79, TEMPSCR)
        /// %%%%%%%%%%%%%%%%%%% your std print routine %%%%%%%%%%%%%%%%%%%
        /// maybe a menu with print options %%%%%%%%%%%%%%%%%%%%%%%%%%%% 
      
    CASE M_RESP2 = "V"
        // Added an option menu here. Not actually necessary but put in as an example
        FIRST := SAVESCREEN(0,0,24,79)    
        OK1 := WHICH1(13,33,"SELECT FILE TO VIEW:Abandon View:View Address File:")     // Summary Details:Filter Address File:")
        DO CASE
        CASE OK1 = 1
            SETCOLOR(PRGCOLOR)
            RESTSCREEN(0,0,79,FIRST)
        CASE OK1 = 2
            ///
           
            FIRST := SAVESCREEN(0,0,24,79)              //                                          -                           
            // *************************************************
            Setmode(43,132)   // see Harbour Language Reference
            @ 42,10 SAY "TICKET FILE - [PageUp/Down] and <Arrow> keys or Press [Esc] / [Enter] to Exit & Select"
            // *************************************************
            SETCOLOR(cWhiteScr1)  && DBWHITE)
            //
            DBEDIT(01,01,40,128,A_F81,.T.,.T.,A_F81h)   // For the larger screen set 
            //   I have used DBEDIT() as it is quick and easy to use. At one point it was to becombe unsupported but that hasn't happened as yet (I think)     
            SETMODE(25,80)
            SETCOLOR(PRGCOLOR)
            SETMODE(25,80)
            //
            RESTSCREEN(0,0,24,79,FIRST)
            RELE FIRST
        //CASE OK1 = 3 && 
        //   You could have additional options to filter or view your data here
        //CASE OK1 = 4
        //                ditto as above
        ENDCASE
        Legend(00,Space(74))
        LEGEND(00," CUSTOMER FILE ")
    CASE M_RESP2 = "S"  && 
        // SELE
    CASE M_RESP2 = "W"  && 
        //
    ENDCASE
    //
    DO SAY810   // refresh if edited record
ENDDO
Setcolor(cFrontScrn)
CLOSE DATA
SELE 1
//#RELE M_RESP2,M_ST1,M_ST2,M_ST3,M_ST4,M_ST5,M_ST6,M_ST7,M_ST8,M_ST9,OK2DELE,REC
//#RELE M_CUSTID,M_ALPHA,M_KEY,PFIRST,M_AC,T_SKEY,M_ADDR1,M_ADDR2,M_ADDR3,M_ADDR4,M_ZIP,M_TEL,M_FAX,M_MERGEM_CONTACT,M_HIRE
//#RELE M_SK1,M_SK2,M_SK3,M_SK4,M_SK5,M_SK6,M_SK7,M_SK8,M_SK9,M_MONEY,M_SFILE,M_ROW,M_MXBAL,M_WM1,M_WM2,M_WM3,M_WM4,M_WM5,M_WM6,M_WM7,M_WM8,M_WM9
SETCOLOR(cFrontScrn)
//
RETURN      && HBR810Addr
//
*
*!*********************************************************************
*!
*!      Procedure: SAY810
*!
*!      Called by: S4910.PRG
*!
*!*********************************************************************
PROCEDURE SAY810    // 
   // in correct Work Area 
   // 
   SETCOLOR(PRGCOLOR)
   @ 04-1,25 SAY CUSTID
   //
   @ 07-1,25 SAY NAME
   @ 08-1,25 SAY ADDR1
   @ 09-1,25 SAY ADDR2
   @ 10-1,25 SAY ADDR3
   @ 11-1,25 SAY ADDR4
   @ 12-1,25 SAY PCODE
   @ 13-1,25 SAY TEL
   @ 14-1,25 SAY EMAIL
   @ 15-1,25 SAY contact
   @ 16-1,25 SAY NOTES
   // 
   
 ****-------------------------| FILE SENSITIVE |--------
 **
 //
 *
RETURN  && SAY810
*|******************************************************
*|   PROCEDURE GET810
*|******************************************************
//
PROCEDURE GET810
   @ 12-1,58-13 SAY SPACE(8)
   // ------------------------------------------------------------------------------------------------------------------------------------------------------
      // I have used M_ prefix to indicate a memory variable. You could have just m or mem or whatever. 
      // Purists reckon using M_or m prefixes not to be considered, sometimes even irritating to them
      // Personally I find it useful. Other systems can have all sorts of variables and you need to know the differences
      // For instance I can have a variable on a screen that has been sourced from a table and used along with a memory variable
      // Address1 in a table could be on a screen as sAddress1 and as a memory variable as mAddress1 and you are able to compare them 
      // or make a decision after a change. But as a developer you may choose which you prefer and if it suits you then that is OK
      // Additionally I can have a CustID variable in several Tables. For example; M_81CustID and M_20CustID indicates memory variables from different Tables
      // In this example I have used the M_ prefix as it is very obvious what it is with little explanation.
      // If you are developing a project with others, then you will need an agreed standard for all to comply with of course. Your software, your choice.
      // --------------------------------------------------------------------------------------------------------------------------------------------------------
      M_CUSTID  := CUSTID
      M_NAME    := NAME    
      M_ADDR1   := ADDR1
      M_ADDR2   := ADDR2
      M_ADDR3   := ADDR3
      M_ADDR4   := ADDR4
      M_PCODE   := PCODE
      M_TEL     := TEL
      M_EMAIL   := EMAIL      
      M_CONTACT := CONTACT
      M_NOTES   := NOTES
      //*    =========================================================
      M_YN := "Y"
      //M_OK := 0
      @ 04-1,25 SAY CUSTID
    
      @ 07-1,25 GET M_NAME
      @ 08-1,25 GET M_ADDR1
      @ 09-1,25 GET M_ADDR2
      @ 10-1,25 GET M_ADDR3
      @ 11-1,25 GET M_ADDR4
      @ 12-1,25 GET M_PCODE PICT "!!!!!!!!!!"
      @ 13-1,25 GET M_TEL
      @ 14-1,25 GET M_EMAIL   
      @ 15-1,25 GET M_CONTACT
      @ 16-1,25 GET M_NOTES
      READ
      @ 19,10 Say "SAVE CHANGES Y/N/Q " 
      @ 19,30 Get M_YN Pict "!"     // Here all the user needs to to is press enter to accept the "Y" the space, or just press any other key to change
      READ
      // Instead of the second READ you could have a standard popup prompt menu with two options : Continue & Abandon
      // probably a tidier solution but not included in this example
      If M_YN = "Y" 
         IF REC_LOCK(0)
            REPL NAME WITH M_NAME,ADDR1 WITH M_ADDR1,ADDR2 WITH M_ADDR2,ADDR3 WITH M_ADDR3,ADDR4 WITH M_ADDR4,PCODE WITH M_PCODE,TEL WITH M_TEL,EMAIL WITH M_EMAIL,CONTACT WITH M_CONTACT, NOTES WITH M_NOTES
            COMMIT
         ENDIF
      ENDIF
      @ 19,10 say Space(29)   // clear Save changes prompt
      //
      UNLOCK   //  Just in case!!
      DO SAY810  // this will show the changes or show the original record depening upon M_YN
   //
   //
RETURN  // Proc GET810 
******
**
**                   *************************
*                    *       HBR810Addr      *
*                    *************************
//
//
// EOF: HBR810Addr.PRG
//\\

