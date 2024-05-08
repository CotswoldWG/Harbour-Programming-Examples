*+-----------------------------------------------------------------------------------------------------------+
*| `                                                                                                         |
*|         Program: HBR110Ticket.PRG                                                                         |
*|         CALLED BY start menu HBR102menu from HBR101Start                                                  |
*|         System: HARBOUR STARTER CODE SAMPLE  NAME AND ADDRESS & SIMPLE TICKET SAMPLE PROGRAM CODE         |   
*|         Copyright© W.Greenwood England 1983-2024                                                          |                                                     
*|         This is free software is released under the MIT License.                                          |
*|         See https://en.wikipedia.org/wiki/MIT_License                                                     |     
*|  Last modified: 30/4/24 :                                                                                 |
*| This is a simple ticket entry screen. It does not have the detail to be a commercial application but is   |
*| just a basic example showing one method of data entry complete with an automatic lookup for the name      |
*| and address. Plus a DBEDIT() view of the ticket data that allows you to change the ticket on-screen.      |
*| It also shows one method of providing an easy to use menu system, activated by a single key entry.        |
*| It should be understood that this example has not been mouse-enabled (which it could be, see Harbour      |
*| Lamguage and mouse examples) Control is totally by keyboard. When this screen opens you have access to    |
*| a menu system that is activated by pressing the first letter of the menu word. Also, when you are on      |
*| the ticket screen you can move from record to record by pressing the applicable arrow keys. Up or Left    |
*| to move back up the file and Down, or Right to move to older records. The View option gives a table       |
*| view of all records and again you use the arrow keys to move about and along.You can also use the PageUp  |
*| and PageDown keys to jump a screen of records. To select a record, simply press Enter] on a chosen row.   |
*+-----------------------------------------------------------------------------------------------------------+
*| There isn't any Error Checking in Functions/Procs ot test for invalid user entry. If required then you    |
*| need to add it yourself. Normally I include Error tests but I thought you'd like to add your own!         |
*+-----------------------------------------------------------------------------------------------------------+
//
*** The current advice is to use // to REM a line. I may well use the original * or && from time to time as well!
&&  I also tend to dislike blank lines through the code but that is only my personal preference and not from 
//  my observations, the usual code layout of most developers. ALSO I will often use the old dBASE First Four of 
//  a command rather than the now more used full name i.e. SET DELE ON instead of SET DELETE ON, SELE instead of SELECT
//   
//
CLOSE ALL
//                  // Maybe some of these aren't operational now but never mind it compliles!
SET BELL OFF	    // just to make sure the irritating beep is off
SET DATE BRITISH	// check out all of the date commands & functions : SET DATE, CSETDATE(), CDATEFORMAT() DATE() etc
SET DELE ON		    // just to make sure deleted records are hidden
SET SCOR OFF	    // just to make sure
SET TALK OFF	    // just to make sure
SET WRAP ON
SET KEY -7 TO            && EDIT standing data
SET KEY -8 TO            && edit pds & dates
SET KEY 24 TO skipn      && moves to next #, program needs says following case
SET KEY  4 TO skipn      &&
SET KEY  5 TO skipb      &&
SET KEY 19 TO skipb      && next1 & back1 in locks
M_SAY    := "SAY110"         && set to say proc. used in procs.next1 & back1 in locks
M_SAYDO  :=.T.
PRIVATE mVATR, mVATstr
* ---------------------------------- OPEN FILES HERE -------------------------------------------------------------------
DBCloseAll()   // just to make sure    See Harbour Language Reference
mExclOrNot := .F.
DATAOPEN(mExclOrNot)     //  in 101Start
SETINDEXES()   // Fn() in 101Start
SELE 11        // for this .prg which also uses 81 to lookup customers & 99 to add a new ticket number and the VAT Rate
*-----------------------------------------------------------------------------------------------------------------------
// GET VAT RATE
SELE 99       // System master file has Current VATRATE 
GO TOP
*--------------------------------------------------------------------------------------------------------------------------------
* This is a simple VAT or Tax charge using only one rate, or actually the current rate. Whilst this is adequate for this example.  
* It will not be suitable for a commercial application, which will require a separate VAT Table with multiple VAT/Tax rates and
* their applicable date ranges.  VAT and Tax should be applied to each charge line on an invoice and NOT one calculated charge
* on the invoice total.
*--------------------------------------------------------------------------------------------------------------------------------
mVATR    := VATRATE  // F99->VATRATE
mVATRstr := ALLTRIM(STR(VATRATE))
// or if int(mVATR) <> mVATR; whichever you prefer
If right(mVATRstr,2) = "00"
   mVATRstr := left(mVATRstr,2) +"%"
Else 
   mVatRstr := mVatRstr +"%"
Endif
mVATRstr := "VAT RATE " + AllTRim(Right((Space(12)+mVATRstr),12))
SELE 11   // back to Tickets Table
*************************
//
//
//  For use in DBEDIT() for client lookup
PRIVATE A_F81[6], A_F81h[6], A_F11[6], A_F11h[6]
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
//  For use in DBEDIT() View 11 table
   A_F11[1] := "CUSTID"
   A_F11[2] := "TICKET"
   A_F11[3] := "PRODUCT"
   A_F11[4] := "QUANT"
   A_F11[5] := "RATE"
   A_F11[6] := "NETCALC"
   //
   A_F11h[1] := " Client Code"
   A_F11h[2] := " Ticket "
   A_F11h[3] := " Product Descr"
   A_F11h[4] := " Quant"
   A_F11h[5] := " Rate"
   A_F11h[6] := " Net Goods"
//
GO TOP
*
*  --------------------------------- FILES OPENED EARLIER and SELECT set
*
PRGCOLOR := cCyanScrn1    // cGreyScrn1  // Standard olours set in 101
SETCOLOR(PRGCOLOR)
CLEAR SCREEN
*****************************
//
@ 00,00 CLEAR TO 00,79
LEGEND(00," SIMPLE TICKET FILE ")
@ 24,02 say Chr(27)+Chr(18)+Chr(26)+"  Delete-  Edit   Filter   Import  New+  Print  Sort   View      QUIT"       // The MenuLine with options activated with a single key press (The + and - are also active )
//       use ^ this in example
//--------------------------------------------for my programs-----
mLC := 1    // related to screen size. Can be used to move Says & Gets up and down the screen avoiding line by line changes later as fields may be moved, or new fields are added 
mRC := 70   // ditto
*****************************
// 
@ 03+mLC-1,02 to 16+mLC+1,78 DOUBLE 
@ 04+mLC-1,10 SAY ' CLIENT CODE '   // Account codes are often not related to the client name. By entering your own alpha code 
//                                  // Allows you to sort alphabetically. If practical use all of eight available with 1st 4 alpha of name
//                                  // Often imported records from an accounts system (SAGE) aren't in alpha order. Which can be unhelpful in application
@ 06+mLC,10 SAY 'TICKET NUMBER'     // }
@ 07+mLC,10 SAY " TICKET DATE "     // } use " " or ' ' to enclose a label  you may also use "  '  ' " in expressions or string concatenations 
@ 08+mLC,10 SAY '     PRODUCT '     // The PRODUCT is not usually a field in the ticket table. In a normalised database it would be the PRODUCT CODE, with the description in the PRODUCT table.
@ 09+mLC,10 SAY '    QUANTITY '     // not essential for @ 09-1, 10 but just showing you can set Column and Rows with a calculation as well as literals or variables 
@ 10+mLC,10 SAY '   RATE EACH '     // It can be handy to use calcs as you can easily move fields up or down a screen without heving to amend every line
@ 11+mLC,10 SAY '   NET GOODS '     // Also useful if new fields need to be inserted
@ 12+mLC,10 SAY mVATRstr
@ 13+mLC,10 SAY '         VAT '
@ 14+mLC,10 SAY 'TOTAL CHARGE '
//@ 16+mLC,10 Say "        PAID "
**

SETCOLOR(PRGCOLOR)
*      +12 for gets
M_RESP2 := " "
DO SAY110
RESPTEMP := " "
DO WHILE AT(M_RESP2,"Q0") = 0
    // @23,10 say mF81
    *@ 23,76 SAY MEMORY(0) PICT "999"
    M_RESP2 := " "
    ARROWON2()
    *M_RESP2 =0
    @ 24,78 GET M_RESP2 PICT "!"
    READ
    CLEA GETS
    ARROWOFF()
    IF M_RESP2 = "?"
        // Open your Help file
    ENDIF
    IF M_RESP2 = "+"
        M_RESP2 := "N"    // New
    ENDIF
    IF M_RESP2 = "-"
        M_RESP2 := "D"    // Delete
    ENDIF
    *
    DO CASE
    CASE M_RESP2 = "A"  && ALTER
        *  alter /edit
        M_FIND = SUBSTR(CUSTID+SPACE(LEN(custid)),1,LEN(custid))
    CASE M_RESP2 = "B"
    CASE M_RESP2 = "C"   && skipCode
        **
    
        */ ====================================================
    CASE M_RESP2 = "D"     &&   delete
        //
        // Deletes  require every file to be checked that uses a record in this table 
        // You cannot delete any transactions that have a link, otherwise your user will have 
        // orphaned records that cannot be recovered
        TEMPSCR := SAVESCREEN(20,0,21,79)
        POS     := RECNO()
        OK1     := 0
        OK2DELE := 0
        ERRMSG1(20,10,"DELETE NOT IN USE", "To Delete checks should be made to avoid orphaned records")
        INKEY(0)
        RESTSCREEN(20,0,21,79, TEMPSCR)
        //
        //
        SETCOLOR(prgcolor)
        RELE FIRST
        *
        //
    CASE M_RESP2 = "E"       // edit see below line 349 approx "AE"
        DO GET110            // In HBR110Ticket
        //DO SAY110          //   dittto
    CASE M_RESP2 = "F"       // filter
        * locate a particular ticket or client
        TEMPSCR := SAVESCREEN(20,0,21,79)
        POS     := RECNO()
        OK1     := 0
        OK2DELE := 0
        ERRMSG1(20,10,"FILTER AS REQUIRED", "Add your own Filter Options and routine here ")
        INKEY(0)
        RESTSCREEN(20,0,21,79, TEMPSCR)
        SETCOLOR(PrgColor)
        //
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
        /// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% modify to exclude account code & remove postcode lookup for delivery address %%%%%% ------------------------------------------------------------
        // For this example, not using SEEK. Unless there is a huge number of records LOCATE will work adequately on today's PCs. Plus in this case it is likely that the code isn't known. 
        // So using LOCATE a search of the company/person name can work best as the entry can be with just a few characters from the name. Which is much better for this example.
        //----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        NEWSCR  := SAVESCREEN(0,0,24,79)
        M_RESP2 := "A"
        M_CUSTID:=space(8)
        M_ALPHA :=space(8)
        M_OK    := "Y"
        //#M_SKEY   =SPACE(LEN(custid))
        SET EXACT ON
        SELE 81
        @ 19,02 to 23,76
        //
        @ 20,04 SAY "ENTER PART OF NAME"
        @ 20,37 SAY "EXIT WITH  00 for ENTRY, or Y/N/Q "
        @ 22,04 say "     CHOISE  Y/N/Q"
        ***
        //
        @ 20,24 GET M_CUSTID PICT "!!!!!!!!"  VALID (.NOT. EMPTY(M_CUSTID))      // Not actually using M_CUSTID to Search F81->CUSTID here but using if to search F81->NAME
        @ 22,24 Get M_OK     PICT "!"          // you may prefer the Y/N/Q prompt inside the ticket area, your choice and always showing edit or not 
        READ
        CLEA GETS
        M_CUSTID := ALLTRIM(M_CUSTID)
        PRIVATE mPOS 
        mPOS := 0
        If M_OK = "Y" .AND. LEFT(M_CUSTID,2) <> "00"
            IF SUBSTR(M_CUSTID,1,2) = "00" .OR. EMPTY(M_CUSTID)
                * nothing
            ELSE                                            // USING LOCATE AS QUICK ENOUGH FOR THIS OPERATION
                LOCATE FOR AT(M_CUSTID,UPPER(NAME)) >0      // M_CUSTID is UpperCase, so Field to search must be UpperCase
                If !EOF()  // RECNO() >0
                    mPOS := RecNo()
                    GO mPOS
                Else 
                    DBGOTOP()
                ENDIF
                // For this example, not using SEEK. Unless there is a huge number of records LOCATE will work adequately on today's PCs. Plus in this case it is likely that the code isn't known by the user. 
                // So using LOCATE a search of the company/person name can work best as the entry can be with just a few characters from the name. Which is much better for this example.
                // A part name that isn't found will put the user at the start of the name & address table
                mFIRST = SAVESCREEN(0,0,24,79)
                @ 12,40 CLEA TO 16,60
                SETCOLOR(cBlkYellow)
                SetMode(43,128)      // larger screen for table view to avoid scrolling horizontally with arrow keys
                @ 22,22 SAY "Arrow Up/Down PRESS [Enter] TO SELECT CUSTOMER"
                DBEDIT(01,01,40,128,A_F81,.T.,.T.,A_F81h)   // For the larger screen set 
                SetMode(25,80)    // back to StdScreen size
                SETCOLOR(PRGCOLOR)
                RESTSCREEN(0,0,24,79,mFIRST)
                RELE mFIRST
                M_RESP2 = ' '
                // CLIENT FOUND BEFORE ADDING CHECK IN CASE ABANDONING ADD ------------------
                RESTSCREEN(0,0,24,79,NEWSCR)     // REMOVE PROMPTS AFTER SELECTING CUSTOMER
                NEWSCR := SAVESCREEN(00,00,24,79)
                @ 19,02 to 23,76
                M_CUSTID := F81->CUSTID
                M_NAME   := F81->NAME
                M_OK     := "Y"
                @ 20,10 SAY "ADD NEW TICKET FOR : " + M_CUSTID + " - "+ M_NAME
                @ 21,10 SAY "    CONTINUE Y/N/Q   " GET M_OK PICT "!"
                READ 
                IF M_OK = "Y" 
                    // ADd ticket
                    SELE 99  // Open SysMaster to get next Ticket Number
                    GO 1
                    // Doing this just to show how to get the next ticket number and write it it
                    // In a commercial system  you wouldn't normally allow a use access to ticket numbering
                    // UNLESS of course it is a system to record ticket entries from a manual ticket book, or some other source
                    //----------------------------------------------------------------------------------------------------------
                    Private mTKT :=0
                    mTkt := NEWTKTNO
                    If REC_LOCK(0)
                        REPLACE NEWTKTNO with mTKT +1
                    Endif
                    **
                    SELE 11  // Back to Ticket
                    // NOW ADD NEW TICKET
                    IF ADD_REC(0)
                        REPL CUSTID WITH M_CUSTID, TICKET with AllTrim(str(mTKT)), TDATE with DATE(), VATRATE WITH mVATR   // autoenter date and VAT Rate. The VATRate is added from 99 as being current VATRate. After cannot change VATRate later
                    ENDIF
                    UNLOCK
                    RESTSCREEN(0,0,24,79,NEWSCR)
                    DO SAY110         // refresh with new customer on new ticket
                    M_RESP2 := "E"    // goto editing reqd?
                    DO GET110         //
                ELSE 
                    SELE 11
                    RESTSCREEN(0,0,24,79,NEWSCR)  
                ENDIF
            ENDIF
            // &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
        ELSE 
            //  // exit selected
            // ANY OTHER ENTRY N, Q, whatever exits
            //     // in HBR110Ticket
            SELE 11
            RESTSCREEN(0,0,24,79,NEWSCR)
            //
        ENDIF
        SELE 81     //
        DO SAY110
        *****   
        @ 19,10 say Space(40)
        @ 20,10 Say Space(40)
        @ 22,10 Say Space(40)
        M_RESP2 = " "
        *@ 23,35 say SPACE(32)
        *@ 23,35 SAY REPLICATE(CHR(196),32)
        SETCOLOR(prgcolor)
        RELE NEWSCR
        /// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% one code only %%%%%%%%%%%%%%%%%
    CASE M_RESP2 = "P"   && print
        TEMPSCR := SAVESCREEN(20,0,21,79)
        POS     := RECNO()
        OK1     := 0
        OK2DELE := 0
        ERRMSG1(20,02,"YOUR STANDARD PRINT OPTIONS SCREEN CALLED HERE", "Options to Printer, PDF and Email")
        INKEY(0)
        RESTSCREEN(20,0,21,79, TEMPSCR)
        /// %%%%%%%%%%%%%%%%%%% your std print routine %%%%%%%%%%%%%%%%%%%
        /// maybe a menu with print options %%%%%%%%%%%%%%%%%%%%%%%%%%%% 
        //
    CASE M_RESP2 = "V"
        //SET ESCAPE OFF      // OTHERWISE DROPS OUT OF MENU
        // Added an option menu here. Not actually necessary but put in as an example
        FIRST := SAVESCREEN(0,0,24,79)
        OK1   := WHICH1(13,33,"SELECT FILE TO VIEW:Abandon View:View Ticket File:")     // Summary Details:Filter Address File:")
  
        DO CASE
        CASE OK1 = 1
            //SET ESCAPE ON
            SETCOLOR(PRGCOLOR)
            RESTSCREEN(0,0,24,79,FIRST)  
        CASE OK1 = 2
            //SET ESCAPE ON
            /// YOU COULD REPLACE THE CUSTOMER ID CODE FOR THE CUSTOMER NAME WHICH WOULD BE BETTER
                   
            // *************************************************
            Setmode(43,132)   // see Harbour Language Reference
            @ 42,10 SAY "TICKET FILE - [PageUp/Down] and <Arrow> keys or Press [Esc] / [Enter] to Exit & Select"
            // *************************************************
            SETCOLOR(cWhiteScr1)  && DBWHITE)
            DBEDIT(01,10,40,108,A_F11,.T.,.T.,A_F11h)   // For the larger screen set 
            //   I have used DBEDIT() as it is quick and easy to use. At one point it was to becombe unsupported but that hasn't happened as yet (I think)     
            SETMODE(25,80)
            //
            SETCOLOR(PRGCOLOR)
            RESTSCREEN(0,0,24,79,FIRST) 
            //RELE FIRST
        //CASE OK1 = 3 && 
            //   You could have additional options to filter or view your data here
        //CASE OK1 = 4
            //                ditto as above
        ENDCASE
        //       
        Legend(00,Space(74))
        LEGEND(00," TICKET FILE ")
    CASE M_RESP2 = "S"  && Sort
        TEMPSCR := SAVESCREEN(20,0,21,79)
        POS     := RECNO()
        OK1     := 0
        OK2DELE := 0
        ERRMSG1(20,07,"SORT TICKET FILE BY TICKET, DATE or CLIENT ", " ** Add your own sorting options here. Or maybe to VIEW OPTION MENU ** ")
        INKEY(0)
        RESTSCREEN(20,0,21,79, TEMPSCR)
        // SELE
    CASE M_RESP2 = "W"  && 
        //
    ENDCASE
    //
    DO SAY110
    //
ENDDO
Setcolor(cFrontScrn)
DBCLOSEALL()
//
//
RETURN
*
*!*********************************************************************
*!
*!      Procedure: SAY110
*!
*!      Called by: 110ticket.PRG
*!
*!*********************************************************************
PROCEDURE SAY110    // 
    // in correct Work Area 
    Private mCustID, mAddress
    // 
    SETCOLOR(PRGCOLOR)
    @ 04+mLC-1,25 SAY CUSTID
    mCustID := CUSTID
    //
    SELE 81  // clients
    SEEK mCustID
    If Found() 
        @ 04+mLC-1,35 Say F81->Name 
        mAddress := AddressLine5(Addr1, Addr2, Addr3, Addr4, Pcode)
        @ 05+mLC-1,17 Say Space(60)      // clear prev address
        @ 05+mLC-1,17 say mAddress
        @ 06+mLC-1,48 Say "Tel:" + Left(F81->Tel,26)
    Else
        @ 04+mLC-1,35 Say "CLIENT DETAILS NOT FOUND!"
        @ 05+mLC-1,17 Say Space(60)
        @ 06+mLC-1,52 say Space(26)
    Endif
    SELE 11
    //
    @ 06+mLC,25 SAY TICKET
    @ 07+mLC,25 SAY TDATE
    @ 08+mLC,25 SAY PRODUCT
    @ 09+mLC,26 SAY QUANT     PICT "99999"
    @ 10+mLC,25 SAY RATE      PICT "999999.99"
    @ 11+mLC,25 SAY NETCALC   PICT "999999.99"
    // no need to say again @ 11+mLC,25 SAY 
    @ 13+mLC,25 SAY VATCALC   PICT "999999.99"
    @ 14+mLC,25 SAY GROSSCALC PICT "999999.99"
    // @ 16+mLC,25 SAY PAID       // STOPPED As maybe confusing to include in this example but you may need in some cases to flag as paid/not paid. Often charges are posted to accounts, so maybe other considerations
    // 
    //
 ****-------------------------| |--------
 **
 //
 *
RETURN  && SAY110
//
//
//---------------------------------------------------+
//       Procedure GET110                             |
//---------------------------------------------------+
PROCEDURE GET110
///////
      @ 12-1,58-13 SAY SPACE(8)
      @ 02,60 say "**&&**"
      // ------------------------------------------------------------------------------------------------------------------------------------------------------
      * I have used M_ prefix to indicate a memory variable. You could have just m or mem or whatever. 
      // Purists reckon using M_or m prefixes not to be considered, sometimes even irritating to them
      // Personally I find it useful. Other systems can have all sorts of variables and you need to know the differences
      // For instance I can have a variable on a screen that has been sourced from a table and used along with a memory variable
      // Address1 in a table could be on a screen as sAddress1 and as a memory variable as mAddress1 and you are able to compare them 
      // or make a decision after a change. But as a developer you may choose which you prefer and if it suits you then that is OK
      // Additionally I can have a CustID variable in several Tables. For example; M_81CustID and M_20CustID indicates memory variables from different Tables
      // In this example I have used the M_ prefix as it is very obvious what it is with little explanation.
      // If you are developing a project with others, then you will need an agreed standard for all to comply with of course. Your software, your choice.
      // --------------------------------------------------------------------------------------------------------------------------------------------------------
      M_CUSTID    := CUSTID
      M_TICKET    := TICKET   
      M_TDATE     := TDATE
      M_PRODUCT   := PRODUCT
      M_QUANT     := QUANT
      M_RATE      := RATE
      M_NETCALC   := NETCALC     // 
      //M_TEL     := TEL
      M_VATRATE   := VATRATE      
      M_VATCALC   := VATCALC
      M_GROSSCALC := GROSSCALC
      //
      M_YN := "Y"
      //M_OK := 0
      @ 04+mLC-1,25 SAY CUSTID
      @ 06+mLC,25 GET M_TICKET
      @ 07+mLC,25 GET M_TDATE
      @ 08+mLC,25 GET M_PRODUCT                                   // INTEGER CALCS: It does appear that Harbour/dBASE tables do not have 16 significant decimal places in the background. However in different databases it is not always 
      @ 09+mLC,26 GET M_QUANT     PICT "99999"                    // the case. If you start with Integer Calcs you will have no need to amend your routines in the future if necessary. Personally, in an application I will only store
      @ 10+mLC,25 GET M_RATE      PICT "999999.99"                // the results of calculations as integers in the Tables. i.e. for a calc value of ?567.232166 I would save that as INT((567.232166 * 0.01)) = 56723 to the table.
      @ 11+mLC,25 SAY M_NETCALC   PICT "999999.99"                // If you use integers you do not need to check calculations, ever. Adding up 20 lines, or 200,000 lines can only give the correct result. All you need to do then is to 
      //@ 12+mLC,25 Say M_VATRATEno need to SAY not editable here // string handle the value with a string handling Function that will show 567.23 (from above). A report is as happy with FIELDNAME as it is with DECIMAL2(FIELDNAME) 
      @ 13+mLC,25 SAY M_VATCALC   PICT "999999.99"                // Same with weight calcs. Integers only with Kilograms as integer value. Using integers means no need to check your calcs. BUT YOUR CHOICE
      @ 14+mLC,25 SAY M_GROSSCALC PICT "999999.99"                // 
      // @ 16+mLC,25 Get M_PAID                                   // I have GrossCalc in this Table but normally I see no need as all I need to do is add NetCalc to VatCalc when required.  SEE BELOW %%%%%
      READ
      @ 19,10 Say "SAVE CHANGES Y/N " 
      @ 19,27 Get M_YN Pict "!"     // Here all the user needs to to is press enter to accept the "Y" the space, or just press any other key to change
      READ
      // Instead of the second READ you could have a standard popup prompt menu with two options : Continue & Abandon
      // probably a tidier solution but not included in this example
      If M_YN = "Y" 
         // update calcs before save
         //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
         IF M_VATRATE = 0        // Just in case a zero vat rate. UNLESS of course a zero Vat Rate is valid for your application! VAT can become complex if handled correctly. You also may need have multiple VAT Rates which this example
            M_VATRATE := mVATR   // does not include. Additionally, if VAT Rates change you can be entering charges before the VAT Rate change. In which case you will need a SPECIFIC VAT TABLE with rates and date periods applicable
         Endif                   // In any major system you WILL need a VAT Table with rates and date ranges. DO NOT HARDWIRE VAT Rates into your system. If you only have one VAT Rate and a new client needs to use three they cannot use 
         //                      // your system. No sensible company would buy a system that doesn't handle VAT as well as (say) SAGE does. Additionally and obviously  VAT should be calculated on each line and NOT on the total invoice value. 
         M_NETCALC   := M_QUANT * M_RATE
         M_VATCALC   := M_NETCALC * M_VATRATE * 0.01   // You have VAT at 99.99 not 0.9999 in your 99 Tabel and your 11 Table
         M_GROSSCALC := M_NETCALC + M_VATCALC
         IF REC_LOCK(0)
            REPL CUSTID WITH M_CUSTID, TICKET WITH M_TICKET,TDATE WITH M_TDATE, PRODUCT WITH M_PRODUCT,QUANT WITH M_QUANT,RATE WITH M_RATE,NETCALC WITH M_NETCALC,VATRATE WITH M_VATRATE,VATCALC WITH M_VATCALC,GROSSCALC WITH M_GROSSCALC
            COMMIT
         ENDIF
      ENDIF
      @ 19,10 say Space(20)   // clear Save changes prompt
      //
////////
RETURN   // GET110
******
// %%%%%%%%%%%%%%%%%% Ticket design and a few extra thoughts
// TICKET DESIGN
//          Where practical use codes and the descriptions etc from standing data tables. However, whilst full normalisation is advised, sometimes it can be preferred to repeat some data.
//          It was more of a problem when storage was expensive but today less so. If a description changes in a standing data table, instead of changing it, add a new code and description. 
//          If you chang ethe description, then on any old orders reprinted the new description will show. Instead add a new code and hide the one out of date. The hide will apply when adding 
//          new orders but not when printing or viewing ticket data. 
//  FORMING INVOICES 
//          Many designs have an invoice header and invoice detail. You are then forced to update the header when ticket data changes. Do you need the header? You can easily summarise all 
//          ticket data into one record in a query on the fly. That way any ticket changes are reflected without the need for header updates. 
//
//            
**
**                   *************************
*                    *   END HBR110Ticket    *
*                    *************************
//
//
// EOF: HBR110Ticket.PRG
//\\

