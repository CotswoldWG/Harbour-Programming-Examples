// +-------------------------------------------------------------+
// | ** HBR102MENU                                               |
// | Copyright© W.Greenwood England 1983-2024                    |
// | This free software is released under the MIT License.       |
// | See https://en.wikipedia.org/wiki/MIT_License               |  
// +-------------------------------------------------------------+
// * +-----------------------------------------------------------+
// * |   HBR102Menu                                              |
// * |   Called by HBR101Start                                   |
// * |   Opens HBR810Addr only in this demo/example              |
// * |   Uses Functions in HBRLIB24                              |
// * |   Other: HBR102Menu, HBR110Ticket, HBR810Addr, HBR910Rein |
// * |   Version 2419                                            |
// * +-----------------------------------------------------------+
// *
*+---------------------------------------------------------------------------------------------------------+
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
// 
//
// **********************************************************************************************************
// NOTE! The Windows Close button is set off in HBR101 to prevent an illegal exit when editing or adding data
   SET DELE ON
   PUBLIC  mPATH  //,M_17I, M_17O
   //
   PUBLIC WG_GR, RWFLASH, YONB, YONP, NOCOLOR, ERRCOLOR, MENUCOLOR, FCount, mF81, mA81, mI81, mFatalError
   
   PUBLIC TName[7], TAlias[7], TExcl[7], TIndx[7]
   //
   // ------------------------------------------------------------------------------------------------------------------------
   // %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%move down to Dataopen() NO
   mFatalError := .F.
   // ----------------------------------------------------------------------------------------------------------------------------
   mFatalError = CHECKIFDATAFOUND()  // Function in HBR101Start
   // If  the data is not found the program cannot continue. After issuing software the user is responsible for the data location
   // Although you will probably provide support to help them find it, if it has been moved.
   //-----------------------------------------------------------------------------------------------------------------------------
   CLOSE DATA
    If mFatalError = .T. 
      @ 12,10 
      @ 13,10 Clear to 14,75
      ERRMSG1(12,15,"FATAL ERROR!", "ONE OR MORE DATAFILES ARE MISSING.....PROGRAM Will CLOSE")
      // you should give the user more details of the error. Datafilename and expected location
      INKEY(0)
      QUIT
   ELSE
     // DATAOPEN()  // Fn in HBR101Start ONLY USED WHEN OPENING SCREENS - see 110Ticket, 810Addr
   Endif
   
   // %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   SET BELL OFF
   SET DATE BRITISH
   SET DELE ON
   SET EXAC ON
    // 
   SET MESS TO 23
   SET TALK OFF
   SET SCOR OFF
   SET WRAP ON
   STORE Date() TO M_EDATE
    // 
    // ******************************** 

   MENUCOLOR := cFrontScrn
   SetColor( MENUCOLOR )
  
   // ******************************************  **************
   //
   DECLARE MENU1[ 5 ], MENU2[ 4 ], MENU3[ 2 ]
// "<-------------------->" size keep to this if possible
   MENU1[ 1 ] :="Return to Main Menu  "     // activates with 1st letter or arrow down and press <Enter>
   MENU1[ 2 ] :="Tickets              "     // O
   MENU1[ 3 ] :="Stock/Product Details"     // 
   MENU1[ 4 ] :="Invoicing Section    "     // 
   MENU1[ 5 ] :="Customer Details     "     // 
// * -------------------------------------------- split end of menu 1
   MENU2[ 1 ] :="Return to Main Menu  "
   MENU2[ 2 ] :="Backup and Reindex   "  // user option to check files. You may need to check if the datafiles are in use before proceeding
   MENU2[ 3 ] :="System Settings      "  // a system file to allow the user to make any global adjustments that you want to allow. Also, change telephone, or email addresses etc 
   MENU2[ 4 ] :="Database Location    "  // Set the path to data 
// * -------------------------------------------- split end of menu 2
// * -------------------------------------------- split end of menu 3
   MENU3[ 1 ] :="Return to Main Menu   "
   MENU3[ 2 ] :="Quit Close Program    "
// -------------------------------------------- split end of menu 4
// ********************************* NAME ON SCREEN SETUP *************
   
   CLEAR SCREEN
  
   PUTNAME  :=SaveScreen( 09, 07, 16, 72 ) // SAVING SCREEN AREA OF NAME
// ********************************* NAME ON SCREEN SETUP **************
    DOMENU :=.T.
    DO WHILE DOMENU
        //
        CLEAR SCREEN
        RestScreen( 09, 07, 16, 72, PUTNAME )
        LEGENDBX( 20, NMFWNR, "D" )
        //@ 09,00 say mf81     // path to data
        //@ 23, 00 say mCurPath + "HBR81.DBF"  
        @ 24, 00 say mCurPath // + mF81 //
        @ 24,65 Say "Version: 2419"      && Reset Version number after change
        SetColor( MENUCOLOR )
        @ 01, 02 CLEAR TO 03, 77
        @ 01, 02 TO 03, 77 DOUBLE
        // "<--------->" size keep to this if possible
        @ 02, 05 PROMPT " MAIN OPTIONS "  // 1st menu - activates with 1st letter
        @ 02, 30 PROMPT " UTILITIES "
        @ 02, 65 PROMPT "   QUIT   "  // 2nd & last
        MENU TO CHOICE
        WHICH :="MENU" + AllTrim( Str( CHOICE, 1, 0 ) )
        T :=4  // the top line for use in positioning menus T,L,B,R
        L :=0
        DO CASE
        CASE CHOICE =0
            LOOP
            STORE .F. TO DOMENU
        CASE CHOICE =1  // 4
            L :=5  // start position
            B :=9  // T + no of items on menu
            R :=32 // L + 27 (width of submenu bars 27 ON THIS NORMALLY 25)
        CASE CHOICE =2
            L :=30
            B :=8
            R :=55
        CASE CHOICE = 3   // 2 exit
            L :=50
            B :=6
            R :=75
        ENDCASE
        RINDX :=.F.  // setting
        SAVE SCREEN TO MENUSCR
        HOLE :=SaveScreen( T, L - 1, B + 1, R + 1 )
        @ T, L - 1 CLEAR TO B + 1, R + 1
        @ T, L - 1 TO B + 1, R + 1 DOUBLE
        SUBCHOICE :=AChoice( T + 1, L + 2, B, R - 2, &WHICH )
        IF LastKey() = 4
            RestScreen( T, L - 1, B + 1, R + 1, HOLE )
            KEYBOARD Chr( 4 ) + Chr( 13 )
        ELSEIF LastKey() = 19
            RestScreen( T, L - 1, B + 1, R + 1, HOLE )
            KEYBOARD Chr( 19 ) + Chr( 13 )
        ENDIF
        IF SUBCHOICE = 0
            RestScreen( T, L - 1, B + 1, R + 1, HOLE )
            @ 23, 0 CLEAR
        ELSE
            DO CASE
            CASE SUBCHOICE = 1
                RestScreen( T, L - 1, B + 1, R + 1, HOLE )
            CASE CHOICE = 1       // first menu
                DO CASE
                CASE SUBCHOICE = 2  // ticket screen
                    DO HBR110Ticket
                CASE SUBCHOICE = 3  // stock file or product list
                    TEMPSCR := SAVESCREEN(20,0,21,79)
                    ERRMSG1(20,02,"STOCK FILE OR PRODUCT LIST (not included, to add to your design)", "To enter the Product onto a ticket instead of manually entering each time")
                    INKEY(0)
                    RESTSCREEN(20,0,21,79, TEMPSCR)
                    SETCOLOR(MenuColor)
                    //
                CASE SUBCHOICE = 4  // invoices
                    TEMPSCR := SAVESCREEN(20,0,21,79)
                    ERRMSG1(20,01,"INVOICE SECTION (not included, to add to your design)", "Form, print, Email and export to accounts the ticket charges by date etc")
                    INKEY(0)
                    RESTSCREEN(20,0,21,79, TEMPSCR)
                    SETCOLOR(MenuColor)
                    //
                CASE SUBCHOICE = 5 // Name & Address 
                    DO HBR810Addr   // the only program to open in this example
                    Setcolor(MENUCOLOR)
                ENDCASE
            CASE CHOICE = 2   // Utils menu
                DO CASE
                CASE SUBCHOICE = 2  // REINDEX SYSTEM FILES
                    DO HBR910REIN
                    //
                CASE SUBCHOICE = 3  // alter WASTE % & printer
                    //DO SYSTEM MASTER FILE
                    TEMPSCR := SAVESCREEN(20,0,21,79)
                    ERRMSG1(12,01,"EDIT SYSTEM MASTER FOR USER TO CONFIGURE THE SYSTEM (not included)", "Access for the user to configure some system settings")
                    INKEY(0)
                    RESTSCREEN(20,0,21,79, TEMPSCR)
                    SETCOLOR(MenuColor)
                    //
                CASE SUBCHOICE = 4
                    TEMPSCR := SAVESCREEN(20,0,21,79)
                    ERRMSG1(12,01,"SET DATA LOCATION (not included, to add to your design)", "The Data Tables/Database network setup. Should include/allow for UNC paths.")
                    INKEY(0)
                    RESTSCREEN(20,0,21,79, TEMPSCR)
                    SETCOLOR(MenuColor)
                    //
                ENDCASE
            CASE CHOICE = 3 // EXIT
                IF SUBCHOICE = 2
                    CLOSE DATA
                    QUIT
                ENDIF
            ENDCASE
        ENDIF
        RESTORE SCREEN FROM MENUSCR
    ENDDO
    CLOS DATA
    CLEAR
    QUIT
/////    
//        END xxx PULLDOWN   xxxMENUP
// menu_user.prg - separate file normally
//
//
// PHYSICAL END TO HBRMENU  
// //////////////////////////////////////////////////////////////////////////////
//\\