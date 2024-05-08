// +-------------------------------------------------------------+
// | 1st issue May 2024                                          |
// +-------------------------------------------------------------+
// | For new users of Harbour software that includes the Clipper |
// | programming language. Due to the total lack of printed      |
// | documentation it can be difficult for new users to just get |
// | up and running with even a simple starter from which they   |
// | can proceed and then adapt and use the language in their own|
// | development style. This is an example of that first step.   |
// +-------------------------------------------------------------+
// | There are other examples available to assist new users on   |
// | the internet and posted on the Harbour User Group Website.  |
// | See:                                                        |
// | It is a Menu interface and sample name and address screen   |
// | that is probably typical of just about every application.   |
// | From that you should be able to adapt and extend to include |
// | other screens, or just expand the name and address as you   |
// | may see fit.                                                |
// +-------------------------------------------------------------+
// | Sample program in Harhour text screen written to give an    |
// | idea of how to create your first program. It isn't uncommon |
// | for there to be little if any useful examples to new users. |
// | Or to find useful examples of a pracical nature, or showing |
// | simple examples. Some are often complex or not that useful. | 
// | in th eearly days.                                          |
// | I do not intend to be critical but some can be somewhat     |
// | esoteric. They may well exasperate of put off enthusiastic  |
// | and new users who are keen to see something running.        |
// | This is not a definitive example or something that should be|
// | strictly followed. It no doubt many may opine that is plenty|
// | of scope for updating and improvement. It is more to be     |
// | looked on as something to get you started. Harbour and      |
// | Clipper are used to create serious database applications.   |
// | If you are looking for a Microsoft Access alternative then  |
// | Harbour gives you that. For those needing a GUI interface   |
// | there are several that can be used to extend Harbour        |
// | I hope you find this helpful in getting started.            |
// +-------------------------------------------------------------+                                                
// | Copyright© W.Greenwood England 1983-2024                    |
// | This free software is released under the MIT License.       |
// | See https://en.wikipedia.org/wiki/MIT_License               |  
// +-------------------------------------------------------------+
// | DISCLAIMER - A Necessary Requirement in Today's World (sigh)|
// | This software has been prepared to assist a new user to the | 
// | design and operation of a Harbour text based appliocation.  |
// | Whilst every effort has been made to issue a sound example  |
// | it is up to each individual developer to check the software |
// | is operating as they expect it to do and to complete their  |
// | own tests and validation as to its suitability.             |
// | The responsibilty for any code if passing this software on  |
// | for profit, or not, is with the the transferring entity.    |
// | IT IS ISSUED FREE OF CHARGE IN AN "AS IS" STATE AND UNDER   |
// | THE PRINCIPLE OF CAVEAT EMPTOR MIT License(W.Greenwood 2024)|
// +-------------------------------------------------------------+
// | FILES INCLUDED IN THIS SAMPLE                               |
// | The following files should be in your develop folder        |
// | Addr.bat To run Addr.bhp (you willl need SYS.ch & hbwin.ch) |                               
// | Addr.bhp  Harbour file to compiler                          |
// | HBR81.dbf, HBR81i1.ntx      (address data)                  |
// | HBR11.dbf, HBR11i1.ntx      (ticket data)                   |
// | HBR99.dbf                   (system master)                 |
// | HBR101Start.prg Startup for system & open menu              |
// | HBR102Menu.prg  Menu to access program screens              |
// | HBR110Ticket.prg  Ticket add/edit screen                    |
// | HBR810Addr.prg  Address add/edit screen                     |
// | HBR910Rein.prg  Utility reindex                             |
// | HBRLIB24.prg    Lib file with standard functions            |
// | Knowledge of Harbour/Clipper languages is presumed          |
// | Setting the paths to these in the Harbour folder set will of|
// | course remove the need to those Harbour Include files       |
// | NOTE! Only keyboard operations/selections & <Enter> key     |
// +-------------------------------------------------------------+
// | NOTE! To reiterate. This is software written in Harbour. It |
// | is does not have the possible mouse support included. All   |
// | operations are via keyboard, single key press of the first  |
// | letter of each menu item. The [X] in the Windows control box|
// | is switched off using   hb_gtinfo( HB_GTI_CLOSABLE,.F.)     |     
// +-------------------------------------------------------------+
/////////////////////////////////////////////// 202347 /////////// 
/////////////////////////////////////////////// 202414 ///////////
/////////////////////////////////////////////// 202416 ///////////
/////////////////////////////////////////////// 202418 ///////////
// 
// Start of code
//
//  NOTE! Occaisionally, I will use the Old Clipper/dBASE option of using just the first four characters of a Command
//        SELE for SELECT, INDE as INDEX, SET COLO TO ....etc. 
//
Setmode(43,132)
FUNCTION Main()
   // ----------------------------------------------------------------------------------------------------------------------------------------------------
   // Program        : Functions and location
   // HBR101Start    : Main(), CheckIfDatafound(), DATAOPEN(), SetIndexes()
   // HBR102Menu     : Menu for system control
   // HBR810Addr     : Say810()
   // HBRLIB24       : NET_USE(), NET_USE1(), ArrowOn2(), ArrowOff(), AddrLine5(), SkipB(), SkipN(), Legend(), LegendBX(), ErrMsg1(), REC_LOCK(), ADD_REC(), WHICH1(), ShadowR()
   //                : Normally this would be WGSStandardLib as it would be used in all applications. It would not be applpcation specific. It is here for clarity only
   // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
   //
   #include "sys.ch"          // unless you amend the paths to your system so that C:\HB32 and included folders are included in default
   #include "hbwin.ch"        // you will need these Libs in your folder with the programs during compilation to an EXE
   #include "hbgtinfo.ch"     // 

   REQUEST HB_GT_WVT_DEFAULT
    LOCAL OldRow, OldCursor, OldColor, OldScreen, XX
   // setting colors  -----------------------------------------------------------------------------
   Public cStartScreen, cEdit1Be, cEdit2Gy, cEdit3Gy, cEdit4Cn, cEdit5We, cEdit6Bk, cErrBY, ErrWB, mDrive, mPath, mDataPath, mCurPath, mConoPath, TKount,Kount
   Public NMFWNR, mMaxRow, mMaxCol,mIsNotError,mFileName, mAlias, mExcl, mCODEPRFX
   PUBLIC A_F81[6], A_F81h[6]
   PUBLIC   mF81,mA81, mExcl, mI81 
   //------------------------------------------------------------------------
   // Arrays for files. You will have all files in system set up here. Or in a separate Function 
   // Use in Table opening DATAOPEN() 
   PUBLIC pExclOrNot
   pExclOrNot := .F.    // changed in 110, 810, 910
   PUBLIC FILENAME[3], FILEALIA[3], FILESELE[3]
   FILENAME[1] := "HBR11"  // Ticket Table
   FILENAME[2] := "HBR81"  // Name & Address Table  
   FILENAME[3] := "HBR99"  // System Settings containing global data needed
   FILEALIA[1] := "F11"    //
   FILEALIA[2] := "F81"    //
   FILEALIA[3] := "F99"    //
   FILESELE[1] := 11
   FILESELE[2] := 81
   FILESELE[3] := 99
   //
   //------------------------------------------------------------------------
   PUBLIC mSELE
   NMFWNR  =  " EXAMPLE OF A MENU IN HARBOUR WITH CUSTOMER AND TICKET ENTRY SCREENS " 
   mMaxRow := 25
   mMaxRow := 80
   Setmode(mMaxRow, mMaxCol)      // << see the harbour language reference for Harbour's implementation of clipper 5.3 for this
   // -------------------------------------------------------------------------------------------------------------------------------------------------------
   // Below are variables showing the Harbour Functions for file location.  
   // I usually have a textfile or Table in the current folder that the program will read on opening that gives the path to the data 
   // That file or table needs software to maintain it. Neither are included in this example. You should always avoid Hard-Wiring the path to your data 
   // Keep it an option that the user can set the data location. They may need it on a network, on a different drive, or location to suit their operation
   // Of course you will need a program or Function to allow the user to set the data location. This may be a separate program or included within the main 
   // Clearly if it is the main program, you will need to ensure that on open or locating the data does not create a fatal error that shut the program down
   // I will say that it is always helpful if the actual program is always in a set folder name on the users' PC. It makes issuing updates easier.
   // Particularly if you need to update many PCs on a system with the update.
   // -----------------------------------------------------------------------------------------------------------------------------------------------------------
   mPath     := CurDir()                           // directory of program being run, Current folder
   mDrive    := DiskName()                         // Usually Returns C
   mCurPath  := mDrive + ":\" + mPath + "\"        // These aren't used here but are shown as a Tip or Hint
   mDataPath := mDrive + ":\" + mPath +"\Data\"    // if you have a always have a Data folder in the Program folder
   SET PATH TO mCurPath                            // It is usual to have the setup and location of  the data or database held in a separate text or DBF file That is read on starting up. (not included)
   // 
   // You can have many different colours if you wish. I used to have maybe 24 but you tend not to need that many
   // as you will no doubt adopt a standard colour scheme with many screens the same. 
   // This function could be in a Function in your own separate Standard Library for use by all of your software
   // Instead of using variables as below you could have them in an Array
   // Generally, if you are often using a function that does not change keep in one place only.
   // ON COLOURS:  White and Green to be avoided as often difficult for colour blind users. 
   // You  can also set colours using standard RGB() colours : browser search "harbour hb_gt?" for more info
   // additionally see the helpful forum entry by Paul Smith https://groups.google.com/g/harbour-users/c/eq-NK8YkI0o/m/ODD9qfnOAAAJ
   // you can get carried away and have too many colours. Some programs only use five or six. No need to over complicate matters
   // see colour chart in Cotswold notes manual for letters and numbers for colours
   PUBLIC cFrontScrn, cGreyScrn1, cGreyScrn2, cBlkYellow, cWhMagenta, cCyanScrn1, cWhiteScr1, cWhiteScr2
   //
   //
   //SETTING COLOURS FOR PROGRAM  This coould be in its owb Function -------------------------------------------------
   cFrontScrn  := "14/01,0/15,,,14/01"    && STDCOLOR[1]     // Yellow/blue menu
   cGreyScrn1  := "15/08,0/15,,,15/08"    && White/Grey, gets black on White
   cGreyScrn2  := "W+/N+,W+/RB,,,W+/N"    && White on grey - getsWhire on magenta  << ALPHA Color Codes do not apper to work in Harbour 3.2
   cBlkYellow  := "0/14,14/0,,,0/14"      // "N/GR+,GR+/N,,,N/GR+"    && black on yellow  
   cWhMagenta  := "15/5,0/15,,,15/5"      //"W+/RB,N/W+,,,W+/RB"     && White on Magenta
   cCyanScrn1  := "0/11,14/0,,,0/11"  
   cWhiteScr1  := "0/15,14/0,,15/0,0/15"
   cWhiteScr2  := "0/7,14/0,,7/0,0/7"
   //  Colors end -------------------------------------------------------------------------------------^^^------------------
   SETCOLOR(cFrontScrn)
   SetMode(25,80)    // 25,80 is the default screen size 43,132 is used in the 110 Ticket & 810 Address screen on the View options.
                     // See Harbour Language Reference for SetMode() options
   // -------------------------------------------------------------------------------------------------------------------------------------------------------
   hb_gtInfo( HB_GTI_WINTITLE, "Clipper5.3 & Harbour 3.2" ) // can use top line for details
   hb_gtInfo( HB_GTI_FONTNAME, "Consolas" )                 // set a font to suit yourself
   hb_gtinfo( HB_GTI_CLOSABLE,.F.)                          // Set so the [X] close button to the screen control box is OFF
                                                            // then the user cannot shut the program down without correctly closing any open datafiles/Tables
   //       NOTE! Whilst the [X] close is disabled, mininise and maximise will still operate normally and resize the screens correctly to your screen setting                                                           
   // --------------------------------------------------------------------------------------------------------------------------------------------------------
   // Open 1st MAIN PROGRAME
      DO HBR102Menu    // .prg  = default expected
   
CLOSE ALL 

RETURN NIL  // Main
//
*!*********************************************************************
*!
*!       Function: DATAOPEN()
*!
*!*********************************************************************
FUNCTION DATAOPEN(mExclOrNot)   &&  CALLED FROM 810 & 
   // From 110, 810 =F from 910 =T
   //
   // -------------------------------------------------------------------------------------------------------------------------------------
   // In this example the path is hardwired, which is not very flexible. Instead use a Table or Text file that the program can read.
   // The user will need a Function in the application or a separate program. Whilst you may issue software with a specific data location 
   // usually the user will require to have the data in a location that is suitable to them. Often on a network. Setting up the location 
   // of the data and saving the path into a textfile or Table by the user allows the program to access and open their data without error.
   // With either dBASE and DBF tables, or using an SQL database can be saved into the file. It is often simpler to have the local file,
   // Table or Textfile in the folder where the application .EXE resides. But your choice as a developer.
   // --------------------------------------------------------------------------------------------------------------------------------------
   //--------------------------------------------------------------------------
   // This example has only one Table, so the array has only one entry
   // It is included to show a file opening routine in place of a simple
   // SELECT : USE FILE NAME ALIAS etc
   // NOTE! The setting up of the Arrays for file opening would not normally 
   //       be in this location but away in another program in the program set
   //       as there will be so many arrays that would confuse in this function
   //--------------------------------------------------------------------------
   PRIVATE FILEREQD[2]
   FILEREQD := {1,0}
   
   PRIVATE FEXIT  := .F.                     && now starts file open
   PRIVATE FCOUNT := 3
   PRIVATE FOPEN  := 4     // This would be the total of the number of Tables in system +1
   PRIVATE mSELE
   PRIVATE pFILENAME
   PRIVATE pFALIAS
   // -------------------------------------------------------------
   // Following variables are set up above at the start of 101Start
   //DECLARE FILENAME[1], FILEALIA[1], FILESELE[1]
   //PRIVATE FILENAME:={"HBR81"}
   //PRIVATE FILEALIA:={"F81"}
   //PRIVATE FILESELE:={81}
   //FILENAME[1] := "HBR81"
   //FILEALIA[1] := "F81"
   //FILESELE[1] := 81
   //FOR FCOUNT = 1 TO FOPEN
   FCOUNT = 1
   //? FCOUNT
   //? fopen
   //-----------------------------------------------------------
   DO WHILE FCOUNT < FOPEN 
      
      **IF FILEREQD[FCOUNT] >0
      If FCOUNT < 4 
         mSELE := FILESELE[FCOUNT]
         SELECT &mSELE // FILESELE[FCOUNT]
         pFILENAME := FILENAME[FCOUNT]
         pFALIAS   := FILEALIA[FCOUNT]
         //
        **IF NET_USE1(pFILENAME,.F., pFALIAS, 32)           // Files not opened in Exclusive Mode
        IF NET_USE1(pFILENAME,mExclOrNot, pFALIAS, 32)      // Files not opened in Exclusive Mode
            * OPEN INDEXES LATER IN THIS EXAMPLE AS THERE CAN BE A VARYING NUMBER FOR EACH FROM 0 TO WHATEVER
         ELSE
            FEXIT = .T.
         ENDIF
        
         IF FEXIT
            FCOUNT = FOPEN+1
         ENDIF
      ENDIF
      //   
      FCOUNT := FCOUNT +1
      // 
   ENDDO
   //
   IF FEXIT    // Error on open
      * CLOSE DATA FILES
      DBCLOSEALL()
      ERRMSG1(12,20,"THERE ARE FILES LOCKED!", "PLEASE REPEAT OPENING FILES.......any key to continue")
      INKEY(0)
      @ 12,20 clear to 13,75
      RETURN (0)
   ENDIF  && endof opening routine
   //
RETURN (0)    &&  DATAOPEN
***************************
// ********************************************************************************************************************
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
FUNCTION CheckIfDataFound    // Used in Hbr102Menu
   // -----------------------------------------------------------------------------------
   // Used to check that data exists 
   // If data is not found Harbour crashes with unfriendly error
   // In an application the filenames would be in an Array and processed in turn to check
   // Any missing datafiles would be noted and the user advised so they could be located
   // Clearly this function would not be as simple as it is
   // Additionally, I suggest that a Table or text file in the program folder has the path
   // to the data. That could b eon a local drive, on on a UNC path on a network.
   // The setting of  the path to the data would either be a part of the application.
   // Or it could be a separate program for administrator use. If it is a part of the 
   // application, access to it can always have password access to prevent user access.
   // Maybe the path to the data is incorrect? Or Maybe the data is lost?
   // The main thing is to avoid DBFNTX/1001 or similar fatal errors 
   // -----------------------------------------------------------------------------------
   PRIVATE ISNotFound, mDataFile
    PRIVATE FILEREQD[2]
   FILEREQD := {1,0}
   Private mErrFile 
   mErrFile := 0
   mMissing := "-"
   mCheckFile :=":"
   PRIVATE FEXIT  := .F.                     && now starts file open
   PRIVATE FCOUNT := 1
   PRIVATE FOPEN  := 4     // This would be the total of the number of Tables in system +1
   PRIVATE mSELE
   PRIVATE pFILENAME
   PRIVATE pFALIAS
   //
   DO WHILE FCOUNT < FOPEN 
      
      **IF FILEREQD[FCOUNT] >0
      If FCOUNT < FOPEN
         //mSELE := FILESELE[FCOUNT]
         //SELECT &mSELE // FILESELE[FCOUNT]
         pFILENAME := ALLTRIM(FILENAME[FCOUNT]) +".dbf"
         mCheckFile := mCheckFile + pFILENAME +":"
         //pFALIAS   := FILEALIA[FCOUNT]
         //
         
         IF File(pFILENAME)      // checking for Files 
            //mFatalError = .F.
            // Found
         ELSE
            //mFatalError .T.
            mMissing := mMissing + pFileName +"-"
            mErrFile = mErrFile +1 
         ENDIF
      ENDIF  
      //   
      FCOUNT := FCOUNT +1
      // 
   ENDDO
   //
   Errmsg1(05,05, "FILES Checked", mCheckFile)
   IF mErrFile <> 0   // Error on missing files
      * CLOSE DATA FILES
      DBCLOSEALL()   // not really needed
      mFatalError = .T.
      ERRMSG1(12,10,"FILES MISSING FATAL ERROR!", mMissing + AllTrim(STR(mErrFile)) + " MISSING FILES.......any key to continue")
      INKEY(0)
      RETURN (.T.)
   ELSE 
      mFatalError = .F.
   ENDIF  && endof opening routine
   **//
   **mDataFile  := mCurpath + "HBR81.dbf"      // mCurPath set above in 101Start   
   **//
   **If FILE(mDataFile) 
   **   mFatalError = .F.
   **ELSE 
   **   mFatalError := .T.
   **EndIf
   //
   //
Return (mFatalError)
//
//
FUNCTION SetIndexes()
    // ----------------------------------------------------------------------------------------------------------
    // Check if Indexes exist & create if not
    // Open indexes for file system here
    // Of course you can use Macro & expansion and Arrays but arrays can be messy with many indexes to a table.
    // Better I think to make this Function application specific
    //-----------------------------------------------------------------------------------------------------------
    IndTest = .T.  // NIU
    PRIVATE mIndexFiles
    SELE 81
    mIndexFiles := mCurPath + "HBR81I1.NTX"
    mIndexFiles := "HBR81I1.NTX"
    If FILE(mIndexFiles) 
        // Found
        SET INDEX TO HBR81I1
    Else
        SET INDEX TO
        INDEX ON CUSTID to HBR81I1.ntx
        SET INDEX TO HBR81I1
    Endif
    //
    SELE 11
    mIndexFiles := "HBR11i1.NTX"
    if File(mIndexFiles)
        Set Index to HBR11i1                // stopped see below, HBR11I2
    Else
        INDEX ON TICKET TO HBR11i1.NTX
        Set INDEX TO HBR11i1
    Endif
    // NO INDEX TO 99
    //
RETURN (IndTest)
//
//    ############################################################################################### // ############################################
** TABLES ISSUED WITH THIS EXAMPLE  ********************************
**
** HBR11.dbf        TICKET      {Table for basic ticket entry )
**          FIELDS
**                CUSTID,    C,  8
**                TICKET,    C, 10
**                TDATE,     D,  8
**                PRODUCT,   C, 16           
**                QUANT,     N,  5.0
**                RATE,      N, 10.2
**                NETCALC,   N, 10.2
**                VATRATE,   N,  6.2
**                VATCALC,   N, 10.2
**                GROSSCALC, N, 10.2
**                
** HBR81.dbf        NAME & ADDRESS  (Simple/basic name and address file)
**          FIELDS
**                CUSTID,  C,  8
**                NAME,    C, 32
**                ADDR1,   C, 32
**                ADDR2,   C, 32
**                ADDR3,   C, 32
**                ADDR4,   C, 32
**                PCODE,   C, 10
**                TEL,     C, 16
**                EMAIL,   C, 32
**                CONTACT, C, 32
**                NOTES,   C, 48
**                 
** HBR99.dbf        SYSTEM MASTER   (No user access to this table in system supplied) 
**          FIELDS 
**                RECNO99,  N, 10.2
**                NEWTKTNO, N, 10.0
**                VATRATE,  N,  6.2
**                CODESUFX, N, 10.0
**                CODEPRFX, N,  1.0
**
** END OF TABLE & STRUCTURE *****************************************
//    
// NAMING SYSTEM USED 
//      See the document COTSWOLD NAMING SYSTEM
//
//    ############################################################################################ // ###############################################           
// +----------------------+
// | END OF HBR101Start   |
// +----------------------+
//

