//+-------------------------------------------------------------------------------------------------------------+
//|     Copyright© W.Greenwood England 1983-2024                                                                |
//|     This free software is released under the MIT License.                                                   |
//|     See https://en.wikipedia.org/wiki/MIT_License                                                           |  
//|     Copyright © 1983-2024 WGREENWOOD                                                                        |
//|                                                                                                             |
//|     HBRLIB24.prg                                                                                            |
//|  Issued as a part my ADDRESS & TICKET example which is issued as an example of a simple Harbour program     |
//|  I have only included the Functions that are used by the three example programs                             |
//|      HBR101Start                                                                                            |
//|      HBR102Menu     some functions removed for simplification of Library example                            |                                        
//|      HBR810Addr       - ditto -                                                                             |
//|      HBR910Rein       - ditto -                                                                             |
//|      HBRLIB24         - ditto -                                                                             |                        
//|                                                                                                             |
//+-------------------------------------------------------------------------------------------------------------+
****
*  
******************************************************************************
// ------------------------------------------------------------------------------------------------------------------
** This Lib originated in Clipper87 and has been updated. Some functions are taken from Clipper Tools
// NOTE! Some of these Functions & Procs may be using Clipper87 code that is out of date in Harbour & Clipper5.3
//       These Functions have been carried over from Clipper87 and modified where necessary.
//       Sonme of the Functions below have been replaced by built-in functions in Harbour.
//       However, all of the Functions used do appear to work and later code here may be more efficient than the earlier code
//       There are mentions of Functions below that are not included in this HBR Address example.
//       Functions that are included are marked *##* below 
//
//       I would usually have a Master Library file used in all programs and not one individual to an application.
//       Using more that one Library can have obvious over time as changes and updates are added/
// ------------------------------------------------------------------------------------------------------------------
 *
   *##*  Contains: NET_USE()             (Params: FILE, EX_USE, WAIT)
   *##*  Contains: NET_USE1()            (Params: FILE, EX_USE, ALYAS, WAIT)
   *##*  Contains: AddressLine5()        (Params:Addr1, Addr2, Addr3, Addr4, Pcode)
   *     Contains: NET_USE3()            (Params: FILE, EX_USE, ALIASREF, WAIT)
   *     Contains: FIL_LOCK()            (Params: WAIT)
   *##*  Contains: REC_LOCK()            (Params: WAIT)
   *##*  Contains: ADD_REC()             (Params: WAIT)
   *     Contains: RED_BOXV()            (Params: TLIN,TCOL,BLIN,BCOL)
   *     Contains: RED_BOX()             (Params: SIZE)
   *     Contains: RED_BOX2              (Params: none)
   *     Contains: RED_BOX3              (Params: none)
   *     Contains: COLORSET              (Params: none)
   *     Contains: RESPONSE              (Params: none)
   *     Contains: RESPOND               (Params:     )
   *     Contains: DATOWEEK              (Params: none)
   *     Contains: DATOFRI               (Params: none)
   *     Contains: DATOTHU               (Params: none)
   *     Contains: F_DATE6()             (Params: FDATE)
   *     Contains: DEC2()                (Params: anyVarType)  to return a decimal number from any VarType, usually a long but will prevent a Null being entered or used
   *     Contains: Dec3()                (Params:      -ditto -  also converting an integer to a decimal
   *     Contains: Intyger()             (Params:      return an integer from any Vartype 
   *     Contains: LongInty()            (Params:      - ditto - but Long   (Ex Access DBSMaths()) 
   *     Contains: CSTMR                 (Params: none)
   *##*  Contains: LEGEND()              (Params: L_LINE,L_NAME)
   *##*  Contains: LEGENDBX()            (Params: L_LINE,NAME,BX)
   *     Contains: NOWEEKS()             (Params: YW1,YW2)
   *     Contains: MONTHADD              (Params: none)
   *     Contains: DELEDFLT()            (Params: L1,C1,L2,C2)
   *     Contains: STRPD()               (Params: ISDATE)
   *     Contains: NEXT1                 (Params: none)
   *     Contains: BACK1                 (Params: none)
   *##*  Contains: ARROWOFF()            (Params: none)
   *     Contains: ARROWON()             (Params: none)
   *##*  Contains: ARROWON2()            (Params: none)
   *     Contains: ALTERIT()             (Params: L1,C1,L2,C2,TITLE1)
   *     Contains: SELECTOR()            (Params: TL,TC,BL,BC,AR01,AR02,MESG)
   *     Contains: ERRMSG()              (Params: LC02,AC02,TTL, MSG, OPTION, PROMPT) Y/N/Q, , Exclam/Fatal/ etc 
   *##*  Contains: ERRMSG1()             (Params: LC02,AC02,TTL,MSG)
   *     Contains: BOXDRAW()             (Params: L_LINE1,L_LINE2,SIZEOF,BRDR)
   *##*  Contains: SKIPB                 (Params: none)
   *##*  Contains: SKIPN                 (Params: none)
   *     Contains: ZEROFILL()            (Params: Z01,Z02)
   *     Contains: REPLOCK()             (Params: RL)
   *     Contains: REPLADD()             (Params: RL)
   *     Contains: DOITBOX2()            (Params: L1,C1,TTL,NO,P1,P2,OK1)
   *     Contains: DATEREV()             (Params: RDATE)
   *     Contains: NOTZERO()             (Params: V_TOX)
   *     Contains: SETLPTON()            (Params: none)
   *     Contains: PRINTOKX()            (Params: none : returns 0=abandon print or 2 continue
   *     Contains: STNDRDWN()            (Params: S_DATE)
   *     Contains: CLOG()                (Params: none ) key part 3
   *     Contains: PC0DE()               (Params: pPOSTCODE) locate/lookup an address from all or part of Postcode in Postcode datafile
   *     Contains: CSETSN0W()            (Params: none ) cee part 1
   *     Contains: PGETNAME()            (Params: none ) cee part 4
   //*   Contains: MENULINE()            (Params: none )
   *     Contains: GETDNGL()             (Params: none )
   *##*  Contains: WHICH1()              (Params: row,col,string delimited with :'s)
   *                                     (allows a menu with <=9 items & returns number)
   *     Contains: FINAL()               (Params: row,col uses which1
   *     Contains: BOXLINE()             (Params: row,col,top space,bottom space,wide)
   *##*  Contains: SHADOWR()              (Params: row,col,row,col) see thro shadow
   *     Contains: NOTBLANK()            (Params: memvar,line,column
   *     Contains: *******               (Params: none)	&& Security & client name validadtion
   //*   Contains: SYSTITLE()            (Params: "NAME",lename,linecount)displays name on front screen
   *     Contains: MEMLIST               (Params: none is PROCEDURE display memory )
   *     Contains: UCASE1st()            (Params: (Var or Field)  Will upper case 1st char of each word in a string if lowercase
   *     Contains: WHICH2()              (Params: Line,row,selections returns number0 up, as which1 but no screen restore)
   *     Contains: FOPENER1()            (Params: file number, could replace dataopen() )
   *     Contains: F_DBEDIT()            (MASK ONLY )
   *     Contains: NON2VIEW()            (Params: none simply returns a no records in file if empty on view etc.)
   *     Contains: DATERANGE()           (params:NONE - RETURNS 16CHAR STRING DATE1+DATE2)
   *     Contains: FIRSTDOM()            (Parame: Date) returns first day in month
   *     Contains: LASTDOM()             (Params: Date)  Returns last day of month
   *     Contains: FIRSTWKNO()           (Params: Year)  Returns the date of first date in Week No 1
   *
 *
 //----------------------------------------------------------------------------------------
 FUNCTION NET_USE
   PARAMETERS file, ex_use, wait
	PRIVATE forever
	forever :=(wait=0)
	DO WHILE (forever .OR. wait > 0)
		IF ex_use                    && exclusive
			USE &file EXCLUSIVE
		ELSE
			USE &file      && shared
		ENDIF
		IF .NOT. NETERR()           && USE succeeds
			RETURN (.T.)
		ENDIF
		INKEY(1) 	          	&& wait 1 second
		wait :=wait - 1
	ENDDO
RETURN (.F.)			&& USE fails 
* END NETUSE()
// 
FUNCTION NET_USE1   && modified from net_use
   PARAMETERS Tfile, ex_use, ALYAS, wait      && alias added
   PRIVATE forever
   forever :=(wait=0)
   DO WHILE (forever .OR. wait > 0)
      IF ex_use                    && exclusive
         USE &Tfile EXCLUSIVE ALIAS &ALYAS     // 
      ELSE
         USE &Tfile ALIAS &ALYAS      && shared
      ENDIF
      IF .NOT. NETERR()           && USE succeeds
         RETURN (.T.)
      ENDIF
      INKEY(1) 	          	&& wait 1 second
      wait :=wait - 1
   ENDDO
RETURN (.F.)			&& USE fails
* End - NET_USE1
//
//
FUNCTION ARROWON2
   * NO PARAMS
   SET KEY  4 TO SKIPN      && moves to next #, program needs says following case
   SET KEY 24 TO SKIPN      &&
   SET KEY  5 TO SKIPB      &&
   SET KEY 19 TO SKIPB     && next1 & back1 in locks
RETURN 0          // ArrowOn2()
//
FUNCTION ARROWOFF
   * * NO PARAMS
   SET KEY 4  TO      && moves to next #, program needs says following case
   SET KEY 5  TO      &&
   SET KEY 19 TO      &&
   SET KEY 24 TO      && next1 & back1 in locks
RETURN 0       // ArrowOff()
//
*
PROCEDURE SKIPB
   SKIP -1
   IF BOF()
      GO TOP
   ENDIF
   KEYBOARD CHR(32)
RETURN         // SkipB
*
//
*
PROCEDURE SKIPN
   SKIP
   IF EOF()
      GO BOTT
   ENDIF
   KEYBOARD CHR(32)
RETURN
*
* FUNCTION  LEGEND
* LEGEND(04,"NAME OF APPLICATION")  << IN PROGRAM
FUNCTION LEGEND
   PARAMETERS L_LINE,L_NAME
   @ L_LINE,39-(LEN(L_NAME)/2) SAY L_NAME
   RETURN (.F.)
* ENDOF LEGEND
//
*
FUNCTION LEGENDBX
   * LEGENDBX(05,"NAME OR LEGEND","D") < in program
   PARAMETERS L_LINE,Z_NAME,BX
   IF PCOUNT() <>3
      BX :="D"
   ENDIF
   IF BX = "D"  && double
      @ L_LINE-1,(39 -((LEN(Z_NAME)/2) +2)) TO L_LINE+1,(39 -((len(Z_name)/2) +3) +LEN(Z_NAME)+4) DOUBLE
      @ L_LINE,(39 -LEN(Z_NAME)/2)-1 SAY SPACE(LEN(Z_NAME)+2)
      @ L_LINE,(39 -LEN(Z_NAME)/2) SAY Z_NAME
   ELSE
      @ L_LINE-1,(39 -((LEN(Z_NAME)/2) +2)) TO L_LINE+1,(39 -((len(Z_name)/2) +3) +LEN(Z_NAME)+4)
      @ L_LINE,(39 -LEN(Z_NAME)/2)-1 SAY SPACE(LEN(Z_NAME)+2)
      @ L_LINE,(39 -LEN(Z_NAME)/2) SAY Z_NAME
   ENDIF
RETURN (.F.)
* ENDOF FUNCTION LEGENDBX()
*
//
FUNCTION ERRMSG1
   PARAMETERS LC02,AC02,TITLE,MSG
   COLORSET :=SETCOLOR()
   PRIVATE LenT, LenM, mTitle, mMsg
   lenT := Len(Title)
   lenM := Len(Msg)
   If lenM >lenT 
      mTitle := Left((TITLE +Space(lenM)),lenM)
      mMsg   :=MSG
   Else
      mMsg   := Left((Msg +Space(lenT)),lenT)
      mTitle := TITLE
   Endif
   IF ISCOLOR()
      SETCOLOR(cBlkYellow)   // ERRCOLOR
   ENDIF
   
   @ LC02,  AC02 SAY mTitle  // MSG
   @ LC02+1,AC02 Say mMsg
   SETCOLOR(COLORSET)
RETURN (0)     // ErrMsg()
*
//
FUNCTION REC_LOCK 
   PARAMETERS wait
   PRIVATE forever
   IF RLOCK()
      RETURN (.T.)		&& locked
   ENDIF
   forever :=(wait :=0)
   DO WHILE (forever .OR. wait > 0)
      IF RLOCK()
         RETURN (.T.)		&& locked
      ENDIF
      INKEY(.5)			&& wait 1/2 second
      wait :=wait - .5
   ENDDO
RETURN (.F.)			&& not locked
* End - REC_LOCK
****
//
****
*   ADD_REC function
*  Returns true if record appended.  The new record is current
*  and locked.
*  Pass the following parameter
*    1. Numeric - seconds to wait (0 :=wait forever)
FUNCTION ADD_REC
   PARAMETERS wait
   PRIVATE forever
   APPEND BLANK
   IF .NOT. NETERR()
      RETURN (.T.)
   ENDIF
   forever :=(wait :=0)
   DO WHILE (forever .OR. wait > 0)
      APPEND BLANK
      IF .NOT. NETERR()
         RETURN .T.
      ENDIF
      INKEY(.5)			&& wait 1/2 second
      wait :=wait - .5
   ENDDO
RETURN (.F.)			&& not locked
* End ADD_REC
*
// --------------------------------NR/NA
*FUNCTION PRINTOKX
* *****************************
*RETURN (P_OK)  && PrintOKX()
*
// --------------------------^^ NR/NA
//
FUNCTION WHICH1
   PARAMETERS R1,C1,TEMP
   PRIVATE R1,C1,TEMP,WHICHONE,POS,TITLE,TEMP1,Kount,LONGEST,which,COL_W
   PRIVATE TEMPA[NUMAT(":",TEMP)]
   SET ESCAPE OFF
   WHICHONE :=0
   DECLARE TEMPA[NUMAT(":",TEMP)]
   IF NUMAT(":",TEMP) <3 .OR. NUMAT(":",TEMP) >9
      RETURN (1)
   ENDIF
   AFILL(TEMPA," ")
   WHICH  :=SAVESCREEN(0,0,MAXROW(),MAXCOL())
   COL_W  :=SETCOLOR()
   SETCOLOR(cBlkYellow)
   POS :=AT(":",TEMP)
   IF POS = 0
      RESTSCREEN(0,0,MAXROW(),MAXCOL(),WHICH)
      rele WHICH,POS
      SETCOLOR(cBlkYellow)
      RETURN (1)
   ENDIF
   TEMPA[1] :=" "+ SUBSTR(TEMP,1,POS-1) +" "
   LONGEST :=LEN(TEMPA[1])
   XTITLE :=SUBSTR(TEMP,(POS+1),(LEN(TEMP)-POS) )
   Kount :=2
   DO WHILE Kount <=NUMAT(":",TEMP)
      POS :=AT(":",XTITLE)
      IF POS = 0
         Kount :=12
      ELSE
         TEMPA[Kount] :=" "+ SUBSTR(XTITLE,1,POS-1) +" "
         IF LEN(TEMPA[Kount]) >LONGEST
            LONGEST :=LEN(TEMPA[Kount])
         ENDIF
         XTITLE :=SUBSTR(XTITLE,(POS+1),(LEN(XTITLE)-POS) )
      ENDIF
      Kount :=Kount +1
   ENDDO
   Kount =1
   DO WHILE Kount <=NUMAT(":",TEMP)
      TEMPA[Kount] :=SUBSTR(TEMPA[Kount]+SPACE(20),1,LONGEST)
      Kount :=Kount +1
   ENDDO
   *************** BOX & LINE UNDER HEADING
   @ R1,C1 CLEAR TO R1+(1+NUMAT(":",TEMP)),(C1+1+LONGEST +2)
   @ R1,C1       TO R1+(2+NUMAT(":",TEMP)),(C1+1+LONGEST +2) DOUBLE
   @ R1+2,C1 SAY CHR(199)
   @ R1+2,C1+1 SAY REPLICATE(CHR(196),(LONGEST+2) )
   @ R1+2,(C1+1+LONGEST+2) SAY CHR(182)
  
   SHADOWR(R1,C1,(R1+(2+NUMAT(":",TEMP))),(C1+1+LONGEST +2) )
    //SETCOLOR(PRGCOLOR)  && setcolor(DBWHITE)
   *ENDIF
   **************************************************************
   Kount :=1
   @ R1+Kount,C1+2 SAY TEMPA[Kount]   && TITLE
   Kount :=Kount +1
   @ R1+Kount+1,C1+2 PROMPT TEMPA[Kount]   && 1
   Kount :=Kount +1
   @ R1+Kount+1,C1+2 PROMPT TEMPA[Kount]   && 2
   Kount :=Kount +1
   IF Kount <= NUMAT(":",TEMP)
      @ R1+Kount+1,C1+2 PROMPT TEMPA[Kount]   && 3
   ENDIF
   Kount :=Kount +1
   IF Kount <= NUMAT(":",TEMP)
      @ R1+Kount+1,C1+2 PROMPT TEMPA[Kount]   && 4
   ENDIF
   Kount :=Kount +1
   IF Kount <= NUMAT(":",TEMP)
      @ R1+Kount+1,C1+2 PROMPT TEMPA[Kount]   && 5
   ENDIF
   Kount :=Kount +1
   IF Kount <= NUMAT(":",TEMP)
      @ R1+Kount+1,C1+2 PROMPT TEMPA[Kount]   && 6
   ENDIF
   Kount :=Kount +1
   IF Kount <= NUMAT(":",TEMP)
      @ R1+Kount+1,C1+2 PROMPT TEMPA[Kount]   && 7
   ENDIF
   Kount :=Kount +1
   IF Kount <= NUMAT(":",TEMP)
      @ R1+Kount+1,C1+2 PROMPT TEMPA[Kount]   && 8
   ENDIF
   Kount :=Kount +1
   IF Kount <= NUMAT(":",TEMP)
      @ R1+Kount+1,C1+2 PROMPT TEMPA[Kount]   && 9  && MAX ALLOWED
   ENDIF
   Kount :=Kount +1
   MENU TO WHICHONE
   IF LASTKEY() = 27 .OR. LASTKEY() =18
      WHICHONE :=0
   ENDIF
   *
   *SETCOLOR(PRGCOLOR)
   //SETCOLOR(COL_W)
   RESTSCREEN(0,0,MAXROW(),MAXCOL(),WHICH)
   RELE WHICH,COL_W
   SET ESCAPE ON
RETURN (WHICHONE)    && ENDOF WHICH1
*
//
*
FUNCTION SHADOWR
   PARAMETERS ROW1,COL1,ROW2,COL2
   PRIVATE WINDOW, X
   IF PCOUNT() = 4
      WINDOW :=SAVESCREEN(ROW1+1,COL2+1,ROW2+1,COL2+1)
      FOR X=2 TO LEN(WINDOW) STEP 2
         WINDOW :=STUFF(WINDOW,X,1,CHR(8))
      NEXT
      RESTSCREEN(ROW1+1,COL2+1,ROW2+1,COL2+1,WINDOW)
      WINDOW :=SAVESCREEN(ROW2+1,COL1+2,ROW2+1,COL2+1)
      FOR X=2 TO LEN(WINDOW) STEP 2
         WINDOW :=STUFF(WINDOW,X,1,CHR(8))
      NEXT
      RESTSCREEN(ROW2+1,COL1+2,ROW2+1,COL2+1,WINDOW)
      RELE WINDOW
   ENDIF
RETURN (0)     // ShadowR()
*
//
// 
Function AddressLine5(Addr1, Adddr2, Addr3, Addr4,Pcode) 
   **  241829 converted to Harbour X Access
   ** to store address & postcode into one line
   Private mAd1, mAd2, mAd3, mAd4, mAd5, AddressLine5 
   mAd1 := Left((Addr1 + Space(50)), 50)	// Single line of an Address unlikely to be longer that 50
   mAd2 := Left((Addr2 + Space(50)), 50)
   mAd3 := Left((Addr3 + Space(50)), 50)
   mAd4 := Left((Addr4 + Space(50)), 50)
   mAd5 := Left((Pcode + Space(50)), 10)
   If Len(AllTrim(mAd1)) > 1    // not default empty string or empty string
      **If Left(mAd1,1) <> Space(1) And Len(AllTrim(mAd1)) > 1    // not default empty string or empty string
       AddressLine5 := AllTrim(mAd1)
   Else
       AddressLine5 := Space(1) 
   EndIf
   If Len(AllTrim(mAd2)) > 1  // something there
      **If Left(mAd2,1) <> Space(1) And Len(AllTrim(mAd2)) > 1  // something there
       If Len(AllTrim(AddressLine5)) > 0 
           AddressLine5 := AddressLine5 + ", " + AllTrim(mAd2)
       Else
           AddressLine5 := AllTrim(mAd2)
       EndIf
   EndIf
   If Len(AllTrim(mAd3)) > 1  // something there
      **If Left(mAd3,1) <> Space(1) And Len(AllTrim(mAd3)) > 1  // something there
       AddressLine5 := AddressLine5 + ", " + AllTrim(mAd3)
   EndIf
   If Len(AllTrim(mAd4)) > 1    // something there
      **If Left(mAd4,1) <> Space(1) And Len(AllTrim(mAd4)) > 1    // something there
       AddressLine5 := AddressLine5 + ", " + AllTrim(mAd4)
   EndIf
   If Len(AllTrim(mAd5)) > 1  // something there
      **If Left(mAd5,1) <> Space(1) And Len(AllTrim(mAd5)) > 1  // something there
       AddressLine5 := AddressLine5 + ", " + AllTrim(mAd5)
   EndIf
   AddressLine5 := AllTrim(AddressLine5)
   **
   RETURN (AddressLine5)
    //
//End Function    // AddressLine5
//
// END HBRDemoLIB24 ----------------------------------------------|
//
