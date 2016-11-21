

   MEMBER('github1.clw')                                   ! This is a MEMBER module

                     MAP
                       INCLUDE('GITHUB1003.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
DGMessage            PROCEDURE  (STRING ptext,<STRING pcaption>,<STRING picon>,<STRING pbuttons>,UNSIGNED pdefault,UNSIGNED pstyle) ! Declare Procedure
!******************************************************************************
! free to use, provided as is, no warranty provided
! this code cannot be included in other toolsets and resold for profit
!
! by Richard Bryce
! https://au.linkedin.com/in/richardbryce  
! http://northshoreit.com.au
!******************************************************************************

! calls DBSearchReplace() also

returnValue         unsigned

loopvar             Long
DisplayText         CString(4001)
Chartosearch        CString(41) 
CharToReplace       CString(41)

!canCopy             Byte(False)

! this is all for the button processing
ButtonNumber2       Long

buttonfeq           signed,dim(8)
returnValues        long,dim(8)


buttonok            byte
buttonyes           byte
buttonno            byte
buttonabort         byte
buttonretry         byte
buttonignore        byte
buttoncancel        byte
buttonhelp          byte

buttonUsed          byte ! count

Window WINDOW('[title]'),AT(,,350,190),IMM,AUTO,FONT('Arial',,,FONT:regular,CHARSET:DEFAULT),ALRT(EscKey),DOUBLE
        BUTTON,AT(3,8,30,30),USE(?IconDisplay),SKIP,ICON(ICON:Question),FLAT
        TEXT,AT(37,8,245,176),USE(displayText),SKIP,TRN,FLAT,CENTER,FONT(,14),READONLY
        BUTTON('&OK'),AT(285,8,60,18),USE(?Button1),FONT(,8),ICON(ICON:None),ALRT(DownKey), ALRT(UpKey)
        BUTTON('&OK'),AT(285,30,60,18),USE(?Button2),FONT(,8),ICON(ICON:None),ALRT(DownKey), ALRT(UpKey)
        BUTTON('&OK'),AT(285,53,60,18),USE(?Button3),FONT(,8),ICON(ICON:None),ALRT(DownKey), ALRT(UpKey)
        BUTTON('&OK'),AT(285,76,60,18),USE(?Button4),FONT(,8),ICON(ICON:None),ALRT(DownKey), ALRT(UpKey)
        BUTTON('&OK'),AT(285,98,60,18),USE(?Button5),FONT(,8),ICON(ICON:None),ALRT(DownKey), ALRT(UpKey)
        BUTTON('&OK'),AT(285,120,60,18),USE(?Button6),FONT(,8),ICON(ICON:None),ALRT(DownKey), ALRT(UpKey)
        BUTTON('&OK'),AT(285,143,60,18),USE(?Button7),FONT(,8),ICON(ICON:None),ALRT(DownKey), ALRT(UpKey)
        BUTTON('&OK'),AT(285,166,60,18),USE(?Button8),FONT(,8),ICON(ICON:None),ALRT(DownKey), ALRT(UpKey)
    END

  CODE
   ! Displays a message dialog box and returns the button the user pressed

   ! returns      An unsigned number representing the number of the button the user presses to exit the dialog box

   returnValue = 0
   displayText = pText
   If displayText = '' Then displayText = '[the message box display text string is blank]!'.



   Open(Window)

   ! text field, string replace pipe with the carraige return/line feed
   ! "text"       A string constant or variable containing the text to display in the message box. 
   !              A vertical bar (|) in the text indicates a line break for multi-line messages. 
   !              Including an ASCII 9 in the text inserts a tab for text alignment.
      Chartosearch  = '|'
      CharToReplace = '<13,10>'
      DGSearchReplace (displayText, ChartoSearch, 1, CharToReplace) 

      
   ! caption-window title
   ! "caption"    The dialog box title. If omitted, the dialog has no title.
      If Omitted(pCaption) = 1
         0{Prop:Text} = 'Message'
      Else
         0{Prop:Text} = Clip(Left(pCaption))
      .                         

      ?displayText{Prop:Height} = (?displayText{PROP:LineCount}+1) * ?displayText{PROP:LineHeight}

   
   ! picon
   ! "icon"       A string constant or variable naming the .ICO file to display, or an EQUATE for one of Windows' standard icons
   !              (these EQUATEs are listed in EQUATES.CLW). If omitted, no icon is displayed on the dialog box.      
   Do IconProcessing

   
   ! "buttons"    Either an integer expression which indicates which Windows standard buttons (may indicate multiple buttons) to place on the dialog box,
   !              or a string expression containing a vertical bar (|) delimited list of the text for up to 8 buttons. 
   !              If omitted, the dialog displays an Ok button.       
   buttonok     = false
   buttonyes    = false
   buttonno     = false
   buttonabort  = false
   buttonretry  = false
   buttonignore = false
   buttoncancel = false
   buttonhelp   = false

   buttonfeq[1] = ?Button1
   buttonfeq[2] = ?Button2
   buttonfeq[3] = ?Button3
   buttonfeq[4] = ?Button4
   buttonfeq[5] = ?Button5
   buttonfeq[6] = ?Button6
   buttonfeq[7] = ?Button7
   buttonfeq[8] = ?Button8

   Do ButtonsProcessing



   ! "style"      The style parameter is a bitmap integer constant, variable, EQUATE, or expression that specifies the type of modal behavior, 
   !              and whether or not the text of the message can be copied to the Windows Clipboard.


   !The following list shows the EQUATEs used in the style parameter:
   !MSGMODE:SYSMODAL - MODAL has no effect for 32-bit applications, and has been deprecated in this release. The Microsoft Win32 API does not support system modal windows
   !MSGMODE:CANCOPY
   !MSGMODE:FIXEDFONT  
   
   If pstyle > 0

      If pstyle = MSGMODE:SYSMODAL
         0{Prop:Modal} = 1 ! see note above, probably disabled in the windows environment by microsoft
      .

      !If pStyle = MSGMODE:CANCOPY       
      !   canCopy = True
      !.                                                           

      If pStyle = MSGMODE:FIXEDFONT
         ?displayText{PROP:FontName}       = 'Lucida Console'   !'Courier'
      .
   .


   ! resize the window
   If (?displayText{Prop:yPos} + ?displayText{Prop:Height}) > (buttonfeq[buttonused]{prop:yPos} + buttonfeq[buttonused]{prop:Height})
      0{Prop:Height} = (?displayText{Prop:yPos} + ?displayText{Prop:Height})  + 20 
   Else
      0{Prop:Height} = (buttonfeq[buttonused]{prop:yPos} + buttonfeq[buttonused]{prop:Height}) + 20
   .      

   0{Prop:Center} = 1


   Accept

     Case Event()
       Of Event:OpenWindow
          ! "default"    An integer constant, variable, EQUATE, or expression which indicates the default button on the dialog box. 
          !              If omitted, the first button is the default.
          If Omitted(pDefault) = 1
             Select(?Button1)
          Else
             If pDefault > 0 Then pDefault -= 1.
             Select( (?Button1+pDefault) )
          .
   
       Of Event:CloseWindow    
       
       Of EVENT:AlertKey
          If Keycode() = EscKey Then Cycle.
     .

     Case Accepted()

       Of ?Button1
          returnValue = returnValues[1]
          Post(Event:CloseWindow)

       Of ?Button2
          returnValue = returnValues[2]
          Post(Event:CloseWindow)

       Of ?Button3
          returnValue = returnValues[3]
          Post(Event:CloseWindow)

       Of ?Button4
          returnValue = returnValues[4]
          Post(Event:CloseWindow)

       Of ?Button5
          returnValue = returnValues[5]
          Post(Event:CloseWindow)

       Of ?Button6
          returnValue = returnValues[6]
          Post(Event:CloseWindow)

       Of ?Button7
          returnValue = returnValues[7]
          Post(Event:CloseWindow)

       Of ?Button8
          returnValue = returnValues[8]
          Post(Event:CloseWindow)
     End !End_CaseAccepted


     Case Focus()
       Of ?Button1
          Case Event()
            Of Event:PreAlertKey
               Case Keycode()
                 Of UpKey
                 OrOf DownKey
                    Cycle
               End
               
            Of Event:AlertKey
               Case Keycode()
                 Of UpKey
                    Select(?-1)
                    
                 Of DownKey
                    Select(?+1)
               End
          End
          
       Of ?Button2
          Case Event()
            Of Event:PreAlertKey
               Case Keycode()
                 Of UpKey
                 OrOf DownKey
                    Cycle
               End
               
            Of Event:AlertKey
               Case Keycode()
                 Of UpKey
                    Select(?-1)
                    
                 Of DownKey
                    Select(?+1)
               End
          End
          
       Of ?Button3
          Case Event()
            Of Event:PreAlertKey
               Case Keycode()
                 Of UpKey
                 OrOf DownKey
                    Cycle
               End
               
            Of Event:AlertKey
               Case Keycode()
                 Of UpKey
                    Select(?-1)
                    
                 Of DownKey
                    Select(?+1)
               End
          End
          
       Of ?Button4
          Case Event()
            Of Event:PreAlertKey
               Case Keycode()
                 Of UpKey
                 OrOf DownKey
                    Cycle
               End
               
            Of Event:AlertKey
               Case Keycode()
                 Of UpKey
                    Select(?-1)
                    
                 Of DownKey
                    Select(?+1)
               End
          End
          
       Of ?Button5
          Case Event()
            Of Event:PreAlertKey
               Case Keycode()
                 Of UpKey
                 OrOf DownKey
                    Cycle
               End
               
            Of Event:AlertKey
               Case Keycode()
                 Of UpKey
                    Select(?-1)
                    
                 Of DownKey
                    Select(?+1)
               End
          End
          
       Of ?Button6
          Case Event()
            Of Event:PreAlertKey
               Case Keycode()
                 Of UpKey
                 OrOf DownKey
                    Cycle
               End
               
            Of Event:AlertKey
               Case Keycode()
                 Of UpKey
                    Select(?-1)
                    
                 Of DownKey
                    Select(?+1)
               End
          End
          
       Of ?Button7
          Case Event()
            Of Event:PreAlertKey
               Case Keycode()
                 Of UpKey
                 OrOf DownKey
                    Cycle
               End
               
            Of Event:AlertKey
               Case Keycode()
                 Of UpKey
                    Select(?-1)
                    
                 Of DownKey
                    Select(?+1)
               End
          End
          
       Of ?Button8
          Case Event()
            Of Event:PreAlertKey
               Case Keycode()
                 Of UpKey
                 OrOf DownKey
                    Cycle
               End
               
            Of Event:AlertKey
               Case Keycode()
                 Of UpKey
                    Select(?-1)
                    
                 Of DownKey
                    Select(?+1)                    
               End
          End
          

          


     End !End_Case Focus()

     
   End

   Close(Window)

   Return returnValue
   
   
IconProcessing Routine

   !! if no icon them move the controls left...
   !If Omitted(pIcon) = 1 
   !   
   !   ?IconDisplay{Prop:Hide} = 1                                                
   !
   !   Loop loopvar = ?displaytext to ?Button8 By 1
   !      loopvar{prop:xPos} = loopvar{prop:xPos} - ?IconDisplay{Prop:Width}
   !   .
   !   0{Prop:Width} = 0{Prop:Width} - ?IconDisplay{Prop:Width}
   !
   !   Exit
   !.                                      
         
   !picon = lower(clip(picon))
   
   
   If pIcon = ''
      ! use icon:exclamation if no icon specified
      ?IconDisplay{Prop:Icon} = icon:exclamation

   elsif InString('.ico', lower(clip(picon)), 1, 1) > 0
      ?IconDisplay{Prop:Icon} = lower(clip(picon))

   elsif InString('.gif', lower(clip(picon)), 1, 1) > 0
      ?IconDisplay{Prop:Icon} = lower(clip(picon))

   elsif InString('.jpg', lower(clip(picon)), 1, 1) > 0
      ?IconDisplay{Prop:Icon} = lower(clip(picon))

   elsif InString('.pcx', lower(clip(picon)), 1, 1) > 0
      ?IconDisplay{Prop:Icon} = lower(clip(picon))

   else                                                  
      ?IconDisplay{Prop:Icon} = lower(clip(picon))

!          If clip(picon) = 'icon:none'              Then ?IconDisplay{Prop:Icon} = icon:none.
!          If clip(picon) = 'icon:application'       Then ?IconDisplay{Prop:Icon} = icon:application.
!          If clip(picon) = 'icon:hand'              Then ?IconDisplay{Prop:Icon} = icon:hand.
!          If clip(picon) = 'icon:question'          Then ?IconDisplay{Prop:Icon} = icon:question.
!          If clip(picon) = 'icon:exclamation'       Then ?IconDisplay{Prop:Icon} = icon:exclamation.
!          If clip(picon) = 'icon:asterisk'          Then ?IconDisplay{Prop:Icon} = icon:asterisk.
!          If clip(picon) = 'icon:pick'              Then ?IconDisplay{Prop:Icon} = icon:pick.
!          If clip(picon) = 'icon:save'              Then ?IconDisplay{Prop:Icon} = icon:save.
!          If clip(picon) = 'icon:print'             Then ?IconDisplay{Prop:Icon} = icon:print.
!          If clip(picon) = 'icon:paste'             Then ?IconDisplay{Prop:Icon} = icon:paste.
!          If clip(picon) = 'icon:open'              Then ?IconDisplay{Prop:Icon} = icon:open.
!          If clip(picon) = 'icon:new'               Then ?IconDisplay{Prop:Icon} = icon:new.
!          If clip(picon) = 'icon:help'              Then ?IconDisplay{Prop:Icon} = icon:help.
!          If clip(picon) = 'icon:cut'               Then ?IconDisplay{Prop:Icon} = icon:cut.
!          If clip(picon) = 'icon:copy'              Then ?IconDisplay{Prop:Icon} = icon:copy.
!          If clip(picon) = 'icon:child'             Then ?IconDisplay{Prop:Icon} = icon:child.
!          If clip(picon) = 'icon:frame'             Then ?IconDisplay{Prop:Icon} = icon:frame.
!          If clip(picon) = 'icon:clarion'           Then ?IconDisplay{Prop:Icon} = icon:clarion.
!          If clip(picon) = 'icon:noprint'           Then ?IconDisplay{Prop:Icon} = icon:noprint.
!          If clip(picon) = 'icon:zoom'              Then ?IconDisplay{Prop:Icon} = icon:zoom.
!          If clip(picon) = 'icon:nextpage'          Then ?IconDisplay{Prop:Icon} = icon:nextpage.
!          If clip(picon) = 'icon:prevpage'          Then ?IconDisplay{Prop:Icon} = icon:prevpage.
!          If clip(picon) = 'icon:jumppage'          Then ?IconDisplay{Prop:Icon} = icon:jumppage.
!          If clip(picon) = 'icon:thumbnail'         Then ?IconDisplay{Prop:Icon} = icon:thumbnail.
!          If clip(picon) = 'icon:tick'              Then ?IconDisplay{Prop:Icon} = icon:tick.
!          If clip(picon) = 'icon:cross'             Then ?IconDisplay{Prop:Icon} = icon:cross.
!          If clip(picon) = 'icon:connect'           Then ?IconDisplay{Prop:Icon} = icon:connect.
!          If clip(picon) = 'icon:print1'            Then ?IconDisplay{Prop:Icon} = icon:print1.
!          If clip(picon) = 'icon:ellipsis'          Then ?IconDisplay{Prop:Icon} = icon:ellipsis.
!          If clip(picon) = 'icon:vcrtop'            Then ?IconDisplay{Prop:Icon} = icon:vcrtop.
!          If clip(picon) = 'icon:vcrrewind'         Then ?IconDisplay{Prop:Icon} = icon:vcrrewind.
!          If clip(picon) = 'icon:vcrback'           Then ?IconDisplay{Prop:Icon} = icon:vcrback.
!          If clip(picon) = 'icon:vcrplay'           Then ?IconDisplay{Prop:Icon} = icon:vcrplay.
!          If clip(picon) = 'icon:vcrfastforward'    Then ?IconDisplay{Prop:Icon} = icon:vcrfastforward.
!          If clip(picon) = 'icon:vcrbottom'         Then ?IconDisplay{Prop:Icon} = icon:vcrbottom.
!          If clip(picon) = 'icon:vcrlocate'         Then ?IconDisplay{Prop:Icon} = icon:vcrlocate.

   End

   ! "icon"       A string constant or variable naming the .ICO file to display, or an EQUATE for one of Windows' standard icons
   !              (these EQUATEs are listed in EQUATES.CLW). If omitted, no icon is displayed on the dialog box.      
   
ButtonsProcessing Routine
   ! "buttons"    Either an integer expression which indicates which Windows standard buttons (may indicate multiple buttons) to place on the dialog box,
   !              or a string expression containing a vertical bar (|) delimited list of the text for up to 8 buttons. 
   !              If omitted, the dialog displays an Ok button.       

   If Omitted(pButtons) = 1
      ?Button1{Prop:Text}       = '&Ok'
      buttonUsed                = 1
      returnValues[buttonUsed]  = button:Ok

      Loop loopvar = ?Button2 to ?Button8 By 1
         loopvar{prop:hide} = 1
      End

      Exit
   End        

   !message( pButtons )

   !from equates.clw in \libsrc\
   !BUTTON:OK               EQUATE (01H)
   !BUTTON:YES              EQUATE (02H)
   !BUTTON:NO               EQUATE (04H)
   !BUTTON:ABORT            EQUATE (08H)
   !BUTTON:RETRY            EQUATE (10H)
   !BUTTON:IGNORE           EQUATE (20H)
   !BUTTON:CANCEL           EQUATE (40H)
   !BUTTON:HELP             EQUATE (80H)

   If Numeric( clip(pButtons) ) = 1

      ButtonNumber2 = clip(pButtons)     
      
      ! this order is important, buttonhelp has the highest value at the moment
      If (ButtonNumber2 / BUTTON:HELP) >= 1     Then    ButtonNumber2 = (ButtonNumber2 % BUTTON:HELP);      buttonhelp   = True.
      If (ButtonNumber2 / BUTTON:CANCEL) >= 1   Then    ButtonNumber2 = (ButtonNumber2 % BUTTON:CANCEL);    ButtonCancel = True.
      If (ButtonNumber2 / BUTTON:IGNORE) >= 1   Then    ButtonNumber2 = (ButtonNumber2 % BUTTON:IGNORE);    ButtonIgnore = True.
      If (ButtonNumber2 / BUTTON:RETRY) >= 1    Then    ButtonNumber2 = (ButtonNumber2 % BUTTON:RETRY);     ButtonRetry  = True.
      If (ButtonNumber2 / BUTTON:ABORT) >= 1    Then    ButtonNumber2 = (ButtonNumber2 % BUTTON:ABORT);     ButtonAbort  = True.
      If (ButtonNumber2 / BUTTON:NO) >= 1       Then    ButtonNumber2 = (ButtonNumber2 % BUTTON:NO);        ButtonNo     = True.
      If (ButtonNumber2 / BUTTON:YES) >= 1      Then    ButtonNumber2 = (ButtonNumber2 % BUTTON:YES);       ButtonYes    = True.
      If (ButtonNumber2 / BUTTON:OK) >= 1       Then    ButtonNumber2 = (ButtonNumber2 % BUTTON:OK);        Buttonok     = True.

      buttonUsed = 0
      If buttonok     = 1   
         buttonUsed               += 1
         returnValues[buttonUsed]  = Button:Ok
         buttonfeq[buttonUsed]{Prop:Text} = '&Ok'
      .

      If buttonyes    = 1
         buttonUsed               += 1
         returnValues[buttonUsed]  = Button:Yes
         buttonfeq[buttonUsed]{Prop:Text} = '&Yes'
      .

      If buttonno     = 1
         buttonUsed               += 1
         returnValues[buttonUsed]  = Button:No
         buttonfeq[buttonUsed]{Prop:Text} = '&No'
      .

      If buttonabort  = 1
         buttonUsed               += 1
         returnValues[buttonUsed]  = Button:Abort
         buttonfeq[buttonUsed]{Prop:Text} = '&Abort'
      .

      If buttonretry  = 1
         buttonUsed               += 1
         returnValues[buttonUsed]  = Button:Retry
         buttonfeq[buttonUsed]{Prop:Text} = '&Retry'
      .

      If buttonignore = 1
         buttonUsed               += 1
         returnValues[buttonUsed]  = Button:Ignore
         buttonfeq[buttonUsed]{Prop:Text} = '&Ignore'
      .

      If buttoncancel = 1
         buttonUsed               += 1
         returnValues[buttonUsed]  = Button:Cancel
         buttonfeq[buttonUsed]{Prop:Text} = '&Cancel'
      .

      If buttonhelp   = 1
         buttonUsed               += 1
         returnValues[buttonUsed]  = Button:Help
         buttonfeq[buttonUsed]{Prop:Text} = '&Help'
      .

   Else
      ! test string that might be pipe delimited

      buttonUsed = 0

      loopVar = InString('|', clip(pButtons), 1, 1)

      Loop 8 times
         loopVar = InString('|', clip(pButtons), 1, 1)

         If loopVar = 0
            ! last on the line
            buttonUsed                         += 1
            returnValues[buttonUsed]            = buttonUsed
            buttonfeq[buttonUsed]{Prop:Text}    = Clip(pButtons)    

            Break
         .

         buttonUsed                        += 1
         returnValues[buttonUsed]           = buttonUsed
         buttonfeq[buttonUsed]{Prop:Text}   = Clip(pButtons[1 : loopvar-1])    

         pButtons                           = sub(pButtons, loopvar+1, len(clip(pButtons)) )

         If pButtons = '' Then Break.
      .
   .


   Case buttonUsed 
     Of 8
        ! all buttons used

     Of 7                      
        ?Button8{Prop:Hide} = 1

     Of 6
        ?Button7{Prop:Hide} = 1
        ?Button8{Prop:Hide} = 1

     Of 5
        ?Button6{Prop:Hide} = 1
        ?Button7{Prop:Hide} = 1
        ?Button8{Prop:Hide} = 1

     Of 4
        ?Button5{Prop:Hide} = 1
        ?Button6{Prop:Hide} = 1
        ?Button7{Prop:Hide} = 1
        ?Button8{Prop:Hide} = 1

     Of 3
        ?Button4{Prop:Hide} = 1
        ?Button5{Prop:Hide} = 1
        ?Button6{Prop:Hide} = 1
        ?Button7{Prop:Hide} = 1
        ?Button8{Prop:Hide} = 1

     Of 2
        ?Button3{Prop:Hide} = 1
        ?Button4{Prop:Hide} = 1
        ?Button5{Prop:Hide} = 1
        ?Button6{Prop:Hide} = 1
        ?Button7{Prop:Hide} = 1
        ?Button8{Prop:Hide} = 1

     Of 1
        ?Button2{Prop:Hide} = 1
        ?Button3{Prop:Hide} = 1
        ?Button4{Prop:Hide} = 1
        ?Button5{Prop:Hide} = 1
        ?Button6{Prop:Hide} = 1
        ?Button7{Prop:Hide} = 1
        ?Button8{Prop:Hide} = 1
   End


   
   
