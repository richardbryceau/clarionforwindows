

   MEMBER('github1.clw')                                   ! This is a MEMBER module


   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

                     MAP
                       INCLUDE('GITHUB1001.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
Main PROCEDURE 

l:Guid               CSTRING(101)                          !
hexSelector          BYTE(1)                               !
hextobyteString      CSTRING(101)                          !
hexResultString      CSTRING(1001)                         !
hexCounter           LONG                                  !
l:versionString      CSTRING(41)                           !
l:string             CSTRING(4096)                         !
l:search             CSTRING(21)                           !
l:replace            CSTRING(21)                           !
l:length             LONG                                  !
l:searchOn           BYTE(3)                               !
l:searchOnString     CSTRING(21)                           !
l:ShellExecuteFileToLoad CSTRING(255)                      !
l:ShellExecutePrint  BYTE                                  !
Window               WINDOW('Caption'),AT(,,494,287),FONT('Arial',10,,FONT:regular),DOUBLE,AUTO,CENTER,IMM,SYSTEM
                       SHEET,AT(8,6,485,280),USE(?SHEET1),LEFT
                         TAB('Guid'),USE(?TabGuid)
                           PROMPT('l : Guid:'),AT(93,37,33,7),USE(?l:Guid:Prompt)
                           ENTRY(@s100),AT(124,37,231,12),USE(l:Guid)
                           BUTTON('Get Guid'),AT(89,13,,11),USE(?BUTTON_GetGuid)
                         END
                         TAB('Hex to Byte'),USE(?TabHextoByte)
                           PROMPT('String'),AT(91,17),USE(?hextobyteString:Prompt),TRN
                           ENTRY(@s100),AT(126,18,215,10),USE(hextobyteString)
                           OPTION('Selector'),AT(91,37,172,32),USE(hexSelector),BOXED,TRN
                             RADIO('Byte To Hex'),AT(126,52),USE(?hexSelector:Radio1),TRN,VALUE('1')
                             RADIO('Hex To Byte'),AT(191,52),USE(?hexSelector:Radio2),TRN,VALUE('2')
                           END
                           BUTTON('Convert'),AT(269,48),USE(?BUTTON_HexConvert)
                           PROMPT('Result'),AT(91,94),USE(?hexResultString:Prompt),TRN
                           TEXT,AT(126,94,214,88),USE(hexResultString),VSCROLL
                         END
                         TAB('Message'),USE(?TabMessage)
                           BUTTON('Show Message Box Examples'),AT(107,22),USE(?BUTTON_Message)
                         END
                         TAB('OS Version'),USE(?TabOSVersion)
                           BUTTON('Get Version'),AT(155,10),USE(?BUTTON_GetVersion)
                           PROMPT('l : version String:'),AT(83,28),USE(?l:versionString:Prompt:2)
                           ENTRY(@s40),AT(155,28,178,10),USE(l:versionString,,?l:versionString:2)
                         END
                         TAB('Search / Replace'),USE(?TabSearchReplace)
                           PROMPT('Search For'),AT(82,14),USE(?l:searchOn:Prompt:2)
                           LIST,AT(137,14,107,10),USE(l:searchOn),DROP(10),FROM('CRLF characters|#1|TAB characters' & |
  '|#2|Search Text string|#3')
                           PROMPT('Search Text'),AT(82,32),USE(?l:search:Prompt)
                           ENTRY(@s20),AT(137,32,60,10),USE(l:search)
                           PROMPT('Replace'),AT(208,32),USE(?l:replace:Prompt)
                           ENTRY(@s20),AT(245,32,60,10),USE(l:replace)
                           BUTTON('Search'),AT(312,32,42,10),USE(?BUTTON_Search)
                           STRING('The search is case sensitive!!!'),AT(137,47),USE(?SearchIsCaseSensitive)
                           STRING('Search Text'),AT(82,60),USE(?SearchText)
                           TEXT,AT(138,60,347,185),USE(l:string),VSCROLL,BOXED
                           STRING('Instructions'),AT(137,249),USE(?Info1)
                           STRING('1. use Search For drop down, or enter a Search Text.'),AT(137,259,187,10),USE(?Info2)
                           STRING('2. enter replacement text. Code in Event:Accepted will then trigger the search/' & |
  'replace operation.'),AT(137,271,325,10),USE(?Info3)
                         END
                         TAB('Shell Execute'),USE(?TabShellExecute)
                           PROMPT('File To Load'),AT(94,15),USE(?l:ShellExecuteFileToLoad:Prompt)
                           ENTRY(@s254),AT(139,16,294,10),USE(l:ShellExecuteFileToLoad)
                           CHECK('Direct Print'),AT(138,42),USE(l:ShellExecutePrint)
                           BUTTON('Shell Execute'),AT(138,65,33,21),USE(?Button_ShellEx)
                         END
                       END
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeNewSelection       PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('Main')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?l:Guid:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.Open(Window)                                        ! Open window
  ! replace the message box
     System{Prop:MessageHook} = Address(DGMessage)
  
  
  ! a simple search string
     l:string = 'Enter a string of characters<13,10>to be searched on here, include carraige return / line feed<13,10><13,10> and any other characters that you want to search on.'
  
     Post(Event:NewSelection, ?l:searchOn)
  
  Do DefineListboxStyle
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?BUTTON_GetGuid
      ThisWindow.Update()
      l:Guid = DGGuid()
      
    OF ?BUTTON_HexConvert
      ThisWindow.Update()
        Case hexSelector
      
      
          Of 1 ! byte to hex    
             hexResultString = ''
             Loop hexCounter = 1 to Len(Clip(hextobyteString)) By 1
          
                hexResultString = Clip(hexResultString) & DGByteToHex ( Val(hextobyteString[hexCounter] ) )
             End                                                      
             
             Message( Clip(hextobyteString) & '||Converted to hex equals|' & Clip(hexResultString) )
             
      
          Of 2 ! hex to byte
             hextobyteString = ''
      
             Loop hexCounter = 1 to Len(Clip(hexResultString)) by 2   ! two by two
                hextobyteString = Clip(hextobyteString) & CHR( DGHexToByte ( hexResultString[ hexCounter : (hexCounter+1) ]))
             End
      
             Message( 'Hex: ' & Clip(hexResultString) & '||equals|' & Clip(hextobyteString) )
        End
      
    OF ?BUTTON_Message
      ThisWindow.Update()
        ! this is job code that was added to test new message box function
        If Message ('hello world').
        If Message ('hello world','title').
        Message('button ' &  Message ('hello world','an exception',Icon:Exclamation,'Close The Application|Close A Thread|Invoke The Debugger|Log Info Blah|Ok Continue') & ' pressed' )
        
        If Message ('hello world','a title',Icon:Exclamation,'ayes|ano|amaybe|aabort').
        
        Message ( 'The Clarion 10 gold release shipped on June 5th, 2015. There have been many updates since.',|
                    'the title', Icon:Exclamation, button:abort+button:yes+button:no )
        
        Message ( 'The Clarion 10 gold release shipped on June 5th, 2015. There have been many updates since then, with the latest release sent out in February 2016.' & |
                  ' Clarion developers are moving forward, don''t be left behind! If you want to slash your build times in half (or more), plug-in web socket communications,' & |
                  ' send SMS messages, execute global search and replace in the Dictionary, and much more — you need C10 today.' & |
                  ' Renew your subscription today','TITLE', Icon:Exclamation, button:abort+button:yes+button:no )
                  
        !MSGMODE:SYSMODAL
      
        !MSGMODE:CANCOPY
        ! not supported, copy of display text enabled by default as TEXT control used
      
        !MSGMODE:FIXEDFONT
        
        If Message ('hello world fixed font','title',Icon:Exclamation, , , MSGMODE:FIXEDFONT ).
        
        ! not working when tested
        If Message ('hello world sys modal','title',Icon:Exclamation, , , MSGMODE:SYSMODAL ).
      
          
        Message( 'hello world','title', button:abort+button:yes)
      
    OF ?BUTTON_GetVersion
      ThisWindow.Update()
        l:versionString = DGOpSystemVersion(True)
        
    OF ?l:replace
        If l:string = ''
           Message('The Search Text field is empty. Please add a block of text that has a carraige return and prehaps also a tab character!','Error',Icon:Exclamation)
           Select(?l:string)
           Cycle
        End
        
        If l:searchOn = 3
           If len(l:search) = 0
              Message('Please enter a search phase','Error',Icon:Exclamation)
              Select(?l:search)
              Cycle
           END   
      
           If Len(l:replace) = 0
              Message('Please enter a search replacement phase','Error',Icon:Exclamation)
              Select(?l:replace)
              Cycle
           END   
      
           l:length = len(l:search)
             
           DGSearchReplace      (l:string, l:search, l:length, l:replace)
           Display()
      
           Message('search finished')
        Else
           If Len(l:replace) = 0
              Message('Please enter a search replacement phase','Error',Icon:Exclamation)
              Select(?l:replace)
              Cycle
           END   
      
           l:length = len(l:searchOnString)
             
           DGSearchReplace      (l:string, l:searchOnString, l:length, l:replace)
           Display()
      
           Message('search finished')
        End
      
    OF ?BUTTON_Search
      ThisWindow.Update()
      Post(Event:Accepted, ?l:replace)
      
    OF ?Button_ShellEx
      ThisWindow.Update()
        ShellEx(l:ShellExecuteFileToLoad, l:ShellExecutePrint)
       
        
        
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeNewSelection PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all NewSelection events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeNewSelection()
    CASE FIELD()
    OF ?l:searchOn
      If l:searchOn = 1
         l:searchOnString = '<13,10>'     ! carraige return / line feed in ascii
      End
      If l:searchOn = 2
         l:searchOnString = '<9>'         ! tab character in ascii
      End             
      
      If l:searchOn = 3   
         ?l:search:Prompt{Prop:Disable} = False
         ?l:search{Prop:Disable}        = False
      ELSE
         ?l:search:Prompt{Prop:Disable} = True
         ?l:search{Prop:Disable}        = True
      End
      
      Display()
      
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

