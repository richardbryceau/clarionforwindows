

   MEMBER('github1.clw')                                   ! This is a MEMBER module

                     MAP
                       INCLUDE('GITHUB1002.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
DGByteToHex          PROCEDURE  (Byte AsciiValueIn)        ! Declare Procedure
!******************************************************************************
! free to use, provided as is, no warranty provided
! this code cannot be included in other toolsets and resold for profit
!
! by Richard Bryce
! https://au.linkedin.com/in/richardbryce  
! http://northshoreit.com.au
!******************************************************************************


Out       STRING(2),AUTO

HexDigit0  STRING('00123456789ABCDEF00000000000')
!                  123456789012345678901234567890

HexDigit1  STRING('123456789ABCDEF0000000000000')
HexDigit2  STRING('0123456789ABCDEF000000000000')

  CODE
  ! (Byte AsciiValueIn) this is an integar and represents the ascii value of the character...

  If AsciiValueIn <= 15
     Out[1] = HexDigit0 [ Int(AsciiValueIn / 15) + 1 ]

     Case Int(AsciiValueIn % 15)
       Of 1 to 14
          Out[2] = HexDigit2 [ Int(AsciiValueIn % 15) + 1 ]

       Else
          Out[2] = 'F'
     .
  Else
     Out[1] = HexDigit1 [ Int(AsciiValueIn / 16) ]

     Case Int(AsciiValueIn % 16)
       Of 0 to 15
          Out[2] = HexDigit2[ Int(AsciiValueIn % 16) + 1 ]

       Else
          Out[2] = 'F'
     .
  .

  RETURN Out

!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
DGHexToByte          PROCEDURE  (String pHex)              ! Declare Procedure
!******************************************************************************
! free to use, provided as is, no warranty provided
! this code cannot be included in other toolsets and resold for profit
!
! by Richard Bryce
! https://au.linkedin.com/in/richardbryce  
! http://northshoreit.com.au
!******************************************************************************


Char1   Long
Char2   Long

Out     Long

  CODE
  !Char1   Long
  !Char2   Long
  !
  !Out     Long

  Case Upper(phex[1])
    Of '0' ; Char1 = 0
    Of '1' ; Char1 = 1
    Of '2' ; Char1 = 2
    Of '3' ; Char1 = 3
    Of '4' ; Char1 = 4
    Of '5' ; Char1 = 5
    Of '6' ; Char1 = 6
    Of '7' ; Char1 = 7
    Of '8' ; Char1 = 8
    Of '9' ; Char1 = 9
    Of 'A' ; Char1 = 10
    Of 'B' ; Char1 = 11
    Of 'C' ; Char1 = 12
    Of 'D' ; Char1 = 13
    Of 'E' ; Char1 = 14
    Of 'F' ; Char1 = 15
  End

  Case Upper(phex[2])
    Of '0' ; Char2 = 0
    Of '1' ; Char2 = 1
    Of '2' ; Char2 = 2
    Of '3' ; Char2 = 3
    Of '4' ; Char2 = 4
    Of '5' ; Char2 = 5
    Of '6' ; Char2 = 6
    Of '7' ; Char2 = 7
    Of '8' ; Char2 = 8
    Of '9' ; Char2 = 9
    Of 'A' ; Char2 = 10
    Of 'B' ; Char2 = 11
    Of 'C' ; Char2 = 12
    Of 'D' ; Char2 = 13
    Of 'E' ; Char2 = 14
    Of 'F' ; Char2 = 15
  End

  Return  (  (Char1*16^1) + (Char2*16^0) )  ! exponentiation, a ^ b means a raised to the power of b

!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
DGOpSystemVersion    PROCEDURE  (<Byte pReturnAsText>)     ! Declare Procedure
!******************************************************************************
! free to use, provided as is, no warranty provided
! this code cannot be included in other toolsets and resold for profit
!
! by Richard Bryce
! https://au.linkedin.com/in/richardbryce  
! http://northshoreit.com.au
!******************************************************************************

! for details on what the results mean, see this page on microsoft.com
! https://msdn.microsoft.com/en-au/library/windows/desktop/ms724832(v=vs.85).aspx
!
! there are other pages / sites that provide a more complete list



ReturnString        String(40)

OVI                 Like(OSVersionInfo)  ! a group type declared in the global include file...

! OVI = OperatingSystem
!       Version
!       Info


! this is in a global data embed point
!OSVersionInfo             Group,Type
!dwOSVersionInfoSize           ULong ! Specifies the size, in bytes, of this data structure.
!dwMajorVersion                ULong ! Major version number of the operating system
!dwMinorVersion                ULong ! Minor version number of the operating system
!dwBuildNumber                 ULong ! See below
!dwPlatformId                  ULong ! Identifies the operating system platform (Win32s/9x/NT)
!szCSDVersion                  String(128) !See below
!                          .

! this is in the global map
!MODULE('WIN32.LIB')
! GetVersionEx(Long),Bool,Raw,Pascal,Name('GetVersionExA')
!END





  CODE
    !(<Byte pReturnAsText>),String

    OVI:dwOSVersionInfoSize = Size(OVI)

    If GetVersionEx(Address(OVI)) = 0
       ReturnString = ''
       Message('Unable to determine what version of Windows you are on','OS Unsupported', Icon:Exclamation)

    Else
       If Omitted(pReturnAsText) = True
       
          ReturnString = OVI:dwMajorVersion      & '.' & |
                         OVI:dwMinorVersion      & '.' & |
                         OVI:dwBuildNumber       !& '.' & |
                         !OVI:dwPlatformId        & '.' & |
                         !clip(OVI:szCSDVersion)
                         
       Else              
          ! set here incase code below fails
          ReturnString = OVI:dwMajorVersion      & '.' & |
                         OVI:dwMinorVersion      & '.' & |
                         OVI:dwBuildNumber       !& '.' & |
                         !OVI:dwPlatformId        & '.' & |
                         !clip(OVI:szCSDVersion)

          Case OVI:dwMajorVersion
            Of 10          
               ReturnString = 'Windows 10 (' & Clip(ReturnString) & ')'
               
            Of 6
               Case OVI:dwMinorVersion
                 Of 3          
                    ReturnString = 'Windows 8.1 / Server 2012 R2 (' & Clip(ReturnString) & ')'
                  
                 Of 2
                    ReturnString = 'Windows 8 / Server 2012 (' & Clip(ReturnString) & ')'
                    
                 Of 1
                    ReturnString = 'Windows 7 / Server 2008 R2 (' & Clip(ReturnString) & ')'

                 Of 0
                    ReturnString = 'Windows Vista / Server 2008 (' & Clip(ReturnString) & ')'
               .

            Of 5
               Case OVI:dwMinorVersion
                 Of 2          
                    ReturnString = 'Windows XP 64bit / Server 2003 (' & Clip(ReturnString) & ')'
                  
                 Of 1
                    ReturnString = 'Windows XP (' & Clip(ReturnString) & ')'
                    
                 Of 0
                    ReturnString = 'Windows Server 2000 (' & Clip(ReturnString) & ')'
               .
            
          End
       
       .
    .
    Return ReturnString


!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
DGSearchReplace      PROCEDURE  (*CString pString,*CString pchartosearch,Long pchartosearchLength,*CString pchartoreplace) ! Declare Procedure
! when i wrote this i was trying to remove embedded carraige returns/line feeds, and tab characters etc 
! from a string. The code has not been used in production situations
! prehaps i should have added more code comments when i first wrote it!!!


!******************************************************************************
! free to use, provided as is, no warranty provided
! this code cannot be included in other toolsets and resold for profit
!
! by Richard Bryce
! https://au.linkedin.com/in/richardbryce  
! http://northshoreit.com.au
!******************************************************************************

  map
StringSearchReplaceIn Procedure (*CString pchartosearchIn, Long pchartosearchLength2, *CString pStringIn, Long pint1, Long pint2),Long
  .
  
xPos                Long
replaceLength       Long

  CODE
      xPos = 1
      
      !(*CString pString, *CString pchartosearch, Long pchartosearchLength=0, *CString pchartoreplace),

      Loop
         xPos = StringSearchReplaceIn(pchartosearch, pchartosearchLength, pString, 1, xPos)

         If xPos = 0 Then Break.
         
         If pchartosearchLength = 0

            If len(pchartoreplace) = 0
               If xPos = 1
                  pString = pString[ 2 : Len(Clip(pString)) ]
                  
               ElsIf xPos = 2
                  pString = pString[ 1 ] & pString[ xPos+1 : Len(Clip(pString)) ]
                  
               Else
                  pString = pString[ 1 : xPos - 1 ] & pString[ xPos+1 : Len(Clip(pString)) ]
               .

            ElsIf len(pchartoreplace) = 1
               If xPos = 1
                  pString = pchartoreplace[1] & pString[ 2 : Len(Clip(pString)) ]
                  
               ElsIf xPos = 2
                  pString = pString[ 1 ] & pchartoreplace[1] & pString[ xPos+1 : Len(Clip(pString)) ]
                  
               Else
                  pString = pString[ 1 : xPos - 1 ] & pchartoreplace[1] & pString[ xPos+1 : Len(Clip(pString)) ]
               .

            ElsIf Len(Clip(pchartoreplace)) > 1
               If xPos = 1
                  pString = Clip(pchartoreplace) & pString[ 2 : Len(Clip(pString)) ]
                  
               ElsIf xPos = 2
                  pString = pString[ 1 ] & Clip(pchartoreplace) & pString[ xPos+1 : Len(Clip(pString)) ]
                  
               Else
                  pString = pString[ 1 : xPos - 1 ] & Clip(pchartoreplace) & pString[ xPos+1 : Len(Clip(pString)) ]
               .

            Else
               ! this works for blank space or any other character
               pString[xPos] = sub(pchartoreplace, 1, 1)
            . 
         Else
            If len(pchartoreplace) = 0
            
               If xPos = 1
                  pString = pString[ pchartosearchLength+1 : Len(Clip(pString)) ]
                  
               ElsIf xPos = 2                          
                  pString = pString[ 1 ] & pString[ xPos+pchartosearchLength : Len(Clip(pString)) ]
                  
               Else
                  pString = pString[ 1 : xPos - 1 ] & pString[ xPos+pchartosearchLength : Len(Clip(pString)) ]
               .

            ElsIf len(pchartoreplace) = 1
               If xPos = 1
                  pString = pchartoreplace[1] & pString[ pchartosearchLength+1 : Len(Clip(pString)) ]
                  
               ElsIf xPos = 2
                  pString = pString[ 1 ] & pchartoreplace[1] & pString[ xPos+pchartosearchLength : Len(Clip(pString)) ]
                  
               Else
                  pString = pString[ 1 : xPos - 1 ] & pchartoreplace[1] & pString[ xPos+pchartosearchLength : Len(Clip(pString)) ]
               .

            Else !ElsIf Len(Clip(pchartoreplace)) > 1
               If xPos = 1
                  pString = Clip(pchartoreplace) & pString[ pchartosearchLength+1 : Len(Clip(pString)) ]
                  
               ElsIf xPos = 2
                  pString = pString[ 1 ] & Clip(pchartoreplace) & pString[ xPos+pchartosearchLength : Len(Clip(pString)) ]
                  
               Else
                  pString = pString[ 1 : xPos - 1 ] & Clip(pchartoreplace) & pString[ xPos+pchartosearchLength : Len(Clip(pString)) ]
               .

            !Else
            !   ! this works for blank space or any other character
            !   pString[xPos] = sub(pchartoreplace, 1, 1)
            . 
         .
         
         ! at bottom of the loop
         xPos += len(pchartoreplace)   !pchartosearchLength
      .



StringSearchReplaceIn Procedure (*CString pchartosearchIn, Long pchartosearchLength2=0, *CString pStringIn, Long pint1, Long pint2)
  Code
    If pchartosearchLength = 0                                                                                                                       
       Return InString( pchartosearchin, |
                        clip(pStringin), |
                        pint1, |
                        pint2) 
    Else
       Return InString( sub(pchartosearchin, 1, pchartosearchLength2), |
                        clip(pStringin), |
                        pint1, |
                        pint2) 
    .

	
	
	
!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
DGGuid               PROCEDURE                             ! Declare Procedure
!******************************************************************************
! free to use, provided as is, no warranty provided
! this code cannot be included in other toolsets and resold for profit
!
! by Richard Bryce
! https://au.linkedin.com/in/richardbryce  
! http://northshoreit.com.au
!******************************************************************************


!! put this into global data
!! for the Guid procedure in data, calling the windows CoCreateGuid function
!guid0           Group,type
!data1               ulong
!data2               short
!data3               short
!data4               Byte,Dim(8)
!                End
!
!
!! put this into the global map
!     MODULE('GUID')
!            RS_CoCreateGUID (*GUID0 Guid),LONG,RAW,PASCAL,NAME('CoCreateGuid')
!            RS_StringFromGUID2 (*GUID0 Guid,*Cstring Lpsz, LONG Cbmax),LONG,RAW,PASCAL,NAME('StringFromGUID2')
!     End


LOC:GUID1               CSTRING(101)
LOC:Guid2               STRING(37)
LOC:ReturnCode          LONG
LOC:GUID                LIKE(GUID0)

LoopChar                Long




  CODE

  Loc:ReturnCode  = RS_CoCreateGUID (Loc:GUID)

         !Value                   Meaning
         !RPC_S_OK              - The call succeeded.
         !RPC_S_UUID_LOCAL_ONLY - The UUID is guaranteed to be unique to this computer only.
         !RPC_S_UUID_NO_ADDRESS - Cannot get Ethernet or token-ring hardware address for this computer.
         !
         !message('call 1: ' & Loc:ReturnCode )
   If Loc:ReturnCode ~= 0 Then Return ''.


   Loc:ReturnCode  = RS_StringFromGUID2(Loc:GUID,Loc:GUID1,SIZE(Loc:GUID1))
         ! If the function succeeds, the return value is the number of characters in the returned string, including
         ! the null terminator. If the buffer is too small to contain the string, the return value is 0.
         !
         !message('call 2: ' & Loc:ReturnCode )
   If Loc:ReturnCode = 0 Then Return ''.


   Loc:GUID1       = UPPER(Loc:GUID1)

   LoopChar = 0
   LOOP I# = 2 TO SIZE(Loc:GUID1)  ! skip first character
      IF Loc:GUID1[I#] = '}'       ! also skip the last character, basically (and yes there is an easier way to do this)
         BREAK
      .
      If Loc:GUID1[I#] = '<0>'
         CYCLE
      .

      LoopChar += 1
      If LoopChar > SIZE(LOC:GUID2)
         BREAK
      .
      Loc:GUID2[LoopChar] = Loc:GUID1[I#]
   .
   Loc:GUID2[LoopChar + 1] = '<0>'

   Return Clip(Loc:Guid2)

   
!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
ShellEx              PROCEDURE  (String pFile,<Byte pOp>)  ! Declare Procedure
!******************************************************************************
! free to use, provided as is, no warranty provided
! this code cannot be included in other toolsets and resold for profit
!
! by Richard Bryce
! https://au.linkedin.com/in/richardbryce  
! http://northshoreit.com.au
!******************************************************************************


!(String pFile, <Byte pOp>)      

l:url             cstring(255)
l:handle          ulong
l:null            &cstring

l:op              CString(255)

Window WINDOW,AT(,,59,11),CENTER,IMM,AUTO
    END


! this is declared in the global map
!     
!     MODULE('shellex')
!       ShellExecute(ulong hwnd, *cstring lpOperation, *cstring lpFile, *cstring lpParameters, *cstring lpDirectory, long nShowCmd), ulong, pascal, raw, name('ShellExecuteA')
!     End



  CODE
  !(String pFile, <Byte pOp>)      

  If Clip(pFile) = '' Then Return.
  

  If pFile[1:4] = 'http'
     l:url = clip(pFile)

  !ElsIf pFileNameToPreview[1:2] = '\\' and instring('"', Clip(pFileNameToPreview), 1, 1) ~> 0
  
  ElsIf instring('"', Clip(pFile), 1, 1) = 0
     l:url = '"' & clip(pFile) & '"'      ! this is done so long file names are wrapped in double quotes, they will fail otherwise
  
  End                                        
 
  Open(Window)
  Accept
     Case Event()
       Of Event:OpenWindow
            l:null &= NULL

            l:handle = 0{prop:clienthandle}

            If omitted(pOp) = True
               If ShellExecute(l:handle,l:null,l:url,l:null,l:null,1) End               
            Else
               If pOp = True
                  l:Op = 'print'
                  If ShellExecute(l:handle,l:Op,l:url,l:null,l:null,1) End
               Else
                  If ShellExecute(l:handle,l:null,l:url,l:null,l:null,1) End
               End
            End

            0{Prop:Hide} = True
            Post(Event:CloseWindow)
     End
  End

  Close(Window)

