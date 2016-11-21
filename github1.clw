   PROGRAM



   INCLUDE('ABERROR.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('ERRORS.CLW'),ONCE
   INCLUDE('KEYCODES.CLW'),ONCE
   INCLUDE('ABFUZZY.INC'),ONCE

   MAP
     MODULE('GITHUB1_BC.CLW')
DctInit     PROCEDURE                                      ! Initializes the dictionary definition module
DctKill     PROCEDURE                                      ! Kills the dictionary definition module
     END
!--- Application Global and Exported Procedure Definitions --------------------------------------------
     MODULE('GITHUB1001.CLW')
Main                   PROCEDURE   !
     END
     MODULE('GITHUB1002.CLW')
DGByteToHex            FUNCTION(Byte AsciiValueIn),String   !
DGHexToByte            FUNCTION(String pHex),Long   !
DGOpSystemVersion      FUNCTION(<Byte pReturnAsText>),String   !
DGSearchReplace        PROCEDURE(*CString pString, *CString pchartosearch, Long pchartosearchLength=0, *CString pchartoreplace)   !
DGGuid                 FUNCTION(),String   !
ShellEx                PROCEDURE(String pFile, <Byte pOp>)   !
     END
     MODULE('GITHUB1003.CLW')
DGMessage              FUNCTION(STRING ptext, <STRING pcaption>, <STRING picon>, <STRING pbuttons>, UNSIGNED pdefault=0, UNSIGNED pstyle=0),UNSIGNED   !
     END
     !***************************************************************************************************************
     ! global map embed
     
          MODULE('GUID')
             RS_CoCreateGUID (*GUID0 Guid),LONG,RAW,PASCAL,NAME('CoCreateGuid')
             RS_StringFromGUID2 (*GUID0 Guid,*Cstring Lpsz, LONG Cbmax),LONG,RAW,PASCAL,NAME('StringFromGUID2')
          End
     
          MODULE('winver')
             GetVersionEx(Long),Bool,Raw,Pascal,Name('GetVersionExA')
          END
          
          
          MODULE('shellex')
            ShellExecute(ulong hwnd, *cstring lpOperation, *cstring lpFile, *cstring lpParameters, *cstring lpDirectory, long nShowCmd), ulong, pascal, raw, name('ShellExecuteA')
          End
     
     !***************************************************************************************************************
     
   END

SilentRunning        BYTE(0)                               ! Set true when application is running in 'silent mode'

!region File Declaration
!endregion

!***************************************************************************************************************
! global data embed


! put this into global data, for the Guid procedure in data, calling the windows CoCreateGuid function
guid0           Group,type
data1               ulong
data2               short
data3               short
data4               Byte,Dim(8)
                End


! for the os version procedure
OSVersionInfo             Group,Type
dwOSVersionInfoSize           ULong ! Specifies the size, in bytes, of this data structure.
dwMajorVersion                ULong ! Major version number of the operating system
dwMinorVersion                ULong ! Minor version number of the operating system
dwBuildNumber                 ULong ! See below
dwPlatformId                  ULong ! Identifies the operating system platform (Win32s/9x/NT)
szCSDVersion                  String(128) !See below
                          End

!***************************************************************************************************************

FuzzyMatcher         FuzzyClass                            ! Global fuzzy matcher
GlobalErrorStatus    ErrorStatusClass,THREAD
GlobalErrors         ErrorClass                            ! Global error manager
INIMgr               INIClass                              ! Global non-volatile storage manager
GlobalRequest        BYTE(0),THREAD                        ! Set when a browse calls a form, to let it know action to perform
GlobalResponse       BYTE(0),THREAD                        ! Set to the response from the form
VCRRequest           LONG(0),THREAD                        ! Set to the request from the VCR buttons

Dictionary           CLASS,THREAD
Construct              PROCEDURE
Destruct               PROCEDURE
                     END


  CODE
  GlobalErrors.Init(GlobalErrorStatus)
  FuzzyMatcher.Init                                        ! Initilaize the browse 'fuzzy matcher'
  FuzzyMatcher.SetOption(MatchOption:NoCase, 1)            ! Configure case matching
  FuzzyMatcher.SetOption(MatchOption:WordOnly, 0)          ! Configure 'word only' matching
  INIMgr.Init('.\github1.INI', NVD_INI)                    ! Configure INIManager to use INI file
  DctInit
  Main
  INIMgr.Update
  INIMgr.Kill                                              ! Destroy INI manager
  FuzzyMatcher.Kill                                        ! Destroy fuzzy matcher


Dictionary.Construct PROCEDURE

  CODE
  IF THREAD()<>1
     DctInit()
  END


Dictionary.Destruct PROCEDURE

  CODE
  DctKill()

