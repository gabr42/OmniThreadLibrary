(*:Collection of Win32 wrappers and helper functions.
   @desc <pre>
   Free for personal and commercial use. No rights reserved.

   Maintainer        : gabr
   Contributors      : ales, aoven, gabr, Lee_Nover, _MeSSiah_, Miha-R, Odisej, xtreme,
                       Brdaws, Gre-Gor, krho, Cavlji, radicalb, fora, M.C, MP002, Mitja,
                       Christian Wimmer, Tommi Prami, Miha
   Creation date     : 2002-10-09
   Last modification : 2012-04-20
   Version           : 1.66
</pre>*)(*
   History:
     1.66: 2012-04-20
       - TDSiRegistry.ReadBinary, WriteBinary now use RawByteString for data buffer.
     1.65: 2012-02-06
       - Implemented DSiSetFileTime and DSiSetFileTimes.
       - Implemented DSiDateTimeToFileTime.
     1.64b: 2012-01-11
       - Set msg.Result := 0; in DSiClassWndProc.
     1.64a: 2011-12-20
       - DSiAllocateHWnd works in 64-bit mode.
       - Fixed 64-bit DSiInterlockedExchangeAdd64.
     1.64: 2011-12-16
       - [GJ] Assembler implementation of the x64 version of Interlocked*64 family of functions.
     1.63: 2011-10-21
       - Interlocked*64 family implemented using Windows' InterlockedCompareExchange64
         on Win64-bit platform.
       - Removed dependency on the Consts unit.
       - Scoped unit names are used in XE2.
     1.62a: 2011-11-07
       - Compiles with XE2 (in 32-bit mode).
     1.62: 2011-09-26
       - Implemented DSiGetThreadTime (two overloaded versions).
     1.61c: 2011-07-26
       - [miha] SetErrorMode (SEM_NOOPENFILEERRORBOX) is called before LoadLibrary to 
         prevent opening messagebox by Windows.
     1.61b: 2011-06-27
       - [tommi prami] Compiles with D7.
     1.61a: 2011-05-06
       - [achim] DSiAddApplicationToFirewallExceptionListXP could fail with "Unknown
         resoveConflict" exception due to a syntax error.
     1.61: 2011-03-01
       - Faster DSiFileExtensionIs.
     1.60: 2010-12-04
       - When compiled with D2007 or newer, unit FileCtrl is not included.
     1.59b: 2010-10-28
       - Call UniqueString before calling CreateProcessW.
     1.59a: 2010-09-25
       - [Tommi Prami] Added types missing in Delphi 7.
     1.59: 2010-09-24
       - Added function DSiDisableStandby that will try to disable standby and hibernate
         on Windows XP SP 2 and newer.
     1.58a: 2010-09-19
       - Define TStartupInfoW in Delphi 7 and earlier.
     1.58: 2010-07-27
       - DSiAddApplicationToFirewallExceptionList[Advanced|XP] got a new parameter
         TDSiFwResolveConflict (default rcDuplicate) where the caller can specify
         behaviour if the rule with the same name already exists.
         [rcDuplicate = add new rule with the same name, rcOverwrite = remove all rules
          with the same name and then add the new rule, rcSkip = leave existing rules
          intact and don't add the new rule]
       - Implemented DSiFindApplicationInFirewallExceptionList[Advanced|XP].
     1.57a: 2010-07-20
       - Bug fix in DSiAddApplicationToFirewallExceptionListAdvanced: setting
         rule.ServiceName to '' caused fwPolicy2.Rules.Add(rule) to raise exception.
     1.57: 2010-06-18
       - DSiExecuteAsUser now calls CreateProcessWithLogonW if CreateProcessAsUser fails
         with ERROR_PRIVILEGE_NOT_HELD (1314). Windows error code is now returned in a
         parameter. Window station and desktop process-specific ACEs are removed in a
         background thread when child process exits (thanks to Christian Wimmer to pointing
         out this problem). If the username is not set, DSiExecuteAsUser still sets up
         ACEs for window station/desktop access but ends calling CreateProcess and does
         not remove ACEs at the end.
       - Two overloaded versions of DSiExecuteAsUser are implemented. One is backwards
         compatible and another introduces two parameters -
         out processInfo: TProcessInformation and startInfo: PStartupInfo (default: nil).
         If the latter is assigned, it will be used instead of the internally generated
         TStartupInfo. This version does not close process and thread handle returned
         in the TProcessInformation if 'wait' is False.
       - Implemented DSiRemoveAceFromWindowStation and DSiRemoveAceFromDesktop.
       - Added dynamically loaded API forwarders DSiCreateProcessWithLogonW,
         DSiCreateEnvironmentBlock, DSiDestroyEnvironmentBlock and
         DSiGetUserProfileDirectoryW.
       - DSiAddAceTo[WindowStation|Desktop] will not add SID if it already exists in the
         ACL.
     1.56: 2010-06-10
       - [Mitja] Clear the last error if CreateProcess succeeds in DSiExecuteAndCapture
         (found a test case where CreateProcess succeeded but last error was 126).
       - [Christian Wimmer] In Unicode Delphis, DSiOpenSCManager, DSiGetLongPathName,
         DSiGetModuleFileNameEx, DSiGetProcessImageFileName, DSiSetDllDirectory, and
         DSiSHEmptyRecycleBin were linking to the wrong verison of the API.
       - [Christian Wimmer] DSiCreateProcessAsUser must make a local copy of the
         commandLine parameter when using Unicode API because the API may modify the
         contents of this parameter.
       - Implemented DSiGetLogonSID, DSiAddAceToWindowStation and DSiAddAceToDesktop.
       - Redesigned DSiExecuteAsUser.
     1.55: 2010-04-12
       - Implemented DSiHasElapsed64 and DSiElapsedTime64.
     1.54: 2010-04-08
       - Implemented DSiLogonAs and DSiVerifyPassword.
     1.53c: 2010-02-01
       - DSiGetProcAddress made public.
     1.53b: 2009-12-15
       - Fixed Delphi 5 compilation.
     1.53a: 2009-11-24
       - Fixed TDSiRegistry.ReadVariant and WriteVariant to work with varUString
         (also fixes all sorts of TDSiRegistry problems in Delphi 2010.)
     1.53: 2009-11-13
       - Implemented DSiDeleteRegistryValue.
       - Added parameter 'access' to the DSiKillRegistry.
     1.52a: 2009-11-04
       - [Mitja] Fixed allocation in DSiGetUserName.
       - [Mitja] Also catch 'error' output in DSiExecuteAndCapture.
     1.52: 2009-10-28
       - DSiAddApplicationToFirewallExceptionList renamed to
         DSiAddApplicationToFirewallExceptionListXP.
       - Added DSiAddApplicationToFirewallExceptionListAdvanced which uses Advanced
         Firewall interface, available on Vista+.
       - DSiAddApplicationToFirewallExceptionList now calls either
         DSiAddApplicationToFirewallExceptionListXP or
         DSiAddApplicationToFirewallExceptionListAdvanced, depending on OS version.
       - Implemented functions to remove application from the firewall exception list:
         DSiRemoveApplicationFromFirewallExceptionList,
         DSiRemoveApplicationFromFirewallExceptionListAdvanced,
         DSiRemoveApplicationFromFirewallExceptionListXP.
     1.51a: 2009-10-27
       - Convert non-EOleSysError exceptions in DSiAddApplicationToFirewallExceptionList
         into ERROR_INVALID_FUNCTION error.
     1.51: 2009-10-22
       - Added 'onNewLine' callback to the DSiExecuteAndCapture. This event reports
         program output line-by-line in real time and it can extend total time allowed
         for program execution.
     1.50: 2009-10-20
       - [Mitja] Updated DSiExecuteAndCapture: settable timeout, Unicode Delphi support,
         LastError is set if CreateProcess fails.
       - Implemented functions DSiGetSubstDrive and DSiGetSubstPath.
     1.49: 2009-10-12
       - Added 'access' parameter to the DSiWriteRegistry methods so that user can
         request writing to the non-virtualized key when running on 64-bit system
         (KEY_WOW64_64KEY).
     1.48: 2009-10-09
       - Defined TOSVersionInfoEx record and corresponding constants.
       - Extended DSiGetWindowsVersion to return wvWinServer2008, wvWin7 and
         wvWinServer2008R2.
       - Extended DSiGetTrueWindowsVersion to return wvWinServer2008OrVistaSP1 and
         wvWin7OrServer2008R2.
     1.47a: 2009-09-03
       - Added parameter connectionIsAvailable to the DSiGetNetworkResource.
     1.47: 2009-05-22
       - Added dynamically loaded API forwarder DSiGetTickCount64.
     1.46: 2009-03-17
       - Added dynamically loaded API forwarders DSiWow64DisableWow64FsRedirection and
         DSiWow64RevertWow64FsRedirection.
       - Implemented function DSiDisableWow64FsRedirection and DSiRevertWow64FsRedirection.
     1.45: 2009-03-16
       - Implemented DSiGetCurrentThreadHandle and DSiGetCurrentProcessHandle.
     1.44a: 2009-02-28
       - Added D2009 compatibility fixes to DSiGetFolderLocation, DSiGetNetworkResource,
         DSiGetComputerName, DSiGetWindowsFolder.
       - Fixed DSiGetTempFileName, DSiGetUserName.
     1.44: 2009-02-05
       - Added functions DSiAddApplicationToFirewallExceptionList and
         DSiAddPortToFirewallExceptionList.
     1.43: 2009-01-28
       - Added functions DSiGetNetworkResource, DSiDisconnectFromNetworkResource.
     1.42: 2009-01-23
       - Added functions DSiGetGlobalMemoryStatus, DSiGlobalMemoryStatusEx.
     1.41: 2008-08-20
       - Delphi 2009 compatibility.
     1.40c: 2008-07-14
       - Bug fixed: It was not possible to use DSiTimeGetTime64 in parallel from multiple
         threads
     1.40b: 2008-07-11
       - Forced {$T-} as the code doesn't compile in {$T+} state.
     1.40a: 2008-06-23
       - Added constants FILE_LIST_DIRECTORY, FILE_SHARE_FULL, FILE_ACTION_ADDED,
         FILE_ACTION_REMOVED, FILE_ACTION_MODIFIED, FILE_ACTION_RENAMED_OLD_NAME,
         FILE_ACTION_RENAMED_NEW_NAME.
     1.40: 2008-05-30
       - Added function DSiCopyFileAnimated.
     1.39: 2008-05-05
       - Added function DSiConnectToNetworkResource.
     1.38: 2008-04-29
       - Added functions to copy HTML format to and from the clipboard:
         DSiIsHtmlFormatOnClipboard, DSiGetHtmlFormatFromClipboard,
         DSiCopyHtmlFormatToClipboard.
     1.37: 2008-03-27
       - Created DSiInterlocked*64 family of functions by copying the code from
         http://qc.borland.com/wc/qcmain.aspx?d=6212. Functions were written by
         Will DeWitt Jr [edge@icehouse.net] and are included with permission.
       - Implemented DSiYield.
     1.36a: 2008-01-16
       - Changed DSiIsAdmin to use big enough buffer for token data.
       - Changed DSiIsAdmin to ignore SE_GROUP_ENABLED attribute because function was
         sometimes incorrectly returning False.
     1.36: 2007-12-29
       - Added procedures DSiCenterRectInRect and DSiMakeRectFullyVisibleOnRect.
     1.35: 2007-12-17
       - Added DSiTerminateProcessById procedure.
     1.34: 2007-12-03
       - Added three performance counter helpers DSiPerfCounterToMS, DSiPerfCounterToUS,
         and DSiQueryPerfCounterAsUS.
     1.33: 2007-11-26
       - Added function DSiTimeGetTime64.
     1.32: 2007-11-13
       - Added parameter 'parameters' to DSiCreateShortcut and DSiGetShortcutInfo.
       - Added function DSiEditShortcut.
       - Added function DSiInitFontToSystemDefault. 
     1.31: 2007-11-06
       - Added SHGetSpecialFolderLocation folder constants: CSIDL_ALTSTARTUP,
         CSIDL_CDBURN_AREA, CSIDL_COMMON_ALTSTARTUP, CSIDL_COMMON_DESKTOPDIRECTORY,
         CSIDL_COMMON_FAVORITES, CSIDL_COMMON_MUSIC, CSIDL_COMMON_PICTURES,
         CSIDL_COMMON_PROGRAMS, CSIDL_COMMON_STARTMENU, CSIDL_COMMON_STARTUP,
         CSIDL_COMMON_TEMPLATES, CSIDL_COMMON_VIDEO, CSIDL_COMPUTERSNEARME,
         CSIDL_CONNECTIONS, CSIDL_COOKIES, CSIDL_INTERNET, CSIDL_INTERNET_CACHE,
         CSIDL_MYDOCUMENTS, CSIDL_MYMUSIC, CSIDL_MYVIDEO, CSIDL_PHOTOALBUMS,
         CSIDL_PLAYLISTS, CSIDL_PRINTHOOD, CSIDL_PROFILE, CSIDL_RESOURCES,
         CSIDL_SAMPLE_MUSIC, CSIDL_SAMPLE_PLAYLISTS, CSIDL_SAMPLE_PICTURES,
         CSIDL_SAMPLE_VIDEOS.
       - Added ShGetSpecialFolderLocation flags CSIDL_FLAG_DONT_UNEXPAND and
         CSIDL_FLAG_DONT_VERIFY.
       - Added dynamically loaded API forwarder DSiSetSuspendState.
       - Added dynamically loaded API forwarders DSiEnumProcessModules,
         DSiGetModuleFileNameEx, and DSiGetProcessImageFileName.
       - Added function DSiGetProcessFileName.
       - More stringent checking in DSiGetProcessWindow.
       - Fixed DSiLoadLibrary so that it can be called before initialization section is
         executed. Somehow, D2007 doesn't always call initialization sectins in correct
         order >:-(
     1.30: 2007-10-25
       - Added dynamically loaded API forwarders DSiDwmEnableComposition and
         DsiDwmIsCompositionEnabled.
       - Added Aero group with functions DSiAeroDisable, DSiAeroEnable, and
         DSiAeroIsEnabled.
     1.29: 2007-10-03
       - Added function DSiIsAdmin.
     1.28a: 2007-09-12
       - TDSiTimer properties changed from published to public.
     1.28: 2007-07-25
       - Added function DSiMoveFile.
       - Small changes in DSiMoveOnReboot.
     1.27: 2007-07-11
       - Added two overloaded functions DSiGetThreadTimes.
     1.26: 2007-06-13
       - Added two overloaded function DSiFileExtensionIs.
     1.25: 2007-05-30
       - Added thread-safe alternative to (De)AllocateHwnd - DSi(De)AllocateHwnd.
       - Added TDSiTimer - a TTimer clone that uses DSi(De)AllocateHwnd internally.
       - New function DSiGetFolderSize.
       - Bug fixed: GetProcAddress result was not checked in DSiRegisterActiveX.
     1.24: 2007-03-21
       - Added support for search depth limitation to DSiEnumFilesEx and 
         DSiEnumFilesToSL.
     1.23: 2007-02-14
       - New functions: DSiGetProcessTimes (two overloaded versions), DSiGetFileTimes,
         DSiFileTimeToDateTime (two overloaded versions), DSiFileTimeToMicroSeconds,
         DSiGetProcessMemoryInfo, DSiGetProcessMemory (two overloaded versions),
         DSiIsWow64, DSiGetAppCompatFlags, DSiGetTrueWindowsVersion.
       - New dynamically loaded API forwarder: DSiIsWow64Process.
       - Added parameter 'access' to DSiReadRegistry, DSiRegistryKeyExists, and
         DSiRegistryValueExists functions.
     1.22: 2006-12-07
       - New function: DSiGetFileTime.
       - Added Windows Vista detection to DSiGetWindowsVersion.
     1.21: 2006-08-14
       - New functions: DSiFileExistsW, DSiDirectoryExistsW, DSiFileSizeW,
         DSiCompressFile, DSiUncompressFile, DSiIsFileCompressed, DSiIsFileCompressedW.
     1.20: 2006-06-20
       - New function: DSiGetShortcutInfo.
     1.19: 2006-05-15
       - New functions: DSiEnumFilesToSL, DSiGetLongPathName.
     1.18: 2006-04-11
       - New function: DSiSetDllDirectory.
     1.17a: 2006-04-05
       - Exit DSiProcessMessages when WM_QUIT is received.
     1.17: 2006-03-14
       - New function: DSiRegistryValueExists.
       - Added 'working dir' parameter to the DSiCreateShortcut function.
     1.16: 2006-01-23
       - New DSiExecuteAndCapture implementation, contributed by matej.
     1.15b: 2005-12-19
       - TDSiRegistry.ReadInteger can now also read 4-byte binary values.
     1.15a: 2005-08-09
       - Removed StrNew from DSiGetComputerName because it caused the result never to
         be released.
     1.15: 2005-07-11
       - New function: DSiWin32CheckNullHandle.
     1.14: 2005-06-09
       - New function: DSiGetEnvironmentVariable.
       - DSiExecuteAndCapture modified to return exit code in a (newly added) parameter
         and fixed to work on fast computers.
     1.13a: 2005-03-15
       - Make DSiGetTempFileName return empty string when GetTempFileName fails. 
     1.13: 2005-02-12
       - New functions: DSiExitWindows, DSiGetSystemLanguage, DSiGetKeyboardLayouts.
       - New methods: TDSiRegistry.ReadBinary (two overloaded versions),
         TDSiRegistry.WriteBinary (two overloaded versions).
       - Added OLE string processing to TDSiRegistry.ReadVariant and
         TDSiRegistry.WriteVariant.
       - Exported helper functions UTF8Encode and UTF8Decode for old Delphis (D5 and
         below).
       - Added Windows 2003 detection to DSiGetWindowsVersion.
       - Modified DSiEnablePrivilege to return True without doint anything on 9x platform.
       - Fixed handle leak in DSiSetProcessPriorityClass.
       - Removed some dead code.
       - Documented the Information segment.
     1.12: 2004-09-21
       - Added function DSiIncrementWorkingSet.
     1.11: 2004-02-12
       - Added functions DSiSetProcessPriorityClass, DSiGetProcessOwnerInfo (two
         overloaded versions), DSiEnablePrivilege.
     1.10: 2003-12-18
       - Updated TDSiRegistry.ReadString to handle DWORD registry values too.
       - Updated TDSiRegistry.ReadInteger to handle string registry values too.
     1.09: 2003-11-14
       - Added functions DSiValidateProcessAffinity, DSiValidateThreadAffinity,
         DSiValidateProcessAffinityMask, DSiValidateThreadAffinityMask,
         DSiGetSystemAffinityMask, DSiGetProcessAffinityMask,
         DSiGetThreadAffinityMask, DSiAffinityMaskToString, and
         DSiStringToAffinityMask.
     1.08: 2003-11-12
       - Added functions DSiCloseHandleAndInvalidate, DSiWin32CheckHandle,
         DSiGetSystemAffinity, DSiGetProcessAffinity, DSiSetProcessAffinity,
         DSiGetThreadAffinity, DSiSetThreadAffinity.
       - Added types TDSiFileHandle, TDSiPipeHandle, TDSiMutexHandle, TDSiEventHandle,
         TDSiSemaphoreHandle; all equivaled to THandle.
     1.07a: 2003-10-18
       - DSiuSecDelay was broken. Fixed.
     1.07: 2003-10-09
       - Added functions DSiGetUserNameEx, DSiIsDiskInDrive, DSiGetDiskLabel,
         DSiGetMyDocumentsFolder, DSiGetSystemVersion, DSiRefreshDesktop,
         DSiGetWindowsVersion, DSiRebuildDesktopIcons.
       - Added TDSiRegistry methods ReadStrings and WriteStrings dealing with MULTI_SZ
         registry format.
     1.06a: 2003-09-03
       - Typo fixed in DSiMsgWaitForTwoObjectsEx.
     1.06: 2003-09-02
       - New functions: DSiMsgWaitForTwoObjectsEx, DSiMsgWaitForThreeObjectsEx.
       - Documented 'Handles' and 'Registry' sections.
       - Bug fixed in DSiLoadLibrary.
     1.05: 2003-09-02
       - New functions: DSiMonitorOn, DSiMonitorOff, DSiMonitorStandby, DSiGetBootType,
         DSiShareFolder, DSiUnshareFolder, DSiFileSize, DSiEnumFiles, DSiEnumFilesEx,
         DSiGetDomain, DSiProcessMessages, DSiProcessThreadMessages, DSiLoadLibrary,
         DSiGetProcAddress, DSiDisableX, DSiEnableX.
       - Added dynamically loaded API forwarders: DSiNetApiBufferFree, DSiNetWkstaGetInfo,
         DSiSHEmptyRecycleBin, DSiCreateProcessAsUser, DSiLogonUser,
         DSiImpersonateLoggedOnUser, DSiRevertToSelf, DSiCloseServiceHandle,
         DSiOpenSCManager, DSi9xNetShareAdd, DSi9xNetShareDel, DSiNTNetShareAdd,
         DSiNTNetShareDel.
       - DSiGetUserName could fail on Win9x. Fixed.
       - Declared constants WAIT_OBJECT_1 (= WAIT_OBJECT_0+1) to WAIT_OBJECT_9
         (=WAIT_OBJECT_0+9).
       - All dynamically loaded functions are now available to the public (see new
         { DynaLoad } section).
       - All functions using dynamically loaded API calls were modified to use new
         DynaLoad methods.
       - All string parameters turned into 'const' parameters.
       - Various constants and type declarations moved to the 'interface' section.
     1.04: 2003-05-27
       - New functions: DSiLoadMedia, DSiEjectMedia.
     1.03: 2003-05-24
       - New functions: DSiExecuteAndCapture, DSiFreeMemAndNil.
     1.02a: 2003-05-05
       - Refuses to compile with Kylix.
       - Removed platform-related warnings on Delphi 6&7.
     1.02: 2002-12-29
       - New function: DSiElapsedSince.
     1.01: 2002-12-19
       - Compiles with Delphi 6 and Delphi 7.
       - New functions:
          Files:
            procedure DSiDeleteFiles(folder: string; fileMask: string);
            procedure DSiDeleteTree(folder: string; removeSubdirsOnly: boolean);
            procedure DSiEmptyFolder(folder: string);
            procedure DSiEmptyRecycleBin;
            procedure DSiRemoveFolder(folder: string);
            procedure DSiuSecDelay(delay: word);
          Processes:
            function  DSiExecuteAsUser(const commandLine, username, password: string;
              const domain: string = '.'; visibility: integer = SW_SHOWDEFAULT;
              workDir: string = ''; wait: boolean = false): cardinal;
            function  DSiImpersonateUser(const username, password, domain: string): boolean;
            procedure DSiStopImpersonatingUser;
     1.0: 2002-11-25
       - Released.
*)

unit DSiWin32;

{$J+,T-} // required!

interface

{$IFDEF Linux}{$MESSAGE FATAL 'This unit is for Windows only'}{$ENDIF Linux}
{$IFDEF OSX}{$MESSAGE FATAL 'This unit is for Windows only'}{$ENDIF OSX}
{$IFDEF MSWindows}{$WARN SYMBOL_PLATFORM OFF}{$WARN UNIT_PLATFORM OFF}{$ENDIF MSWindows}

{$DEFINE NeedUTF}{$UNDEF NeedVariants}{$DEFINE NeedStartupInfo}
{$DEFINE NeedFileCtrl}
{$DEFINE NeedRawByteString}
{$IFDEF ConditionalExpressions}
  {$UNDEF NeedUTF}{$DEFINE NeedVariants}{$UNDEF NeedStartupInfo}
  {$IF RTLVersion >= 18}{$UNDEF NeedFileCtrl}{$IFEND}
  {$IF CompilerVersion >= 23}{$DEFINE ScopedUnitNames}{$IFEND}
{$ENDIF}
{$IFDEF Unicode}{$UNDEF NeedRawByteString}{$ENDIF}

uses
  {$IFDEF ScopedUnitNames}Winapi.Windows{$ELSE}Windows{$ENDIF},
  {$IFDEF ScopedUnitNames}Winapi.Messages{$ELSE}Messages{$ENDIF},
  {$IFDEF NeedVariants}
  {$IFDEF ScopedUnitNames}System.Variants{$ELSE}Variants{$ENDIF},
  {$ENDIF}
  {$IFDEF NeedFileCtrl}
  FileCtrl, // use before SysUtils so deprecated functions from FileCtrl can be reintroduced
  {$ENDIF NeedFileCtrl}
  {$IFDEF ScopedUnitNames}
  System.UITypes,
  {$ENDIF}
  {$IFDEF ScopedUnitNames}System.SysUtils{$ELSE}SysUtils{$ENDIF},
  {$IFDEF ScopedUnitNames}Winapi.ShellAPI{$ELSE}ShellAPI{$ENDIF},
  {$IFDEF ScopedUnitNames}Winapi.ShlObj{$ELSE}ShlObj{$ENDIF},
  {$IFDEF ScopedUnitNames}System.Classes{$ELSE}Classes{$ENDIF},
  {$IFDEF ScopedUnitNames}Vcl.Graphics{$ELSE}Graphics{$ENDIF},
  {$IFDEF ScopedUnitNames}System.Win.Registry{$ELSE}Registry{$ENDIF};

const
  // pretty wrappers
  WAIT_OBJECT_1 = WAIT_OBJECT_0+1;
  WAIT_OBJECT_2 = WAIT_OBJECT_0+2;
  WAIT_OBJECT_3 = WAIT_OBJECT_0+3;
  WAIT_OBJECT_4 = WAIT_OBJECT_0+4;
  WAIT_OBJECT_5 = WAIT_OBJECT_0+5;
  WAIT_OBJECT_6 = WAIT_OBJECT_0+6;
  WAIT_OBJECT_7 = WAIT_OBJECT_0+7;
  WAIT_OBJECT_8 = WAIT_OBJECT_0+8;
  WAIT_OBJECT_9 = WAIT_OBJECT_0+9;

  // folder constants missing from ShellObj
  CSIDL_ADMINTOOLS              = $0030; //v5.0; <user name>\Start Menu\Programs\Administrative Tools
  CSIDL_ALTSTARTUP              = $001D; //The file system directory that corresponds to the user's nonlocalized Startup program group.
  CSIDL_APPDATA                 = $001A; //v4.71; Application Data, new for NT4
  CSIDL_CDBURN_AREA             = $003B; //v6.0; The file system directory acting as a staging area for files waiting to be written to CD.
  CSIDL_COMMON_ADMINTOOLS       = $002F; //v5.0; All Users\Start Menu\Programs\Administrative Tools
  CSIDL_COMMON_ALTSTARTUP       = $001E; //The file system directory that corresponds to the nonlocalized Startup program group for all users.
  CSIDL_COMMON_APPDATA          = $0023; //v5.0; All Users\Application Data
  CSIDL_COMMON_DESKTOPDIRECTORY = $0019; //The file system directory that contains files and folders that appear on the desktop for all users.
  CSIDL_COMMON_DOCUMENTS        = $002E; //All Users\Documents
  CSIDL_COMMON_FAVORITES        = $001F; //The file system directory that serves as a common repository for favorite items common to all users.
  CSIDL_COMMON_MUSIC            = $0035; //v6.0; The file system directory that serves as a repository for music files common to all users.
  CSIDL_COMMON_PICTURES         = $0036; //v6.0; The file system directory that serves as a repository for image files common to all users.
  CSIDL_COMMON_PROGRAMS         = $0017; //The file system directory that contains the directories for the common program groups that appear on the Start menu for all users.
  CSIDL_COMMON_STARTMENU        = $0016; //The file system directory that contains the programs and folders that appear on the Start menu for all users.
  CSIDL_COMMON_STARTUP          = $0018; //The file system directory that contains the programs that appear in the Startup folder for all users.
  CSIDL_COMMON_TEMPLATES        = $002D; //The file system directory that contains the templates that are available to all users.
  CSIDL_COMMON_VIDEO            = $0037; //v6.0; The file system directory that serves as a repository for video files common to all users.
  CSIDL_COMPUTERSNEARME         = $003D; //The folder representing other machines in your workgroup.
  CSIDL_CONNECTIONS             = $0031; //The virtual folder representing Network Connections, containing network and dial-up connections.
  CSIDL_COOKIES                 = $0021; //The file system directory that serves as a common repository for Internet cookies.
  CSIDL_HISTORY                 = $0022; //The file system directory that serves as a common repository for Internet history items.
  CSIDL_INTERNET                = $0001; //A viritual folder for Internet Explorer (icon on desktop).
  CSIDL_INTERNET_CACHE          = $0020; //v4.72; The file system directory that serves as a common repository for temporary Internet files.
  CSIDL_LOCAL_APPDATA           = $001C; //v5.0; non roaming, user\Local Settings\Application Data
  CSIDL_MYDOCUMENTS             = $000C; //v6.0; The virtual folder representing the My Documents desktop item.
  CSIDL_MYMUSIC                 = $000D; //The file system directory that serves as a common repository for music files.
  CSIDL_MYPICTURES              = $0027; //v5.0; My Pictures, new for Win2K
  CSIDL_MYVIDEO                 = $000E; //v6.0; The file system directory that serves as a common repository for video files.
  CSIDL_PHOTOALBUMS             = $0045; //Vista; The virtual folder used to store photo albums.
  CSIDL_PLAYLISTS               = $003F; //Vista; The virtual folder used to store play albums.
  CSIDL_PRINTHOOD               = $001B; //The file system directory that contains the link objects that can exist in the Printers virtual folder.
  CSIDL_PROFILE                 = $0028; //v5.0; The user's profile folder.
  CSIDL_PROGRAM_FILES           = $0026; //v5.0; C:\Program Files
  CSIDL_PROGRAM_FILES_COMMON    = $002B; //v5.0; C:\Program Files\Common
  CSIDL_RESOURCES               = $0038; //Vista; The file system directory that contains resource data.
  CSIDL_SAMPLE_MUSIC            = $0040; //Vista; The file system directory that contains sample music.
  CSIDL_SAMPLE_PICTURES         = $0042; //Vista; The file system directory that contains sample pictures.
  CSIDL_SAMPLE_PLAYLISTS        = $0041; //Vista; The file system directory that contains sample playlists.
  CSIDL_SAMPLE_VIDEOS           = $0043; //Vista; The file system directory that contains sample videos.
  CSIDL_SYSTEM                  = $0025; //v5.0; GetSystemDirectory()
  CSIDL_WINDOWS                 = $0024; //GetWindowsDirectory()

  CSIDL_FLAG_DONT_UNEXPAND = $2000; //Combine with another CSIDL constant to ensure expanding of environment variables.
  CSIDL_FLAG_DONT_VERIFY   = $4000; //Combine with another CSIDL constant, except for CSIDL_FLAG_CREATE, to return an unverified folder path-with no attempt to create or initialize the folder.
  CSIDL_FLAG_CREATE        = $8000; // new for Win2K, OR this in to force creation of folder

  FILE_DEVICE_FILE_SYSTEM  = 9;
  FILE_DEVICE_MASS_STORAGE = $2D;
  METHOD_BUFFERED          = 0;
  FILE_ANY_ACCESS          = 0;
  FILE_READ_ACCESS         = 1;
  FILE_WRITE_ACCESS        = 2;
  IOCTL_STORAGE_EJECT_MEDIA = (FILE_DEVICE_MASS_STORAGE shl 16) OR
                              (FILE_READ_ACCESS shl 14)         OR
                              ($202 shl 2)                      OR
                              (METHOD_BUFFERED);
  IOCTL_STORAGE_LOAD_MEDIA  = (FILE_DEVICE_MASS_STORAGE shl 16) OR
                              (FILE_READ_ACCESS shl 14)         OR
                              ($203 shl 2)                      OR
                              (METHOD_BUFFERED);
  FSCTL_SET_COMPRESSION     = (FILE_DEVICE_FILE_SYSTEM shl 16)                 OR
                              ((FILE_READ_ACCESS OR FILE_WRITE_ACCESS) shl 14) OR
                              (16 shl 2)                                       OR
                              (METHOD_BUFFERED);

  COMPRESSION_FORMAT_NONE    = 0;
  COMPRESSION_FORMAT_DEFAULT = 1;

  SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
  SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;

  STYPE_DISKTREE = 0;
  SHI50F_RDONLY = $0001;
  SHI50F_FULL = $0002;
  SHI50F_DEPENDSON = SHI50F_RDONLY or SHI50F_FULL;
  SHI50F_ACCESSMASK = SHI50F_RDONLY or SHI50F_FULL;

  // IPersisteFile GUID
  IID_IPersistFile: TGUID = (
    D1: $0000010B; D2: $0000; D3: $0000; D4: ($C0, $00, $00, $00, $00, $00, $00, $46));

  // Extension for shortcut files
  CLinkExt = '.lnk';

  // ShEmptyRecycleBinA flags
  SHERB_NOCONFIRMATION = $00000001;
  SHERB_NOPROGRESSUI   = $00000002;
  SHERB_NOSOUND        = $00000004;

  // CurrentVersion registry key
  DSiWinVerKey9x = '\Software\Microsoft\Windows\CurrentVersion';
  DSiWinVerKeyNT = '\Software\Microsoft\Windows NT\CurrentVersion';
  DSiWinVerKeys: array [boolean] of string = (DSiWinVerKey9x, DSiWinVerKeyNT);

  // CPU IDs for the Affinity familiy of functions
  DSiCPUIDs = '0123456789ABCDEFGHIJKLMNOPQRSTUV';

  // security constants needed in DSiIsAdmin
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
  DOMAIN_ALIAS_RID_USERS : DWORD = $00000221;
  DOMAIN_ALIAS_RID_GUESTS: DWORD = $00000222;
  DOMAIN_ALIAS_RID_POWER_: DWORD = $00000223;
  SE_GROUP_ENABLED = $00000004;

type
  // API types not defined in Delphi 5
  PWkstaInfo100 = ^TWkstaInfo100;
  _WKSTA_INFO_100 = record
    wki100_platform_id: DWORD;
    wki100_computername: LPWSTR;
    wki100_langroup: LPWSTR;
    wki100_ver_major: DWORD;
    wki100_ver_minor: DWORD;
  end;
  {$EXTERNALSYM _WKSTA_INFO_100}
  TWkstaInfo100 = _WKSTA_INFO_100;
  WKSTA_INFO_100 = _WKSTA_INFO_100;
  {$EXTERNALSYM WKSTA_INFO_100}

  SHARE_INFO_2_NT = record
    shi2_netname: PWideChar;
    shi2_type: Integer;
    shi2_remark: PWideChar;
    shi2_permissions: Integer;
    shi2_max_uses: Integer;
    shi2_current_uses: Integer;
    shi2_path: PWideChar;
    shi2_passwd: PWideChar;
  end;

  SHARE_INFO_50_9x = record
    shi50_netname: array[1..13] of char;
    shi50_type: byte;
    shi50_flags: short;
    shi50_remark: pchar;
    shi50_path: pchar;
    shi50_rw_password: array[1..9] of char;
    shi50_ro_password: array[1..9] of char;
    szWhatever: array[1..256] of char;
  end;

  _PROCESS_MEMORY_COUNTERS = packed record
    cb: DWORD;
    PageFaultCount: DWORD;
    PeakWorkingSetSize: DWORD;
    WorkingSetSize: DWORD;
    QuotaPeakPagedPoolUsage: DWORD;
    QuotaPagedPoolUsage: DWORD;
    QuotaPeakNonPagedPoolUsage: DWORD;
    QuotaNonPagedPoolUsage: DWORD;
    PagefileUsage: DWORD;
    PeakPagefileUsage: DWORD;
  end;
  PROCESS_MEMORY_COUNTERS = _PROCESS_MEMORY_COUNTERS;
  PPROCESS_MEMORY_COUNTERS = ^_PROCESS_MEMORY_COUNTERS;
  TProcessMemoryCounters = _PROCESS_MEMORY_COUNTERS;
  PProcessMemoryCounters = ^_PROCESS_MEMORY_COUNTERS;

  DWORDLONG = int64;
  
  PMemoryStatusEx = ^TMemoryStatusEx;
  _MEMORYSTATUSEX = record
    dwLength: DWORD;
    dwMemoryLoad: DWORD;
    ullTotalPhys: DWORDLONG;
    ullAvailPhys: DWORDLONG;
    ullTotalPageFile: DWORDLONG;
    ullAvailPageFile: DWORDLONG;
    ullTotalVirtual: DWORDLONG;
    ullAvailVirtual: DWORDLONG;
    ullAvailExtendedVirtual: DWORDLONG;
  end;
  TMemoryStatusEx = _MEMORYSTATUSEX;
  MEMORYSTATUSEX = _MEMORYSTATUSEX;

  // Service Controller handle
  SC_HANDLE = THandle;

  // DSiEnumFiles callback
  TDSiEnumFilesCallback = procedure(const longFileName: string) of object;
  // DSiEnumFilesEx callback
  TDSiEnumFilesExCallback = procedure(const folder: string; S: TSearchRec;
    isAFolder: boolean; var stopEnum: boolean) of object;

  // DSiExecuteAndCapture callback
  TDSiOnNewLineCallback = procedure(const line: string; var runningTimeLeft_sec: integer)
    of object;

  TDSiFileTime = (ftCreation, ftLastAccess, ftLastModification);

{ Handles }

  // Pretty-print aliases
  TDSiFileHandle      = THandle;
  TDSiPipeHandle      = THandle;
  TDSiMutexHandle     = THandle;
  TDSiEventHandle     = THandle;
  TDSiSemaphoreHandle = THandle;

  procedure DSiCloseHandleAndInvalidate(var handle: THandle);
  procedure DSiCloseHandleAndNull(var handle: THandle);
  function  DSiMsgWaitForThreeObjectsEx(obj0, obj1, obj2: THandle;
    timeout: DWORD; wakeMask: DWORD; flags: DWORD): DWORD;
  function  DSiMsgWaitForTwoObjectsEx(obj0, obj1: THandle; timeout: DWORD;
    wakeMask: DWORD; flags: DWORD): DWORD;
  function  DSiWaitForThreeObjects(obj0, obj1, obj2: THandle; waitAll: boolean;
    timeout: DWORD): DWORD;
  function  DSiWaitForThreeObjectsEx(obj0, obj1, obj2: THandle; waitAll: boolean;
    timeout: DWORD; alertable: boolean): DWORD;
  function  DSiWaitForTwoObjects(obj0, obj1: THandle; waitAll: boolean;
    timeout: DWORD): DWORD;
  function  DSiWaitForTwoObjectsEx(obj0, obj1: THandle; waitAll: boolean;
    timeout: DWORD; alertable: boolean): DWORD;
  function  DSiWin32CheckHandle(handle: THandle): THandle;
  function  DSiWin32CheckNullHandle(handle: THandle): THandle;

{ Registry }

const
  KEY_WOW64_64KEY = $0100;

type
  {$IFDEF NeedRawByteString}
  RawByteString = AnsiString;
  {$ENDIF NeedRawByteString}

  TDSiRegistry = class(TRegistry)
  public
    function  ReadBinary(const name: string; const defval: RawByteString): RawByteString; overload;
    function  ReadBinary(const name: string; dataStream: TStream): boolean; overload;
    function  ReadBool(const name: string; defval: boolean): boolean;
    function  ReadDate(const name: string; defval: TDateTime): TDateTime;
    function  ReadFont(const name: string; font: TFont): boolean;
    function  ReadInt64(const name: string; defval: int64): int64;
    function  ReadInteger(const name: string; defval: integer): integer;
    function  ReadString(const name, defval: string): string;
    procedure ReadStrings(const name: string; strings: TStrings);
    function  ReadVariant(const name: string; defval: variant): variant;
    procedure WriteBinary(const name: string; data: RawByteString); overload;
    procedure WriteBinary(const name: string; data: TStream); overload;
    procedure WriteFont(const name: string; font: TFont);
    procedure WriteInt64(const name: string; value: int64);
    procedure WriteStrings(const name: string; strings: TStrings);
    procedure WriteVariant(const name: string; value: variant);
  end; { TDSiRegistry }

  function DSiCreateRegistryKey(const registryKey: string;
    root: HKEY = HKEY_CURRENT_USER): boolean;
  function DSiDeleteRegistryValue(const registryKey, name: string; root: HKEY =
    HKEY_CURRENT_USER; access: longword = KEY_SET_VALUE): boolean;
  function DSiKillRegistry(const registryKey: string;
    root: HKEY = HKEY_CURRENT_USER; access: longword = KEY_SET_VALUE): boolean;
  function DSiReadRegistry(const registryKey, name: string;
    defaultValue: Variant; root: HKEY = HKEY_CURRENT_USER;
    access: longword = KEY_QUERY_VALUE): Variant; overload;
  function DSiReadRegistry(const registryKey, name: string;
    defaultValue: int64; root: HKEY = HKEY_CURRENT_USER;
    access: longword = KEY_QUERY_VALUE): int64; overload;
  function DSiRegistryKeyExists(const registryKey: string;
    root: HKEY = HKEY_CURRENT_USER; access: longword = KEY_QUERY_VALUE): boolean;
  function DSiRegistryValueExists(const registryKey, name: string;
    root: HKEY = HKEY_CURRENT_USER; access: longword = KEY_QUERY_VALUE): boolean;
  function DSiWriteRegistry(const registryKey, name: string; value: int64;
    root: HKEY = HKEY_CURRENT_USER; access: longword = KEY_SET_VALUE): boolean; overload;
  function DSiWriteRegistry(const registryKey, name: string; value: Variant;
    root: HKEY = HKEY_CURRENT_USER; access: longword = KEY_SET_VALUE): boolean; overload;

{ Files }

type
  TShFileOpFlag = (fofAllowUndo, fofFilesOnly, fofMultiDestFiles, fofNoConfirmation,
    fofNoConfirmMkDir, fofNoConnectedElements, fofNoErrorUI, fofNoRecursion,
    fofNoRecurseReparse, fofRenameOnCollision, fofSilent, fofSimpleProgress,
    fofWantMappingHandle, fofWantNukeWarning, fofNoUI);
  TShFileOpFlags = set of TShFileOpFlag;

const
  FILE_LIST_DIRECTORY = $0001;
  FILE_SHARE_FULL     = FILE_SHARE_DELETE OR FILE_SHARE_READ OR FILE_SHARE_WRITE;

  FILE_ACTION_ADDED            = $00000001;
  FILE_ACTION_REMOVED          = $00000002;
  FILE_ACTION_MODIFIED         = $00000003;
  FILE_ACTION_RENAMED_OLD_NAME = $00000004;
  FILE_ACTION_RENAMED_NEW_NAME = $00000005;

  FOF_NOCONNECTEDELEMENTS = $2000;
  FOF_NORECURSION         = $1000;
  FOF_NORECURSEREPARSE    = $8000;
  FOF_WANTNUKEWARNING     = $4000;
  FOF_NO_UI               =  FOF_SILENT OR FOF_NOCONFIRMATION OR FOF_NOERRORUI OR FOF_NOCONFIRMMKDIR;

  CShFileOpFlagMappings: array [TShFileOpFlag] of FILEOP_FLAGS = (FOF_ALLOWUNDO,
    FOF_FILESONLY, FOF_MULTIDESTFILES, FOF_NOCONFIRMATION, FOF_NOCONFIRMMKDIR,
    FOF_NOCONNECTEDELEMENTS, FOF_NOERRORUI, FOF_NORECURSION, FOF_NORECURSEREPARSE,
    FOF_RENAMEONCOLLISION, FOF_SILENT, FOF_SIMPLEPROGRESS, FOF_WANTMAPPINGHANDLE,
    FOF_WANTNUKEWARNING, FOF_NO_UI);

  function  DSiCanWriteToFolder(const folderName: string): boolean;
  function  DSiCompressFile(fileHandle: THandle): boolean;
  function  DSiConnectToNetworkResource(const networkResource: string; const mappedLetter:
    string = ''; const username: string = ''; const password: string = ''): boolean;
  function  DSiCopyFileAnimated(ownerWindowHandle: THandle; sourceFile, destinationFile:
    string; var aborted: boolean; flags: TShFileOpFlags = [fofNoConfirmMkDir]): boolean;
  function  DSiCreateTempFolder: string;
  procedure DSiDeleteFiles(const folder, fileMask: string);
  function  DSiDeleteOnReboot(const fileName: string): boolean;
  procedure DSiDeleteTree(const folder: string; removeSubdirsOnly: boolean);
  function  DSiDeleteWithBatch(const fileName: string; rmDir: boolean = false): boolean;
  function  DSiDirectoryExistsW(const directory: WideString): boolean;
  function  DSiDisableWow64FsRedirection(var oldStatus: pointer): boolean;
  function  DSiDisconnectFromNetworkResource(mappedLetter: char; updateProfile: boolean =
    false): boolean;
  function  DSiEjectMedia(deviceLetter: char): boolean;
  procedure DSiEmptyFolder(const folder: string);
  function  DSiEmptyRecycleBin: boolean;
  function  DSiEnumFiles(const fileMask: string; attr: integer;
    enumCallback: TDSiEnumFilesCallback): integer;
  function  DSiEnumFilesEx(const fileMask: string; attr: integer;
    enumSubfolders: boolean; enumCallback: TDSiEnumFilesExCallback;
    maxEnumDepth: integer = 0): integer;
  procedure DSiEnumFilesToSL(const fileMask: string; attr: integer; fileList: TStrings;
    storeFullPath: boolean = false; enumSubfolders: boolean = false;
    maxEnumDepth: integer = 0);
  function  DSiFileExistsW(const fileName: WideString): boolean;
  function  DSiFileExtensionIs(const fileName, extension: string): boolean; overload;
  function  DSiFileExtensionIs(const fileName: string; extension: array of string):
    boolean; overload;
  function  DSiFileSize(const fileName: string): int64;
  function  DSiFileSizeW(const fileName: WideString): int64;
  function  DSiGetFolderSize(const folder: string; includeSubfolders: boolean): int64;
  function  DSiGetFileTime(const fileName: string; whatTime: TDSiFileTime): TDateTime;
  function  DSiGetFileTimes(const fileName: string; var creationTime, lastAccessTime,
    lastModificationTime: TDateTime): boolean;
  function  DSiGetLongPathName(const fileName: string): string;
  function  DSiGetNetworkResource(mappedLetter: char; var networkResource: string; var
    connectionIsAvailable: boolean): boolean;
  function  DSiGetSubstDrive(mappedLetter: char): string;
  function  DSiGetSubstPath(const path: string): string;
  function  DSiGetTempFileName(const prefix: string; const tempPath: string = ''): string;
  function  DSiGetTempPath: string;
  function  DSiGetUniqueFileName(const extension: string): string;
  function  DSiIsFileCompressed(const fileName: string): boolean;
  function  DSiIsFileCompressedW(const fileName: WideString): boolean;
  function  DSiKillFile(const fileName: string): boolean;
  function  DSiLoadMedia(deviceLetter: char): boolean;
  function  DSiMoveFile(const srcName, destName: string; overwrite: boolean = true): boolean;
  function  DSiMoveOnReboot(const srcName, destName: string): boolean;
  procedure DSiRemoveFolder(const folder: string);
  function  DSiRevertWow64FsRedirection(const oldStatus: pointer): boolean;
  function DSiSetFileTime(const fileName: string; dateTime: TDateTime;
    whatTime: TDSiFileTime): boolean;
  function  DSiSetFileTimes(const fileName: string; creationTime, lastAccessTime,
    lastModificationTime: TDateTime): boolean;
  function  DSiShareFolder(const folder, shareName, comment: string): boolean;
  function  DSiUncompressFile(fileHandle: THandle): boolean;
  function  DSiUnShareFolder(const shareName: string): boolean;

{ Processes }

  function  DSiAddAceToDesktop(desktop: HDESK; sid: PSID): boolean;
  function  DSiAddAceToWindowStation(station: HWINSTA; sid: PSID): boolean;
  function  DSiAffinityMaskToString(affinityMask: DWORD): string;
  function  DSiGetCurrentProcessHandle: THandle;
  function  DSiGetCurrentThreadHandle: THandle;
  function  DSiEnablePrivilege(const privilegeName: string): boolean;
  function  DSiExecute(const commandLine: string;
    visibility: integer = SW_SHOWDEFAULT; const workDir: string = '';
    wait: boolean = false): cardinal;
  function  DSiExecuteAndCapture(const app: string; output: TStrings; const workDir: string;
    var exitCode: longword; waitTimeout_sec: integer = 15; onNewLine: TDSiOnNewLineCallback
    = nil): cardinal;
  function  DSiExecuteAsUser(const commandLine, username, password: string;
    var winErrorCode: cardinal; const domain: string = '.';
    visibility: integer = SW_SHOWDEFAULT; const workDir: string = '';
    wait: boolean = false): cardinal; overload;
  function  DSiExecuteAsUser(const commandLine, username, password: string;
    var winErrorCode: cardinal; var processInfo: TProcessInformation;
    const domain: string = '.'; visibility: integer = SW_SHOWDEFAULT;
    const workDir: string = ''; wait: boolean = false;
    startInfo: PStartupInfo = nil): cardinal; overload;
  function  DSiGetProcessAffinity: string;
  function  DSiGetProcessAffinityMask: DWORD;
  function  DSiGetProcessID(const processName: string; var processID: DWORD): boolean;
  function  DSiGetProcessMemory(var memoryCounters: TProcessMemoryCounters): boolean;
    overload;
  function  DSiGetProcessMemory(process: THandle; var memoryCounters:
    TProcessMemoryCounters): boolean; overload;
  function  DSiGetProcessFileName(process: THandle; var processName: string): boolean;
  function  DSiGetProcessOwnerInfo(const processName: string; var user,
    domain: string): boolean; overload;
  function  DSiGetProcessOwnerInfo(processID: DWORD; var user,
    domain: string): boolean; overload;
  function  DSiGetProcessTimes(var creationTime: TDateTime; var userTime,
    kernelTime: int64): boolean; overload;
  function  DSiGetProcessTimes(process: THandle; var creationTime, exitTime: TDateTime;
    var userTime, kernelTime: int64): boolean; overload;
  function  DSiGetSystemAffinity: string;
  function  DSiGetSystemAffinityMask: DWORD;
  function  DSiGetThreadAffinity: string;
  function  DSiGetThreadAffinityMask: DWORD;
  function  DSiGetThreadTime: int64; overload;
  function  DSiGetThreadTime(thread: THandle): int64; overload;
  function  DSiGetThreadTimes(var creationTime: TDateTime; var userTime,
    kernelTime: int64): boolean; overload;
  function  DSiGetThreadTimes(thread: THandle; var creationTime, exitTime: TDateTime;
    var userTime, kernelTime: int64): boolean; overload;
  function  DSiImpersonateUser(const username, password: string;
    const domain: string = '.'): boolean;
  function  DSiIncrementWorkingSet(incMinSize, incMaxSize: integer): boolean;
  function  DSiIsDebugged: boolean;
  function  DSiLogonAs(const username, password: string;
    var logonHandle: THandle): boolean; overload;
  function  DSiLogonAs(const username, password, domain: string;
    var logonHandle: THandle): boolean; overload;
  function  DSiOpenURL(const URL: string; newBrowser: boolean = false): boolean;
  procedure DSiProcessThreadMessages;
  function  DSiRealModuleName: string;
  function  DSiRemoveAceFromDesktop(desktop: HDESK; sid: PSID): boolean;
  function  DSiRemoveAceFromWindowStation(station: HWINSTA; sid: PSID): boolean;
  function  DSiSetProcessAffinity(affinity: string): string;
  function  DSiSetProcessPriorityClass(const processName: string;
    priority: DWORD): boolean;
  function  DSiSetThreadAffinity(affinity: string): string;
  procedure DSiStopImpersonatingUser;
  function  DSiStringToAffinityMask(affinity: string): DWORD;
  function  DSiTerminateProcessById(processID: DWORD; closeWindowsFirst: boolean = true;
    maxWait_sec: integer = 10): boolean;
  procedure DSiTrimWorkingSet;
  function  DSiValidateProcessAffinity(affinity: string): string;
  function  DSiValidateProcessAffinityMask(affinityMask: DWORD): DWORD;
  function  DSiValidateThreadAffinity(affinity: string): string;
  function  DSiValidateThreadAffinityMask(affinityMask: DWORD): DWORD;
  procedure DSiYield;

{ Memory }

  procedure DSiFreePidl(pidl: PItemIDList);
  procedure DSiFreeMemAndNil(var mem: pointer);
  function  DSiGetGlobalMemoryStatus(var memoryStatus: TMemoryStatusEx): boolean;

{ Windows }

type
  TDSiExitWindows = (ewLogOff, ewForcedLogOff, ewPowerOff, ewForcedPowerOff, ewReboot,
    ewForcedReboot, ewShutdown, ewForcedShutdown);
                   
  function  DSiAllocateHWnd(wndProcMethod: TWndMethod): HWND;
  procedure DSiDeallocateHWnd(wnd: HWND);
  function  DSiDisableStandby: boolean;
  procedure DSiDisableX(hwnd: THandle);
  procedure DSiEnableX(hwnd: THandle);
  function  DSiExitWindows(exitType: TDSiExitWindows): boolean;
  function  DSiForceForegroundWindow(hwnd: THandle;
    restoreFirst: boolean = true): boolean;
  function  DSiGetClassName(hwnd: THandle): string;
  function  DSiGetProcessWindow(targetProcessID: cardinal): HWND;
  function  DSiGetWindowText(hwnd: THandle): string;
  procedure DSiProcessMessages(hwnd: THandle; waitForWMQuit: boolean = false);
  procedure DSiRebuildDesktopIcons;
  procedure DSiRefreshDesktop;
  procedure DSiSetTopMost(hwnd: THandle; onTop: boolean = true;
    activate: boolean = false);

{ Aero }

  function  DSiAeroDisable: boolean;
  function  DSiAeroEnable: boolean;
  function  DSiAeroIsEnabled: boolean;

{ Taskbar }

  function  DSiGetTaskBarPosition: integer;

{ Menus }

  function  DSiGetHotkey(const item: string): char;
  function  DSiGetMenuItem(menu: HMENU; item: integer): string;

{ Screen }

  procedure DSiDisableScreenSaver(out currentlyActive: boolean);
  procedure DSiEnableScreenSaver;
  function  DSiGetBitsPerPixel: integer;
  function  DSiGetBPP: integer;
  function  DSiGetDesktopSize: TRect;
  function  DSiIsFullScreen: boolean;
  procedure DSiMonitorOff;
  procedure DSiMonitorOn;
  procedure DSiMonitorStandby;
  function  DSiSetScreenResolution(width, height: integer): longint;

{ Rectangles }

  procedure DSiCenterRectInRect(const ownerRect: TRect; var clientRect: TRect);
  procedure DSiMakeRectFullyVisibleOnRect(const ownerRect: TRect; var clientRect: TRect);

{ Clipboard }

  function  DSiIsHtmlFormatOnClipboard: boolean;
  function  DSiGetHtmlFormatFromClipboard: string;
  procedure DSiCopyHtmlFormatToClipboard(const sHtml: string; const sText: string = '');
  
{ Information }

type
  TDSiBootType = (btNormal, btFailSafe, btFailSafeWithNetwork, btUnknown);
  TDSiWindowsVersion = (wvUnknown, wvWin31, wvWin95, wvWin95OSR2, wvWin98,
    wvWin98SE, wvWinME, wvWin9x, wvWinNT3, wvWinNT4, wvWin2000, wvWinXP,
    wvWinNT, wvWinServer2003, wvWinVista, wvWinServer2008, wvWinServer2008OrVistaSP1,
    wvWin7, wvWinServer2008R2, wvWin7OrServer2008R2);

  TDSiUIElement = (ueMenu, ueMessage, ueWindowCaption, ueStatus);

const
  CDSiWindowsVersionStr: array [TDSiWindowsVersion] of string = ('Unknown',
    'Windows 3.1', 'Windows 95', 'Windows 95 OSR 2', 'Windows 98',
    'Windows 98 SE', 'Windows Me', 'Windows 9x', 'Windows NT 3.5',
    'Windows NT 4', 'Windows 2000', 'Windows XP', 'Windows NT', 'Windows Server 2003',
    'Windows Vista', 'Windows Server 2008', 'Windows Server 2008 or Windows Vista SP1',
    'Windows 7', 'Windows Server 2008 R2', 'Windows 7 or Windows Server 2008 R2');

  VER_SUITE_BACKOFFICE     = $00000004; // Microsoft BackOffice components are installed.
  VER_SUITE_BLADE          = $00000400; // Windows Server 2003, Web Edition is installed.
  VER_SUITE_COMPUTE_SERVER = $00004000; // Windows Server 2003, Compute Cluster Edition is installed.
  VER_SUITE_DATACENTER     = $00000080; // Windows Server 2008 Datacenter, Windows Server 2003, Datacenter Edition, or Windows 2000 Datacenter Server is installed.
  VER_SUITE_ENTERPRISE     = $00000002; // Windows Server 2008 Enterprise, Windows Server 2003, Enterprise Edition, or Windows 2000 Advanced Server is installed. Refer to the Remarks section for more information about this bit flag.
  VER_SUITE_EMBEDDEDNT     = $00000040; // Windows XP Embedded is installed.
  VER_SUITE_PERSONAL       = $00000200; // Windows Vista Home Premium, Windows Vista Home Basic, or Windows XP Home Edition is installed.
  VER_SUITE_SINGLEUSERTS   = $00000100; // Remote Desktop is supported, but only one interactive session is supported. This value is set unless the system is running in application server mode.
  VER_SUITE_SMALLBUSINESS  = $00000001; // Microsoft Small Business Server was once installed on the system, but may have been upgraded to another version of Windows. Refer to the Remarks section for more information about this bit flag.
  VER_SUITE_SMALLBUSINESS_RESTRICTED
                           = $00000020; // Microsoft Small Business Server is installed with the restrictive client license in force. Refer to the Remarks section for more information about this bit flag.
  VER_SUITE_STORAGE_SERVER = $00002000; // Windows Storage Server 2003 R2 or Windows Storage Server 2003is installed.
  VER_SUITE_TERMINAL       = $00000010; // Terminal Services is installed. This value is always set.
  VER_SUITE_WH_SERVER      = $00008000; // Windows Home Server is installed.

  VER_NT_DOMAIN_CONTROLLER = $0000002; // The system is a domain controller and the operating system is Windows Server 2008, Windows Server 2003, or Windows 2000 Server.
  VER_NT_SERVER            = $0000003; // The operating system is Windows Server 2008, Windows Server 2003, or Windows 2000 Server.
                                       // Note that a server that is also a domain controller is reported as VER_NT_DOMAIN_CONTROLLER, not VER_NT_SERVER.
  VER_NT_WORKSTATION       = $0000001; // The operating system is Windows Vista, Windows XP Professional, Windows XP Home Edition, or Windows 2000 Professional.

type
  _OSVERSIONINFOEXA = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of AnsiChar; { Maintenance AnsiString for PSS usage }
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved: BYTE;
  end;
  {$EXTERNALSYM _OSVERSIONINFOEXA}
  _OSVERSIONINFOEXW = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of WideChar; { Maintenance WideString for PSS usage }
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved: BYTE;
  end;
  {$EXTERNALSYM _OSVERSIONINFOExW}
  _OSVERSIONINFEXO = _OSVERSIONINFOEXA;
  TOSVersionInfoExA = _OSVERSIONINFOEXA;
  TOSVersionInfoExW = _OSVERSIONINFOEXW;
  TOSVersionInfoEx = TOSVersionInfoExA;
  OSVERSIONINFOEXA = _OSVERSIONINFOEXA;
  {$EXTERNALSYM OSVERSIONINFOEXA}
  {$EXTERNALSYM OSVERSIONINFOEX}
  OSVERSIONINFOEXW = _OSVERSIONINFOEXW;
  {$EXTERNALSYM OSVERSIONINFOEXW}
  {$EXTERNALSYM OSVERSIONINFOEX}
  OSVERSIONINFOEX = OSVERSIONINFOEXA;

  {$IFNDEF NeedStartupInfo}
  _STARTUPINFOW = record
    cb: DWORD;
    lpReserved: PWideChar;
    lpDesktop: PWideChar;
    lpTitle: PWideChar;
    dwX: DWORD;
    dwY: DWORD;
    dwXSize: DWORD;
    dwYSize: DWORD;
    dwXCountChars: DWORD;
    dwYCountChars: DWORD;
    dwFillAttribute: DWORD;
    dwFlags: DWORD;
    wShowWindow: Word;
    cbReserved2: Word;
    lpReserved2: PByte;
    hStdInput: THandle;
    hStdOutput: THandle;
    hStdError: THandle;
  end;
  TStartupInfoW = _STARTUPINFOW;
  PStartupInfoW = ^TStartupInfoW;
  TStartupInfoA = TStartupInfo;
  {$ENDIF NeedStartupInfo}

  function  DSiGetAppCompatFlags(const exeName: string): string;
  function  DSiGetBootType: TDSiBootType;
  function  DSiGetCompanyName: string;
  function  DSiGetComputerName: string;
  function  DSiGetDefaultBrowser: string;
  function  DSiGetDirectXVer: string;
  function  DSiGetDiskLabel(disk: char): string;
  function  DSiGetDiskSerial(disk: char): string;
  function  DSiGetDomain: string;
  function  DSiGetEnvironmentVariable(const envVarName: string): string;
  function  DSiGetFolderLocation(const CSIDL: integer): string;
  procedure DSiGetKeyboardLayouts(layouts: TStrings);
  function  DSiGetMyDocumentsFolder: string;
  function  DSiGetProgramFilesFolder: string;
  function  DSiGetRegisteredOwner: string;
  function  DSiGetSystemFolder: string;
  function  DSiGetSystemLanguage: string;
  function  DSiGetSystemVersion: string;
  function  DSiGetTrueWindowsVersion: TDSiWindowsVersion;
  function  DSiGetUserName: string;
  function  DSiGetUserNameEx: string;
  function  DSiGetWindowsFolder: string;
  function  DSiGetWindowsVersion: TDSiWindowsVersion;
  function  DSiInitFontToSystemDefault(aFont: TFont; aElement: TDSiUIElement): boolean;
  function  DSiIsAdmin: boolean;
  function  DSiIsAdminLoggedOn: boolean;
  function  DSiIsDiskInDrive(disk: char): boolean;
  function  DSiIsWinNT: boolean;
  function  DSiIsWow64: boolean;
  function  DSiVerifyPassword(const username, password: string;
    const domain: string = '.'): boolean;

{ Install }

const // Firewall management constants.
  // NET_FW_IP_PROTOCOL
  NET_FW_IP_PROTOCOL_TCP = 6;
  NET_FW_IP_PROTOCOL_UDP = 17;

  // NET_FW_IP_VERSION
  NET_FW_IP_VERSION_V4  = 0;
  NET_FW_IP_VERSION_V6  = 1;
  NET_FW_IP_VERSION_ANY = 2;

  // NET_FW_POLICY_TYPE
  NET_FW_POLICY_GROUP     = 0;
  NET_FW_POLICY_LOCAL     = 1;
  NET_FW_POLICY_EFFECTIVE = 2;

  // NET_FW_PROFILE_TYPE
  NET_FW_PROFILE_DOMAIN   = 0;
  NET_FW_PROFILE_STANDARD = 1;
  NET_FW_PROFILE_CURRENT  = 2;

  // NET_FW_SCOPE
  NET_FW_SCOPE_ALL          = 0;
  NET_FW_SCOPE_LOCAL_SUBNET = 1;
  NET_FW_SCOPE_CUSTOM       = 2;

  // NET_FW_SERVICE_TYPE 
  NET_FW_SERVICE_FILE_AND_PRINT = 0;
  NET_FW_SERVICE_UPNP           = 1;
  NET_FW_SERVICE_REMOTE_DESKTOP = 2;
  NET_FW_SERVICE_NONE           = 3;

  // NET_FW_ACTION
  NET_FW_ACTION_BLOCK = 0;
  NET_FW_ACTION_ALLOW = 1;

  // NET_FW_EDGE_TRAVERSAL_TYPE
  NET_FW_EDGE_TRAVERSAL_TYPE_DENY          = 0;
  NET_FW_EDGE_TRAVERSAL_TYPE_ALLOW         = 1;
  NET_FW_EDGE_TRAVERSAL_TYPE_DEFER_TO_APP  = 2;
  NET_FW_EDGE_TRAVERSAL_TYPE_DEFER_TO_USER = 3;

  // NET_FW_MODIFY_STATE
  NET_FW_MODIFY_STATE_OK              = 0;
  NET_FW_MODIFY_STATE_GP_OVERRIDE     = 1;
  NET_FW_MODIFY_STATE_INBOUND_BLOCKED = 2;

  // NET_FW_PROFILE_TYPE2
  NET_FW_PROFILE2_DOMAIN  = 1;
  NET_FW_PROFILE2_PRIVATE = 2;
  NET_FW_PROFILE2_PUBLIC  = 4;
  NET_FW_PROFILE2_ALL     = $7FFFFFFF;

  // NET_FW_RULE_CATEGORY
  NET_FW_RULE_CATEGORY_BOOT       = 0;
  NET_FW_RULE_CATEGORY_STEALTH    = 1;
  NET_FW_RULE_CATEGORY_FIREWALL   = 2;
  NET_FW_RULE_CATEGORY_CONSEC     = 3;
  NET_FW_RULE_CATEGORY_MAX        = 4;

  // NET_FW_RULE_DIRECTION
  NET_FW_RULE_DIR_IN    = 0;
  NET_FW_RULE_DIR_OUT   = 1;

type // Firewall management types
  TDSiFwIPProtocol = (fwProtoTCP, fwProtoUDP);
  TDSiFwIPProtocols = set of TDSiFwIPProtocol;

  TDSiFwIPProfile = (fwProfileDomain, fwProfilePrivate, fwProfilePublic, fwProfileAll,
    fwProfileCurrent);
  TDSiFwIPProfiles = set of TDSiFwIPProfile;

  TDSiFwResolveConflict = (rcDuplicate, rcOverwrite, rcSkip); 

  function  DSiAddApplicationToFirewallExceptionList(const entryName,
    applicationFullPath: string; resolveConflict: TDSiFwResolveConflict = rcDuplicate;
    description: string = ''; grouping: string = '';
    serviceName: string = ''; protocols: TDSiFwIPProtocols = [fwProtoTCP];
    localPorts: string = '*'; profiles: TDSiFwIPProfiles = [fwProfileAll]): boolean;
  function  DSiAddApplicationToFirewallExceptionListAdvanced(const entryName,
    applicationFullPath: string; resolveConflict: TDSiFwResolveConflict = rcDuplicate;
    description: string = ''; grouping: string = '';
    serviceName: string = ''; protocols: TDSiFwIPProtocols = [fwProtoTCP];
    localPorts: string = '*'; profiles: TDSiFwIPProfiles = [fwProfileAll]): boolean;
  function  DSiAddApplicationToFirewallExceptionListXP(const entryName,
    applicationFullPath: string; resolveConflict: TDSiFwResolveConflict = rcDuplicate;
    profile: TDSiFwIPProfile = fwProfileCurrent): boolean;
  function  DSiAddPortToFirewallExceptionList(const entryName: string;
    portNumber: cardinal): boolean;
  function  DSiAddUninstallInfo(const displayName, uninstallCommand, publisher,
    URLInfoAbout, displayVersion, helpLink, URLUpdateInfo: string): boolean;
  function  DSiAutoRunApp(const applicationName, applicationPath: string;
    enabled: boolean = true): boolean;
  procedure DSiCreateShortcut(const fileName, displayName, parameters: string;
    folder: integer= CSIDL_STARTUP; const workDir: string = '');
  function  DSiDeleteShortcut(const displayName: string;
    folder: integer = CSIDL_STARTUP): boolean;
  procedure DSiEditShortcut(const lnkName, fileName, workDir, parameters: string);
  function  DSiFindApplicationInFirewallExceptionListAdvanced(const entryName: string;
    var rule: OleVariant): boolean;
  function  DSiFindApplicationInFirewallExceptionListXP(const entryName: string;
    var application: OleVariant; profile: TDSiFwIPProfile = fwProfileCurrent): boolean;
  function  DSiGetLogonSID(token: THandle; var logonSID: PSID): boolean;
  function  DSiGetShortcutInfo(const lnkName: string; var fileName, filePath, workDir,
    parameters: string): boolean;
  function  DSiGetUninstallInfo(const displayName: string;
    out uninstallCommand: string): boolean;
  function  DSiIsAutoRunApp(const applicationname: string): boolean;
  function  DSiRegisterActiveX(const fileName: string; registerDLL: boolean): HRESULT;
  procedure DSiRegisterRunOnce(const applicationName,
    applicationPath: string);
  function  DSiRemoveApplicationFromFirewallExceptionList(const entryName,
    applicationFullPath: string): boolean;
  function  DSiRemoveApplicationFromFirewallExceptionListAdvanced(const entryName: string): boolean;
  function  DSiRemoveApplicationFromFirewallExceptionListXP(const applicationFullPath: string): boolean; overload;
  function  DSiRemoveApplicationFromFirewallExceptionListXP(const applicationFullPath: string;
    profile: TDSiFwIPProfile): boolean; overload;
  procedure DSiRemoveRunOnce(const applicationName: string);
  function  DSiRemoveUninstallInfo(const displayName: string): boolean;
  function  DSiShortcutExists(const displayName: string;
    folder: integer = CSIDL_STARTUP): boolean;

{ Time }

type
  {:TTimer clone that uses DSiAllocateHwnd/DSiDeallocateHwnd instead of Delphi's
    AllocateHwnd/DeallocateHwnd. Intented to be dynamically created in threads and
    therefore not a TComponent descendant.
    @author  gabr
    @since   2007-05-30
  }
  TDSiTimer = class
  private
    dtEnabled     : boolean;
    dtInterval    : cardinal;
    dtOnTimer     : TNotifyEvent;
    dtTag         : longint;
    dtWindowHandle: HWND;
  protected
    procedure SetEnabled(value: boolean);
    procedure SetInterval(value: cardinal);
    procedure SetOnTimer(value: TNotifyEvent);
    procedure UpdateTimer;
    procedure WndProc(var msgRec: TMessage);
  public
    constructor Create(enabled: boolean = true; interval: cardinal = 1000; onTimer:
      TNotifyEvent = nil; tag: longint = 0);
    destructor  Destroy; override;
    property Enabled: boolean read dtEnabled write SetEnabled default true;
    property Interval: cardinal read dtInterval write SetInterval default 1000;
    property Tag: longint read dtTag write dtTag;
    property OnTimer: TNotifyEvent read dtOnTimer write SetOnTimer;
  end; { TDSiTimer }

  function  DSiDateTimeToFileTime(dateTime: TDateTime; var fileTime: TFileTime): boolean;
  //Following three functions are based on GetTickCount
  function  DSiElapsedSince(midTime, startTime: int64): int64;
  function  DSiElapsedTime(startTime: int64): int64;
  function  DSiElapsedTime64(startTime: int64): int64;
  function  DSiHasElapsed(startTime: int64; timeout_ms: DWORD): boolean;
  function  DSiHasElapsed64(startTime: int64; timeout_ms: DWORD): boolean;

  function  DSiFileTimeToDateTime(fileTime: TFileTime): TDateTime; overload;
  function  DSiFileTimeToDateTime(fileTime: TFileTime; var dateTime: TDateTime): boolean; overload;
  function  DSiFileTimeToMicroSeconds(fileTime: TFileTime): int64;
  function  DSiPerfCounterToMS(perfCounter: int64): int64;
  function  DSiPerfCounterToUS(perfCounter: int64): int64;
  function  DSiQueryPerfCounterAsUS: int64;
  function  DSiTimeGetTime64: int64;
  procedure DSiuSecDelay(delay: int64);

{ Interlocked }

function  DSiInterlockedDecrement64(var addend: int64): int64; register;
function  DSiInterlockedIncrement64(var addend: int64): int64; register;
function  DSiInterlockedExchangeAdd64(var addend: int64; value: int64): int64; register;
function  DSiInterlockedExchange64(var target: int64; value: int64): int64; register;
function  DSiInterlockedCompareExchange64(var destination: int64; exchange, comparand: int64): int64; register; overload;
function  DSiInterlockedCompareExchange64(destination: PInt64; exchange, comparand: int64): int64; register; overload;

{ DynaLoad }

const // composition action values for DSiDwmEnableComposition
  DWM_EC_DISABLECOMPOSITION = 0;
  DWM_EC_ENABLECOMPOSITION = 1;

type
  PModule = ^HMODULE;

  function  DSi9xNetShareAdd(serverName: PChar; shareLevel: smallint;
    buffer: pointer; size: word): integer; stdcall;
  function  DSi9xNetShareDel(serverName: PChar; netName: PChar;
    reserved: word): integer; stdcall;
  function  DSiCloseServiceHandle(hSCObject: SC_HANDLE): BOOL; stdcall;
  function  DSiCreateProcessAsUser(hToken: THandle;
    lpApplicationName, lpCommandLine: PChar; lpProcessAttributes,
    lpThreadAttributes: PSecurityAttributes; bInheritHandles: BOOL;
    dwCreationFlags: DWORD; lpEnvironment: pointer;
    lpCurrentDirectory: PChar; const lpStartupInfo: TStartupInfo;
    var lpProcessInformation: TProcessInformation): BOOL; stdcall;
  function  DSiCreateProcessWithLogonW(lpUsername, lpDomain, lpPassword: PWideChar;
    dwLogonFlags: DWORD; lpApplicationName, lpCommandLine: PWideChar;
    dwCreationFlags: DWORD; lpEnvironment: pointer; lpCurrentDirectory: PWideChar;
    const lpStartupInfo: TStartupInfoW; var lpProcessInformation: TProcessInformation): BOOL;
  function  DSiCreateEnvironmentBlock(var lpEnvironment: pointer; hToken: THandle;
    bInherit: BOOL): BOOL;
  function  DSiDestroyEnvironmentBlock(lpEnvironment: pointer): BOOL;
  function  DSiDwmEnableComposition(uCompositionAction: UINT): HRESULT; stdcall;
  function  DSiDwmIsCompositionEnabled(var pfEnabled: BOOL): HRESULT; stdcall;
  function  DSiEnumProcessModules(hProcess: THandle; lphModule: PModule; cb: DWORD;
    var lpcbNeeded: DWORD): BOOL; stdcall;
  function  DSiGetModuleFileNameEx(hProcess: THandle; hModule: HMODULE; lpFilename: PChar;
    nSize: DWORD): DWORD; stdcall;
  function  DSiGetProcAddress(const libFileName, procName: string): FARPROC;
  function  DSiGetProcessImageFileName(hProcess: THandle; lpImageFileName: PChar;
    nSize: DWORD): DWORD; stdcall;
  function  DSiGetProcessMemoryInfo(process: THandle; memCounters: PProcessMemoryCounters;
    cb: DWORD): boolean; stdcall;
  function  DSiGetTickCount64: int64; stdcall;
  function  DSiGetUserProfileDirectoryW(hToken: THandle; lpProfileDir: PWideChar;
    var lpcchSize: DWORD): BOOL; stdcall;
  function  DSiGlobalMemoryStatusEx(memStatus: PMemoryStatusEx): boolean; stdcall;
  function  DSiImpersonateLoggedOnUser(hToken: THandle): BOOL; stdcall;
  function  DSiIsWow64Process(hProcess: THandle; var wow64Process: BOOL): BOOL; stdcall;
  function  DSiLogonUser(lpszUsername, lpszDomain, lpszPassword: PChar;
    dwLogonType, dwLogonProvider: DWORD; var phToken: THandle): BOOL; stdcall;
  function  DSiNetApiBufferFree(buffer: pointer): cardinal; stdcall;
  function  DSiNetWkstaGetInfo(servername: PChar; level: cardinal;
    out bufptr: pointer): cardinal; stdcall;
  function  DSiNTNetShareAdd(serverName: PChar; level: integer; buf: PChar;
    var parm_err: integer): DWord; stdcall;
  function  DSiNTNetShareDel(serverName: PChar; netName: PWideChar;
    reserved: integer): DWord; stdcall;
  function  DSiOpenSCManager(lpMachineName, lpDatabaseName: PChar;
    dwDesiredAccess: DWORD): SC_HANDLE; stdcall;
  function  DSiRevertToSelf: BOOL; stdcall;
  function  DSiSetDllDirectory(path: PChar): boolean; stdcall;
  function  DSiSetSuspendState(hibernate: BOOL; forceCritical: BOOL = false;
    disableWakeEvent: BOOL = false): BOOL; stdcall;
  function  DSiSHEmptyRecycleBin(Wnd: HWND; pszRootPath: PChar;
    dwFlags: DWORD): HRESULT; stdcall;
  function  DSiWow64DisableWow64FsRedirection(var oldStatus: pointer): BOOL; stdcall;
  function  DSiWow64RevertWow64FsRedirection(const oldStatus: pointer): BOOL; stdcall;

{ Helpers }

{$IFDEF NeedUTF}
// UTF <-> 16-bit conversion. Same signature as D7 functions but custom implementation
// (taken from http://gp.17slon.com/gp/gptextstream.htm with permission).

type
  UTF8String = type string;
  PUTF8String = ^UTF8String;

function UTF8Encode(const ws: WideString): UTF8String;
function UTF8Decode(const sUtf: UTF8String): WideString;
{$ENDIF NeedUTF}

{ internals used in inline functions }

type
  TInterlockedCompareExchange64 = function(destination: pointer; exchange, comparand: int64): int64; stdcall;

var
  GInterlockedCompareExchange64: TInterlockedCompareExchange64 = nil;

implementation

uses
  Types,
  ComObj,
  ActiveX,
  {$IFDEF CONDITIONALCOMPILATION}
  Variants,
  {$ENDIF}
  TLHelp32,
  MMSystem;

const
  CAPISuffix = {$IFDEF Unicode}'W'{$ELSE}'A'{$ENDIF};

type
  IRestoreLastError = interface
  end; { IRestoreLastError }

  TRestoreLastError = class(TInterfacedObject, IRestoreLastError)
  private
    rleLastError: DWORD;
  public
    constructor Create;
    destructor  Destroy; override;
  end; { TRestoreLastError }

  TBackgroundTask = class
  private
    btWaitObject: THandle;
  public
    constructor Create(waitObject: THandle);
    procedure Awaited; virtual; abstract;
    property WaitObject: THandle read btWaitObject;
  end; { TBackgroundTask }

  TCleanupAces = class(TBackgroundTask)
  private
    caDesktop      : HDESK;
    caSid          : PSID;
    caWindowStation: HWINSTA;
  public
    constructor Create(waitObject: THandle; windowStation: HWINSTA; desktop: HDESK;
      sid: PSID);
    destructor  Destroy; override;
    procedure Awaited; override;
  end; { TCleanupAces }

  TBackgroundThread = class(TThread)
  private
    btTask: TBackgroundTask;
  public
    constructor Create(task: TBackgroundTask);
    procedure Execute; override;
  end; { TBackgroundThread }

  T9xNetShareAdd = function(serverName: PChar; shareLevel: smallint;
    buffer: pointer; size: word): integer; stdcall;
  T9xNetShareDel = function(serverName: PChar; netName: PChar;
    reserved: word): integer; stdcall;
  TCloseServiceHandle = function(hSCObject: SC_HANDLE): BOOL; stdcall;
  TCreateProcessAsUser = function(hToken: THandle;
    lpApplicationName: PChar; lpCommandLine: PChar; lpProcessAttributes,
    lpThreadAttributes: PSecurityAttributes; bInheritHandles: BOOL;
    dwCreationFlags: DWORD; lpEnvironment: pointer;
    lpCurrentDirectory: PChar; const lpStartupInfo: TStartupInfo;
    var lpProcessInformation: TProcessInformation): BOOL; stdcall;
  TCreateProcessWithLogonW = function(lpUsername, lpDomain, lpPassword: PWideChar;
    dwLogonFlags: DWORD; lpApplicationName, lpCommandLine: PWideChar;
    dwCreationFlags: DWORD; lpEnvironment: pointer; lpCurrentDirectory: PWideChar;
    const lpStartupInfo: TStartupInfoW;
    var lpProcessInformation: TProcessInformation): BOOL; stdcall;
  TCreateEnvironmentBlock = function (var lpEnvironment: pointer; hToken: THandle;
    bInherit: BOOL): BOOL; stdcall;
  TDestroyEnvironmentBlock = function (lpEnvironment: pointer): BOOL; stdcall;
  TDwmEnableComposition = function(uCompositionAction: UINT): HRESULT; stdcall;
  TDwmIsCompositionEnabled = function(var pfEnabled: BOOL): HRESULT; stdcall;
  TEnumProcessModules = function(hProcess: THandle; lphModule: PModule; cb: DWORD;
    var lpcbNeeded: DWORD): BOOL; stdcall;
  TGetLongPathName = function(lpszShortPath, lpszLongPath: PChar;
    cchBuffer: DWORD): DWORD; stdcall;
  TGetModuleFileNameEx = function(hProcess: THandle; hModule: HMODULE; lpFilename: PChar;
    nSize: DWORD): DWORD; stdcall;
  TGetProcessImageFileName = function(hProcess: THandle; lpImageFileName: PChar;
    nSize: DWORD): DWORD; stdcall;
  TGetProcessMemoryInfo = function(process: THandle; memCounters: PProcessMemoryCounters;
    cb: DWORD): boolean; stdcall;
  TGetTickCount64 = function: int64; stdcall;
  TGetUserProfileDirectoryW = function (hToken: THandle; lpProfileDir: PWideChar;
    var lpcchSize: DWORD): BOOL; stdcall;
  TGlobalMemoryStatusEx = function(memStatus: PMemoryStatusEx): boolean; stdcall;
  TImpersonateLoggedOnUser = function(hToken: THandle): BOOL; stdcall;
  TIsWow64Process = function(hProcess: THandle; var wow64Process: BOOL): BOOL; stdcall;
  TLogonUser = function(lpszUsername, lpszDomain, lpszPassword: LPCSTR;
    dwLogonType, dwLogonProvider: DWORD; var phToken: THandle): BOOL; stdcall;
  TNetApiBufferFree = function(buffer: pointer): cardinal; stdcall;
  TNetWkstaGetInfo = function(servername: PChar; level: cardinal;
    out bufptr: pointer): cardinal; stdcall;
  TNTNetShareAdd = function(serverName: PChar; level: integer; buf: PChar;
    var parm_err: integer): DWord; stdcall;
  TNTNetShareDel = function(serverName: PChar; netName: PWideChar;
    reserved: integer): DWord; stdcall;
  TOpenSCManager = function(lpMachineName, lpDatabaseName: PChar;
    dwDesiredAccess: DWORD): SC_HANDLE; stdcall;
  TRevertToSelf = function: BOOL; stdcall;
  TSetDllDirectory = function(path: PChar): boolean; stdcall;
  TSetSuspendState = function(hibernate, forceCritical, disableWakeEvent: BOOL): BOOL; stdcall;
  TSHEmptyRecycleBin = function(wnd: HWND; pszRootPath: PChar;
    dwFlags: DWORD): HRESULT; stdcall;
  TWow64DisableWow64FsRedirection = function(var oldStatus: pointer): BOOL; stdcall;
  TWow64RevertWow64FsRedirection = function(const oldStatus: pointer): BOOL; stdcall;

const
  G9xNetShareAdd: T9xNetShareAdd = nil;
  G9xNetShareDel: T9xNetShareDel = nil;
  GCloseServiceHandle: TCloseServiceHandle = nil;
  GCreateProcessAsUser: TCreateProcessAsUser = nil;
  GCreateProcessWithLogonW: TCreateProcessWithLogonW = nil;
  GCreateEnvironmentBlock: TCreateEnvironmentBlock = nil;
  GDestroyEnvironmentBlock: TDestroyEnvironmentBlock = nil;
  GDwmEnableComposition: TDwmEnableComposition = nil;
  GDwmIsCompositionEnabled: TDwmIsCompositionEnabled = nil;
  GEnumProcessModules: TEnumProcessModules = nil;
  GGetModuleFileNameEx: TGetModuleFileNameEx = nil;
  GGetLongPathName: TGetLongPathName = nil;
  GGetProcessImageFileName: TGetProcessImageFileName = nil;
  GGetProcessMemoryInfo: TGetProcessMemoryInfo = nil;
  GGetTickCount64: TGetTickCount64 = nil;
  GGetUserProfileDirectoryW: TGetUserProfileDirectoryW = nil;
  GGlobalMemoryStatusEx: TGlobalMemoryStatusEx = nil;
  GImpersonateLoggedOnUser: TImpersonateLoggedOnUser = nil;
  GIsWow64Process: TIsWow64Process = nil;
  GLogonUser: TLogonUser = nil;
  GNetApiBufferFree: TNetApiBufferFree = nil;
  GNetWkstaGetInfo: TNetWkstaGetInfo = nil;
  GNTNetShareAdd: TNTNetShareAdd = nil;
  GNTNetShareDel: TNTNetShareDel = nil;
  GOpenSCManager: TOpenSCManager = nil;
  GRevertToSelf: TRevertToSelf = nil;
  GSetDllDirectory: TSetDllDirectory = nil;
  GSetSuspendState: TSetSuspendState = nil;
  GSHEmptyRecycleBin: TSHEmptyRecycleBin = nil;
  GWow64DisableWow64FsRedirection: TWow64DisableWow64FsRedirection = nil;
  GWow64RevertWow64FsRedirection: TWow64RevertWow64FsRedirection = nil;

{$IFOPT R+} {$DEFINE RestoreR} {$ELSE} {$UNDEF RestoreR} {$ENDIF}

{ Helpers }

  procedure CreateProcessWatchdog(task: TBackgroundTask);
  begin
    TBackgroundThread.Create(task);
  end; { CreateProcessWatchdog }

  function FileOpenSafe(fileName: string; var fileHandle: textfile;
    diskRetryDelay, diskRetryCount: integer): boolean;
  var
    dum: integer;
  begin
    Assign (fileHandle, fileName);
    {$I-}
    repeat
      if FileExists(fileName) then
        Reset(fileHandle)
      else
        Rewrite (fileHandle);
      dum := IOResult;
      if (dum in [ERROR_SHARING_VIOLATION, ERROR_LOCK_VIOLATION]) and
         (diskRetryDelay > 0) then
      begin
        Sleep(diskRetryDelay);
        if diskRetryCount > 0 then Dec(diskRetryCount);
      end;
    until (not (dum in [ERROR_SHARING_VIOLATION, ERROR_LOCK_VIOLATION])) or
          (diskRetryCount = 0);
    {$I+}
    Result := (dum = 0);
  end; { FileOpenSafe }

{$IFDEF NeedUTF}
  {:Convers buffer of WideChars into UTF-8 encoded form. Target buffer must be
    pre-allocated and large enough (each WideChar will use at most three bytes
    in UTF-8 encoding).                                                            <br>
    RFC 2279 (http://www.ietf.org/rfc/rfc2279.txt) describes the conversion:       <br>
    $0000..$007F => $00..$7F                                                       <br>
    $0080..$07FF => 110[bit10..bit6] 10[bit5..bit0]                                <br>
    $0800..$FFFF => 1110[bit15..bit12] 10[bit11..bit6] 10[bit5..bit0]
    @param   unicodeBuf   Buffer of WideChars.
    @param   uniByteCount Size of unicodeBuf, in bytes.
    @param   utf8Buf      Pre-allocated buffer for UTF-8 encoded result.
    @returns Number of bytes used in utf8Buf buffer.
    @since   2.01
  }
  function WideCharBufToUTF8Buf(const unicodeBuf; uniByteCount: integer;
    var utf8Buf): integer;
  var
    iwc: integer;
    pch: PChar;
    pwc: PWideChar;
    wc : word;
  
    procedure AddByte(b: byte);
    begin
      pch^ := char(b);
      Inc(pch);
    end; { AddByte }
  
  begin { WideCharBufToUTF8Buf }
    pwc := @unicodeBuf;
    pch := @utf8Buf;
    for iwc := 1 to uniByteCount div SizeOf(WideChar) do begin
      wc := Ord(pwc^);
      Inc(pwc);
      if (wc >= $0001) and (wc <= $007F) then begin
        AddByte(wc AND $7F);
      end
      else if (wc >= $0080) and (wc <= $07FF) then begin
        AddByte($C0 OR ((wc SHR 6) AND $1F));
        AddByte($80 OR (wc AND $3F));
      end
      else begin // (wc >= $0800) and (wc <= $FFFF)
        AddByte($E0 OR ((wc SHR 12) AND $0F));
        AddByte($80 OR ((wc SHR 6) AND $3F));
        AddByte($80 OR (wc AND $3F));
      end;
    end; //for
    Result := integer(pch)-integer(@utf8Buf);
  end; { WideCharBufToUTF8Buf }
  
  {:Converts UTF-8 encoded buffer into WideChars. Target buffer must be
    pre-allocated and large enough (at most utfByteCount number of WideChars will
    be generated).                                                                 <br>
    RFC 2279 (http://www.ietf.org/rfc/rfc2279.txt) describes the conversion:       <br>
    $00..$7F => $0000..$007F                                                       <br>
    110[bit10..bit6] 10[bit5..bit0] => $0080..$07FF                                <br>
    1110[bit15..bit12] 10[bit11..bit6] 10[bit5..bit0] => $0800..$FFFF
    @param   utf8Buf      UTF-8 encoded buffer.
    @param   utfByteCount Size of utf8Buf, in bytes.
    @param   unicodeBuf   Pre-allocated buffer for WideChars.
    @param   leftUTF8     Number of bytes left in utf8Buf after conversion (0, 1,
                          or 2).
    @returns Number of bytes used in unicodeBuf buffer.
    @since   2.01
  }
  function UTF8BufToWideCharBuf(const utf8Buf; utfByteCount: integer;
   var unicodeBuf; var leftUTF8: integer): integer;
  var
    c1 : byte;
    c2 : byte;
    ch : byte;
    pch: PChar;
    pwc: PWideChar;
  begin
    pch := @utf8Buf;
    pwc := @unicodeBuf;
    leftUTF8 := utfByteCount;
    while leftUTF8 > 0 do begin
      ch := byte(pch^);
      Inc(pch);
      if (ch AND $80) = 0 then begin // 1-byte code
        word(pwc^) := ch;
        Inc(pwc);
        Dec(leftUTF8);
      end
      else if (ch AND $E0) = $C0 then begin // 2-byte code
        if leftUTF8 < 2 then
          break;
        c1 := byte(pch^);
        Inc(pch);
        word(pwc^) := (word(ch AND $1F) SHL 6) OR (c1 AND $3F);
        Inc(pwc);
        Dec(leftUTF8,2);
      end
      else begin // 3-byte code
        if leftUTF8 < 3 then
          break;
        c1 := byte(pch^);
        Inc(pch);
        c2 := byte(pch^);
        Inc(pch);
        word(pwc^) :=
          (word(ch AND $0F) SHL 12) OR
          (word(c1 AND $3F) SHL 6) OR
          (c2 AND $3F);
        Inc(pwc);
        Dec(leftUTF8,3);
      end;
    end; //while
    Result := integer(pwc)-integer(@unicodeBuf);
  end; { UTF8BufToWideCharBuf }

  function UTF8Encode(const ws: WideString): UTF8String;
  begin
    if ws = '' then
      Result := ''
    else begin
      SetLength(Result, Length(ws)*3); // worst case - 3 bytes per character
      SetLength(Result, WideCharBufToUTF8Buf(ws[1], Length(ws)*SizeOf(WideChar),
        Result[1]));
    end;
  end; { UTF8Encode }

  function UTF8Decode(const sUtf: UTF8String): WideString;
  var
    leftUtf: integer;
  begin
    if sUtf = '' then
      Result := ''
    else begin
      SetLength(Result, Length(sUtf)); // worst case - 1 widechar per character
      SetLength(Result, UTF8BufToWideCharBuf(sUtf[1], Length(sUtf), Result[1], leftUtf)
        div SizeOf(WideChar));
    end;
  end; { UTF8Decode }
{$ENDIF NeedUTF}

{ Handles }

  {:Closes handle (if it is not already INVALID_HANDLE_VALUE) and sets it to
    INVALID_HANDLE_VALUE.
    @author  gabr
    @since   2002-11-25
  }
  procedure DSiCloseHandleAndInvalidate(var handle: THandle);
  begin
    if handle <> INVALID_HANDLE_VALUE then begin
      CloseHandle(handle);
      handle := INVALID_HANDLE_VALUE;
    end;
  end; { DSiCloseHandleAndInvalidate }

  {:Closes handle (if it is not already 0) and sets it to 0.
    @author  gabr
    @since   2002-11-25
  }
  procedure DSiCloseHandleAndNull(var handle: THandle);
  begin
    if handle <> 0 then begin
      CloseHandle(handle);
      handle := 0;
    end;
  end; { DSiCloseHandleAndNull }

  {:Shortcut for WaitForMultipleObjects with two objects.
    @author  gabr
    @since   2002-11-25
  }        
  function DSiWaitForTwoObjects(obj0, obj1: THandle; waitAll: boolean;
    timeout: DWORD): DWORD;
  var
    handles: array [0..1] of THandle;
  begin
    handles[0] := obj0;
    handles[1] := obj1;
    Result := WaitForMultipleObjects(2, @handles, waitAll, timeout);
  end; { DSiWaitForTwoObjects }

  {:Shortcut for WaitForMultipleObjectsEx with two objects.
    @author  gabr
    @since   2002-11-25
  }        
  function DSiWaitForTwoObjectsEx(obj0, obj1: THandle; waitAll: boolean;
    timeout: DWORD; alertable: boolean): DWORD;
  var
    handles: array [0..1] of THandle;
  begin
    handles[0] := obj0;
    handles[1] := obj1;
    Result := WaitForMultipleObjectsEx(2, @handles, waitAll, timeout, alertable);
  end; { DSiWaitForTwoObjectsEx }

  {:As Win32Check, only used for file handles.
    @author  gabr
    @since   2003-11-12
  }        
  function DSiWin32CheckHandle(handle: THandle): THandle;
  begin
    Win32Check(handle <> INVALID_HANDLE_VALUE);
    Result := handle;
  end; { TDSiRegistry.DSiWin32CheckHandle }

  {:As Win32Check, only used for various handles.
    @author  gabr
    @since   2005-07-11
  }
  function DSiWin32CheckNullHandle(handle: THandle): THandle;
  begin
    Win32Check(handle <> 0);
    Result := handle;
  end; { TDSiRegistry.DSiWin32CheckNullHandle }

  {:Shortcut for MsgWaitForMultipleObjects with two objects.
    @author  gabr
    @since   2002-11-25
  }
  function DSiMsgWaitForTwoObjectsEx(obj0, obj1: THandle; timeout: DWORD;
    wakeMask: DWORD; flags: DWORD): DWORD;
  var
    handles: array [0..1] of THandle;
  begin
    handles[0] := obj0;
    handles[1] := obj1;
    Result := MsgWaitForMultipleObjectsEx(2, handles, timeout, wakeMask, flags);
  end; { DSiWaitForThreeObjects }

  {:Shortcut for WaitForMultipleObjects with three objects.
    @author  gabr
    @since   2002-11-25
  }
  function DSiWaitForThreeObjects(obj0, obj1, obj2: THandle; waitAll: boolean;
    timeout: DWORD): DWORD;
  var
    handles: array [0..2] of THandle;
  begin
    handles[0] := obj0;
    handles[1] := obj1;
    handles[2] := obj2;
    Result := WaitForMultipleObjects(3, @handles, waitAll, timeout);
  end; { DSiWaitForThreeObjects }

  {:Shortcut for WaitForMultipleObjectsEx with three objects.
    @author  gabr
    @since   2002-11-25
  }
  function DSiWaitForThreeObjectsEx(obj0, obj1, obj2: THandle; waitAll: boolean;
    timeout: DWORD; alertable: boolean): DWORD;
  var
    handles: array [0..2] of THandle;
  begin
    handles[0] := obj0;
    handles[1] := obj1;
    handles[2] := obj2;
    Result := WaitForMultipleObjectsEx(3, @handles, waitAll, timeout, alertable);
  end; { DSiWaitForThreeObjectsEx }

  {:Shortcut for MsgWaitForMultipleObjectsEx with three objects.
    @author  gabr
    @since   2002-11-25
  }        
  function DSiMsgWaitForThreeObjectsEx(obj0, obj1, obj2: THandle;
    timeout: DWORD; wakeMask: DWORD; flags: DWORD): DWORD;
  var
    handles: array [0..2] of THandle;
  begin
    handles[0] := obj0;
    handles[1] := obj1;
    handles[2] := obj2;
    Result := MsgWaitForMultipleObjectsEx(3, handles, timeout, wakeMask, flags);
  end; { DSiWaitForThreeObjectsEx }

{ Registry }

  {:Reads binary from the registry returning default value if name doesn't exist in the
    open key. Includes special handling for integer and string keys.
    @author  Lee_Nover
    @since   2004-11-29
  }
  function TDSiRegistry.ReadBinary(const name: string; const defval: RawByteString): RawByteString;
  begin
    try
      if GetDataSize(name) < 0 then
        Abort; // D4 does not generate an exception!
      case GetDataType(name) of
        rdInteger:
          Result := RawByteString(IntToStr(inherited ReadInteger(name)));
        rdBinary:
          begin
            SetLength(Result, GetDataSize(name));
            SetLength(Result, ReadBinaryData(name, pointer(Result)^, Length(Result)));
          end; //rdBinary
        else
          Result := RawByteString(inherited ReadString(name));
      end;
    except ReadBinary := defval; end;
  end; { TDSiRegistry.ReadBinary }

  {:Reads binary from the registry. Overwrites 'dataStream'. Keeps data stream unchanged
    if value is not found in the registry.
    Includes special handling for integer and string keys.
    @author  gabr
    @returns True if value exists in the registry.
    @since   2005-02-13
  }
  function TDSiRegistry.ReadBinary(const name: string; dataStream: TStream): boolean;
  var
    i: integer;
    s: RawByteString;
  begin
    try
      if GetDataSize(name) < 0 then
        Abort; // D4 does not generate an exception!
      dataStream.Size := 0;
      case GetDataType(name) of
        rdInteger:
          begin
            i := ReadInteger(name, 0);
            dataStream.Write(i, SizeOf(i));
          end; //rdInteger
        rdBinary:
          begin
            if dataStream is TMemoryStream then begin
              dataStream.Size := GetDataSize(name);
              dataStream.Position := 0;
              ReadBinaryData(name, TMemoryStream(dataStream).Memory^, dataStream.Size);
            end
            else begin
              s := ReadBinary(name, '');
              if s <> '' then
                dataStream.Write(s[1], Length(s));
            end;
          end; //rdBinary
        else
          begin
            s := RawByteString(ReadString(name, ''));
            if s <> '' then
              dataStream.Write(s[1], Length(s));
          end; //else
      end; //case
      Result := true;
    except Result := false; end;
  end; { TDSiRegistry.ReadBinary }

  {:Reads boolean value from the registry returning default value if name doesn't exist in
    the open key.
    @author  gabr
    @since   2002-11-25
  }
  function TDSiRegistry.ReadBool(const name: string; defval: boolean): boolean;
  begin
    try
      if GetDataSize(name) < 0 then
        Abort; // D4 does not generate an exception!
      ReadBool := inherited ReadBool(name);
    except ReadBool := defval; end;
  end; { TDSiRegistry.ReadBool }

  {:Reads date-time from the registry returning default value if name doesn't
    exist in the open key.
    @author  gabr
    @since   2002-11-25
  }
  function TDSiRegistry.ReadDate(const name: string; defval: TDateTime): TDateTime;
  begin
    try
      if GetDataSize(name) < 0 then
        Abort; // D4 does not generate an exception!
      ReadDate := inherited ReadDate(name);
    except ReadDate := defval; end;
  end; { TDSiRegistry.ReadDate }

  {:Reads TFont from the registry.
    @author  gabr
    @since   2002-11-25
  }        
  function TDSiRegistry.ReadFont(const name: string; font: TFont): boolean;
  var
    istyle: integer;
    fstyle: TFontStyles;
  begin
    Result := false;
    if GetDataSize(name) > 0 then begin
      font.Charset := ReadInteger(name+'_charset', font.Charset);
      font.Color   := ReadInteger(name+'_color', font.Color);
      font.Height  := ReadInteger(name+'_height', font.Height);
      font.Name    := ReadString(name, font.Name);
      font.Pitch   := TFontPitch(ReadInteger(name+'_pitch', Ord(font.Pitch)));
      font.Size    := ReadInteger(name+'_size', font.Size);
      fstyle := font.Style;
      istyle := 0;
      Move(fstyle, istyle, SizeOf(TFontStyles));
      istyle := ReadInteger(name+'_style', istyle);
      Move(istyle, fstyle, SizeOf(TFontStyles));
      font.Style := fstyle;
      Result := true;
    end;
  end; { TDSiRegistry.ReadFont }

  {:Reads integer from the registry returning default value if name doesn't
    exist in the open key.
    @author  gabr
    @since   2002-11-25
  }
  function TDSiRegistry.ReadInteger(const name: string; defval: integer): integer;
  begin
    try
      if GetDataSize(name) < 0 then
        Abort; // D4 does not generate an exception!
      if GetDataType(name) = rdInteger then
        Result := inherited ReadInteger(name)
      else if (GetDataType(name) = rdBinary) and (GetDataSize(name) = SizeOf(Result)) then
        ReadBinaryData(name, Result, SizeOf(Result))
      else
        Result := StrToIntDef(ReadString(name, IntToStr(defval)), defval);
    except ReadInteger := defval; end;
  end; { TDSiRegistry.ReadInteger }

  {:Reads 64-bit integer from the registry returning default value if name
    doesn't exist in the open key.
    @author  gabr
    @since   2002-11-25
  }
  function TDSiRegistry.ReadInt64(const name: string; defval: int64): int64;
  begin
    Result := StrToInt64Def(ReadString(name, '!'), defval);
  end; { TDSiRegistry.ReadInt64 }

  {:Reads string from the registry returning default value if name doesn't exist
    in the open key.
    @author  gabr
    @since   2002-11-25
  }
  function TDSiRegistry.ReadString(const name, defval: string): string;
  begin
    Result := defval;
    try
      if GetDataSize(name) < 0 then
        Exit;
      if GetDataType(name) = rdInteger then
        Result := IntToStr(inherited ReadInteger(name))
      else
        Result := inherited ReadString(name);
    except Result := defval; end;
  end; { TDSiRegistry.ReadString }

  {:Writes a MULTI_SZ value into the TStrings object.
    @author  Colin Wilson, borland.public.delphi.vcl.components.using
    @since   2003-10-02
  }
  procedure TDSiRegistry.ReadStrings(const name: string; strings: TStrings);
  var
    buffer   : PChar;
    p        : PChar;
    valueLen : DWORD;
    valueType: DWORD;
  begin
    strings.Clear;
    SetLastError(RegQueryValueEx(CurrentKey, PChar(name), nil, @valueType, nil,
      @valueLen));
    if GetLastError <> ERROR_SUCCESS then
      raise ERegistryException.CreateFmt('Unable read MULTI_SZ value. %s',
        [SysErrorMessage(GetLastError)])
    else if valueType <> REG_MULTI_SZ then
      raise ERegistryException.Create('String list expected.')
    else begin
      GetMem(buffer, valueLen);
      try
        RegQueryValueEx(CurrentKey, PChar(name), nil, nil, PByte(buffer),
          @valueLen);
        p := buffer;
        while p^ <> #0 do begin
          strings.Add(p);
          Inc (p, LStrLen(p) + 1);
        end
      finally FreeMem(buffer); end
    end;
  end; { TDSiRegistry.ReadStrings }

  {:Reads variant (string, integer, boolean, or date-time) from the registry
    returning default value if name doesn't exist in the open key.
    @author  gabr
    @since   2002-11-25
  }
  function TDSiRegistry.ReadVariant(const name: string; defval: variant): variant;
  begin
    case VarType(defval) of
      varInteger: Result := ReadInteger(name,defval);
      varBoolean: Result := ReadBool(name,defval);
      varString : Result := ReadString(name,defval);
      {$IFDEF Unicode}
      varOleStr,
      varUString: Result := ReadString(name, defval);
      {$ELSE}
      varOleStr : Result := UTF8Decode(ReadString(name, UTF8Encode(defval)));
      {$ENDIF Unicode}
      varDate   : Result := ReadDate(name,defval);
      else raise Exception.Create('TDSiRegistry.ReadVariant: Invalid value type!');
    end;
  end; { TDSiRegistry.ReadVariant }

  {:Writes string as binary into the registry.
    @author  Lee_Nover
    @since   2004-11-29
  }
  procedure TDSiRegistry.WriteBinary(const name: string; data: RawByteString);
  begin
    if data = '' then
      WriteBinaryData(name, data, 0)
    else
      WriteBinaryData(name, data[1], Length(data));
  end; { TDSiRegistry.WriteBinary }

  {:Writes stream into binary registry entry.
    @author  gabr
    @since   2005-02-13
  }        
  procedure TDSiRegistry.WriteBinary(const name: string; data: TStream);
  var
    ms: TMemoryStream;
  begin
    if data is TMemoryStream then
      WriteBinaryData(name, TMemoryStream(data).Memory^, data.Size)
    else begin
      ms := TMemoryStream.Create;
      try
        ms.CopyFrom(data, 0);
        WriteBinaryData(name, ms.Memory^, ms.Size);
      finally FreeAndNil(ms); end;
    end;
  end; { TDSiRegistry.WriteBinary }

  {:Writes 64-bit integer into the registry.
    @author  gabr
    @since   2002-11-25
  }
  procedure TDSiRegistry.WriteInt64(const name: string; value: int64);
  begin
    WriteString(name, IntToStr(value));
  end; { TDSiRegistry.WriteInt64 }

  {:Writes variant (string, integer, boolean, or date-time) into the registry.
    @author  gabr
    @since   2002-11-25
  }
  procedure TDSiRegistry.WriteVariant(const name: string; value: variant);
  begin
    case VarType(value) of
      varByte,
      varWord,
      varLongWord,
      varInteger: WriteInteger(name,value);
      varBoolean: WriteBool(name,value);
      varString : WriteString(name,value);
      {$IFDEF Unicode}
      varOleStr,
      varUString: WriteString(name, value);
      {$ELSE}
      varOleStr : WriteString(name,UTF8Encode(value));
      {$ENDIF Unicode}
      varDate   : WriteDate(name,value);
      else raise Exception.Create('TDSiRegistry.WriteVariant: Invalid value type!');
    end;
  end; { TDSiRegistry.WriteVariant }

  {:Writes TFont into the registry.
    @author  gabr
    @since   2002-11-25
  }
  procedure TDSiRegistry.WriteFont(const name: string; font: TFont);
  var
    istyle: integer;
    fstyle: TFontStyles;
  begin
    WriteInteger(name+'_charset', font.Charset);
    WriteInteger(name+'_color', font.Color);
    WriteInteger(name+'_height', font.Height);
    WriteString(name, font.Name);
    WriteInteger(name+'_pitch', Ord(font.Pitch));
    WriteInteger(name+'_size', font.Size);
    fstyle := font.Style;
    istyle := 0;
    Move(fstyle, istyle, SizeOf(TFontStyles));
    WriteInteger(name+'_style', istyle);
  end; { TDSiRegistry.WriteFont }

  {:Writes TStrings into a MULTI_SZ value.
    @author  Colin Wilson, borland.public.delphi.vcl.components.using 
    @since   2003-10-02
  }        
  procedure TDSiRegistry.WriteStrings(const name: string; strings: TStrings);
  var
    buffer: PChar;
    i     : integer;
    p     : PChar;
    size  : DWORD;
  begin
    size := 0;
    for i := 0 to strings.Count - 1 do
      Inc(size, Length(strings[i]) + 1);
    Inc (size);
    GetMem (buffer, size);
    try
      p := buffer;
      for i := 0 to strings.count - 1 do begin
        LStrCpy(p, PChar(strings[i]));
        Inc(p, LStrLen(p) + 1);
      end;
      p^ := #0;
      SetLastError(RegSetValueEx(CurrentKey, PChar(name), 0, REG_MULTI_SZ,
        buffer, size));
      if GetLastError <> ERROR_SUCCESS then
        raise ERegistryException.CreateFmt('Unable to write MULTI_SZ value. %s',
          [SysErrorMessage(GetLastError)]);
    finally FreeMem(buffer); end
  end; { TDSiRegistry.WriteStrings }

  {:Creates a key in the registry.
    @author  gabr
    @since   2002-11-25
  }
  function DSiCreateRegistryKey(const registryKey: string; root: HKEY): boolean;
  begin
    Result := false;
    with TRegistry.Create do try
      RootKey := root;
      if OpenKey(registryKey, true) then begin
        CloseKey;
        Result := true;
      end;
    finally {TRegistry.}Free; end;
  end; { DSiCreateRegistryKey }

  {:Deletes a value from the registry.
    @author  gabr
    @since   2009-11-13
  }
  function DSiDeleteRegistryValue(const registryKey, name: string; root: HKEY =
    HKEY_CURRENT_USER; access: longword = KEY_SET_VALUE): boolean;
  begin
    Result := false;
    try
      with TDSiRegistry.Create(access) do try
        RootKey := root;
        if OpenKey(registryKey, true) then try
          Result := DeleteValue(name);
        finally CloseKey; end;
      finally {TDSiRegistry.}Free; end;
    except end;
  end; { DSiDeleteRegistryValue }

  {:Deletes a key with all subkeys from the registry.
    @author  gabr
    @since   2002-11-25
  }
  function DSiKillRegistry(const registryKey: string; root: HKEY;
    access: longword): boolean;
  begin
    with TRegistry.Create(access) do try
      RootKey := root;
      Result := DeleteKey(registryKey);
    finally {TRegistry.}  Free; end;
  end; { DSiKillRegistry }

  {:Reads 64-bit integer from the registry, returning default value if the
    specified name doesn't exist in the registry.
    @author  gabr
    @since   2002-11-25
  }
  function DSiReadRegistry(const registryKey, name: string; defaultValue: int64;
    root: HKEY; access: longword): int64; 
  begin
    Result := defaultValue;
    try
      with TDSiRegistry.Create(access) do try
        RootKey := root;
        if OpenKeyReadOnly(registryKey) then try
          Result := ReadInt64(name, defaultValue);
        finally CloseKey; end;
      finally {TDSiRegistry.}Free; end;
    except end;
  end; { DSiReadRegistry }

  {:Reads variant (string, integer, boolean, or date-time) from the registry,
    returning default value if the specified name doesn't exist in the registry.
    @author  gabr
    @since   2002-11-25
  }
  function DSiReadRegistry(const registryKey, name: string; defaultValue: Variant;
    root: HKEY; access: longword): Variant;
  begin
    Result := defaultValue;
    try
      with TDSiRegistry.Create(access) do try
        RootKey := root;
        if OpenKeyReadOnly(registryKey) then try
          Result := ReadVariant(name, defaultValue);
        finally CloseKey; end;
      finally {TDSiRegistry.}Free; end;
    except end;
  end; { DSiReadRegistry }

  {:Checks whether the specified registry key exists.
    @author  gabr
    @since   2002-11-25
  }        
  function DSiRegistryKeyExists(const registryKey: string; root: HKEY;
    access: longword): boolean;
  begin
    with TRegistry.Create(access) do try
      RootKey := root;
      Result := KeyExists(registryKey);
    finally {TRegistry.}Free; end;
  end; { DSiRegistryKeyExists }

  {:Checks whether the specified registry value exists.
    @author  gabr
    @since   2006-02-06
  }
  function DSiRegistryValueExists(const registryKey, name: string; root: HKEY;
    access: longword): boolean;
  begin
    Result := false;
    with TRegistry.Create(access) do try
      RootKey := root;
      if OpenKeyReadOnly(registryKey) then try
        Result := ValueExists(name);
      finally CloseKey; end;
    finally {TRegistry.}Free; end;
  end; { DSiRegistryKeyExists }

  {:Writes 64-bit integer into the registry.
    @author  gabr
    @since   2002-11-25
  }        
  function DSiWriteRegistry(const registryKey, name: string; value: int64;
    root: HKEY; access: longword): boolean; 
  begin
    Result := false;
    try
      with TDSiRegistry.Create(access) do try
        RootKey := root;
        if OpenKey(registryKey, true) then try
          WriteInt64(name, value);
          Result := true;
        finally CloseKey; end;
      finally {TDSiRegistry.}Free; end;
    except end;
  end; { DSiWriteRegistry }

  {:Writes variant (string, integer, boolean, or date-time) into the registry.
    @author  gabr
    @since   2002-11-25
  }        
  function DSiWriteRegistry(const registryKey, name: string; value: Variant;
    root: HKEY; access: longword): boolean; 
  begin
    Result := false;
    try
      with TDSiRegistry.Create(access) do try
        RootKey := root;
        if OpenKey(registryKey, true) then try
          WriteVariant(name, value);
          Result := true;
        finally CloseKey; end;
      finally {TDSiRegistry.}Free; end;
    except end;
  end; { DSiWriteRegistry }

{ Files }

  {:Checks if application can write to a folder.
    @author  gabr
    @since   2005-12-08
  }
  function DSiCanWriteToFolder(const folderName: string): boolean;
  var
    tempFile: string;
  begin
    Result := false;
    if DirectoryExists(folderName) then begin
      tempFile := DSiGetTempFileName('dsi', folderName);
      if tempFile <> '' then begin
        DSiKillFile(tempFile);
        Result := true;
      end;
    end;
  end; { DSiCanWriteToFolder }

  {:Internal. Sets specified compression flag.
    @since   2006-08-14
  }
  function DSiSetCompression(fileHandle: THandle; compressionFormat: integer): boolean;
  var
    comp: SHORT;
    res : DWORD;
  begin
    if Win32Platform <> VER_PLATFORM_WIN32_NT then //only NT can compress files
      Result := true
    else begin
      res := 0;
      comp := compressionFormat;
      Result := DeviceIoControl(fileHandle, FSCTL_SET_COMPRESSION, @comp, SizeOf(SHORT),
        nil, 0, res, nil);
    end;
  end; { DSiSetCompression }

  {:Compresses file on NTFS filesystem.
    @author  gabr
    @since   2006-08-14
  }
  function DSiCompressFile(fileHandle: THandle): boolean;
  begin
    Result := DSiSetCompression(fileHandle, COMPRESSION_FORMAT_DEFAULT);
  end; { DSiCompressFile }

  {:Connects to a network resource and optionally maps drive letter to it.
    @author  gabr
    @since   2008-05-05
  }
  function DSiConnectToNetworkResource(const networkResource: string; const mappedLetter:
    string; const username: string; const password: string): boolean;
  var
    driveName  : string;
    isAvailable: boolean;
    netResource: TNetResource;
    remoteName : string;
  begin
    Result := false;
    if mappedLetter <> '' then begin
      if DSiGetNetworkResource(mappedLetter[1], remoteName, isAvailable) and (remoteName <> '') then
        if AnsiSameText(remoteName, networkResource) then
          Result := true
        else
          DSiDisconnectFromNetworkResource(mappedLetter[1]);
    end;
    if not Result then begin
      FillChar(netResource, SizeOf (netResource), 0);
      netResource.dwScope := RESOURCE_GLOBALNET;
      netResource.dwType := RESOURCETYPE_DISK;
      netResource.dwDisplayType := RESOURCEDISPLAYTYPE_SHARE;
      netResource.dwUsage := RESOURCEUSAGE_CONNECTABLE;
      if mappedLetter <> '' then begin
        driveName := mappedLetter + ':'#0;
        netResource.lpLocalName := PChar(@driveName[1]);
      end;
      netResource.lpRemoteName := PChar(networkResource);
      Result := (WNetAddConnection2(netResource, PChar(password), PChar(username), 0) = NO_ERROR);
    end;
  end; { DSiConnectToNetworkResource }

  {:Copies a file via ShFileOperation (with animated icon etc).
    @author  gabr
    @since   2008-05-30
  }
  function DSiCopyFileAnimated(ownerWindowHandle: THandle; sourceFile, destinationFile:
    string; var aborted: boolean; flags: TShFileOpFlags): boolean;
  var
    fileOp: TSHFileOpStruct;
    flag  : TShFileOpFlag;
  begin
    FillChar(fileOp, SizeOf(fileOp), 0);
    fileOp.Wnd := ownerWindowHandle;
    fileOp.wFunc := FO_COPY;
    sourceFile := sourceFile + #0#0;
    fileOp.pFrom := PChar(sourceFile);
    destinationFile := destinationFile + #0#0;
    fileOp.pTo := PChar(destinationFile);
    fileOp.fFlags := 0;
    for flag := Low(TShFileOpFlag) to High(TShFileOpFlag) do
      if flag in flags then
        fileOp.fFlags := fileOp.fFlags OR CShFileOpFlagMappings[flag];
    Result := (SHFileOperation(fileOp) = 0);
    aborted := fileOp.fAnyOperationsAborted;
  end; { DSiCopyFileAnimated }

  {:Creates folder with the unique name under the temporary folder and returns its name.
    @author  Miha-R
    @since   2002-11-25
  }
  function DSiCreateTempFolder: string;
  var
    GUID: TGUID;
  begin
    OleCheck(CoCreateGUID(GUID));
    Result := DSiGetTempPath + GUIDToString(GUID);
    ForceDirectories(Result);
  end; { DSiCreateTempFolder }

  {:Deletes files matching file mask.
    @author  gabr
    @since   2002-12-19
  }
  procedure DSiDeleteFiles(const folder, fileMask: string);
  var
    err     : integer;
    folderBk: string;
    S       : TSearchRec;
  begin                
    folderBk := IncludeTrailingBackslash(folder);
    err := FindFirst(folderBk+fileMask, 0, S);
    if err = 0 then begin
      repeat
        DSiKillFile(folderBk+S.Name);
        err := FindNext(S);
      until err <> 0;
      FindClose(S);
    end;
  end; { DSiDeleteFiles }

  {gp}
  function DSiDeleteOnReboot(const fileName: string): boolean;
  begin
    Result := DSiMoveOnReboot(fileName, '');
  end; { DSiDeleteOnReboot }

  {gp}
  procedure DSiDeleteTree(const folder: string; removeSubdirsOnly: boolean);

    procedure DeleteTree(const folder: string; depth: integer; delete0: boolean);
    var
      err: integer;
      s  : TSearchRec;
    begin
      err := FindFirst(IncludeTrailingBackslash(folder)+'*.*',faDirectory,S);
      if err = 0 then begin
        repeat
          if (S.Attr and faDirectory) <> 0 then
            if (S.Name <> '.') and (S.Name <> '..') then
              DeleteTree(IncludeTrailingBackslash(folder)+S.Name, depth+1, delete0);
          err := FindNext(S);
        until err <> 0;
        FindClose(S);
      end;
      if (depth > 0) or delete0 then
        DSiRemoveFolder(folder);
    end; { DeleteTree }
    
  begin { DSiDeleteTree }
    DeleteTree(folder, 0, not removeSubdirsOnly);
  end; { DSiDeleteTree }

  {gp}
  function DSiDeleteWithBatch(const fileName: string; rmDir: boolean): boolean;
  // Idea stollen from the article by Jeffrey Richter, first published in
  // Microsoft Systems Journal, reprinted in Microsoft Developer Network.
  // Simple but effective solution: create batch file that deletes exe and then
  // deletes itself, then run it as an invisible console app with low priority.
  var
    bat    : text;
    pi     : TProcessInformation;
    si     : TStartupInfo;
    tmpFile: string;
    winTemp: string;
  begin
    Result := false;
    try
      winTemp := DSiGetTempFileName('wt');
      if winTemp <> '' then begin
        tmpFile := ChangeFileExt(winTemp, '.bat');
        MoveFile(PChar(winTemp), PChar(tmpFile));
        Assign(bat, tmpFile);
        Rewrite(bat);
        Writeln(bat,':repeat');
        Writeln(bat, 'del "', fileName, '"');
        Writeln(bat, 'if exist "', fileName, '" goto repeat');
        if rmDir then
          Writeln(bat,'rmdir "', ExtractFilePath(fileName), '"');
        Writeln(bat,'del "', tmpFile, '"');
        Close(bat);
        FillChar(si, SizeOf(si), 0);
        si.cb := SizeOf(si);
        si.dwFlags := STARTF_USESHOWWINDOW;
        si.wShowWindow := SW_HIDE;
        {$IFDEF Unicode}UniqueString(tmpFile);{$ENDIF Unicode}
        if (CreateProcess(nil, PChar(tmpFile), nil, nil, false,
          CREATE_SUSPENDED or IDLE_PRIORITY_CLASS, nil,
          PChar(ExtractFilePath(tmpFile)), si, pi)) then
        begin
          SetThreadPriority(pi.hThread, THREAD_PRIORITY_IDLE);
          CloseHandle(pi.hProcess);
          ResumeThread(pi.hThread);
          CloseHandle(pi.hThread);
          Result := true;
        end;
      end;
    except end;
  end; { DSiDeleteWithBatch }

  {:Wide version of SysUtils.DirectoryExists.
    @author  gabr
    @since   2006-08-14
  }
  function  DSiDirectoryExistsW(const directory: WideString): boolean;
  var
    code: integer;
  begin
    code := GetFileAttributesW(PWideChar(directory));
    Result := (code <> -1) and ((FILE_ATTRIBUTE_DIRECTORY AND code) <> 0);
  end; { DSiDirectoryExistsW }

  {:Try to disable WOW64 file system redirection if running on 64-bit Windows.
    Always succeeds on 32-bit windows.
    @author  gabr
    @since   2009-03-17
  }
  function DSiDisableWow64FsRedirection(var oldStatus: pointer): boolean;
  begin
    if DSiIsWow64 then
      Result := DSiWow64DisableWow64FsRedirection(oldStatus)
    else
      Result := true;
  end; { DSiDisableWow64FsRedirection }

  {:Disconnects mapped letter.
    @author  gabr
    @since   2009-01-28
  }
  function DSiDisconnectFromNetworkResource(mappedLetter: char;
    updateProfile: boolean): boolean;
  var
    driveName: string;
    flags    : cardinal;
  begin
    driveName := mappedLetter + ':'#0;
    flags := 0;
    if updateProfile then
      flags := CONNECT_UPDATE_PROFILE;
    Result := (WNetCancelConnection2(PChar(@driveName[1]), flags, true) = NO_ERROR);
  end; { DSiDisconnectFromNetworkResource }

  {gp}
  function DSiEjectMedia(deviceLetter: char): boolean;
  var
    cd : THandle;
    ret: DWORD;
  begin
    Result := false;
    cd := CreateFile(PChar('\\.\'+deviceLetter+':'), GENERIC_READ, 0, nil, OPEN_EXISTING, 0, 0);
    if cd <> INVALID_HANDLE_VALUE then begin
      Result := DeviceIoControl(cd, IOCTL_STORAGE_EJECT_MEDIA, nil, 0, nil, 0, ret, nil);
      CloseHandle(cd);
    end;
  end; { DSiLoadMedia }

  {:Deletes all files in the folder.
    @author  gabr
    @since   2002-12-19
  }
  procedure DSiEmptyFolder(const folder: string);
  begin
    DSiDeleteFiles(folder, '*.*');
  end; { DSiEmptyFolder }

  {:Empties recycle bin.
    @author  ales
    @since   2002-12-19
  }
  function DSiEmptyRecycleBin: boolean;
  begin
    Result := DSiSHEmptyRecycleBin(0, nil,
      SHERB_NOCONFIRMATION OR SHERB_NOPROGRESSUI OR SHERB_NOSOUND) = S_OK;
  end; { DSiEmptyRecycleBin }

  {:Enumerates all files matching given mask and attribute and calls callback method for
    each file.
    @returns Number of files enumerated.
    @author  gabr
    @since   2003-06-17
  }
  function DSiEnumFiles(const fileMask: string; attr: integer;
    enumCallback: TDSiEnumFilesCallback): integer;
  var
    err   : integer;
    folder: string;
    S     : TSearchRec;
  begin
    Result := 0;
    folder := IncludeTrailingBackslash(ExtractFilePath(fileMask));
    err := FindFirst(fileMask, attr, S);
    if err = 0 then try
      repeat
        enumCallback(folder+S.Name);
        Inc(Result);
        err := FindNext(S);
      until err <> 0;
    finally FindClose(S); end;
  end; { DSiEnumFiles }

  procedure _DSiEnumFilesEx(const folder, fileMask: string; attr: integer; enumSubfolders:
    boolean; enumCallback: TDSiEnumFilesExCallback; var totalFiles: integer; var stopEnum:
    boolean; fileList: TStrings; storeFullPath: boolean; currentDepth, maxDepth: integer);
  var
    err: integer;
    s  : TSearchRec;
  begin
    if enumSubfolders and ((maxDepth <= 0) or (currentDepth < maxDepth)) then begin
      err := FindFirst(folder+'*.*',faDirectory,S);
      if err = 0 then try
        repeat
          if (S.Attr and faDirectory) <> 0 then
            if (S.Name <> '.') and (S.Name <> '..') then begin
              if assigned(enumCallback) then begin
                enumCallback(folder, S, true, stopEnum);
                if stopEnum then
                  Exit;
              end;
              if assigned(fileList) then
                if storeFullPath then
                  fileList.Add(folder + S.Name + '\')
                else
                  fileList.Add(S.Name + '\');
              _DSiEnumFilesEx(folder+S.Name+'\', fileMask, attr, enumSubfolders,
                enumCallback, totalFiles, stopEnum, fileList, storeFullPath,
                currentDepth + 1, maxDepth);
            end;
          err := FindNext(S);
        until (err <> 0) or stopEnum;
      finally FindClose(S); end;
    end;
    if stopEnum then
      Exit;
    err := FindFirst(folder+fileMask, attr, S);
    if err = 0 then try
      repeat
        if assigned(enumCallback) then
          enumCallback(folder, S, false, stopEnum);
        if assigned(fileList) then
          if storeFullPath then
            fileList.Add(folder + S.Name)
          else
            fileList.Add(S.Name);
        Inc(totalFiles);
        err := FindNext(S);
      until (err <> 0) or stopEnum;
    finally FindClose(S); end;
  end; { _DSiEnumFilesEx }

  {:Enumerates all files matching given mask and attribute and calls callback
    method for each file. Optionally descends into subfolders.
    @returns Number of files (not folders!) enumerated.
    @author  gabr
    @since   2003-06-17
  }
  function DSiEnumFilesEx(const fileMask: string; attr: integer; enumSubfolders: boolean;
    enumCallback: TDSiEnumFilesExCallback; maxEnumDepth: integer): integer;
  var
    folder  : string;
    mask    : string;
    stopEnum: boolean;
  begin
    mask := fileMask;
    folder := ExtractFilePath(mask);
    Delete(mask, 1, Length(folder));
    if folder <> '' then
      folder := IncludeTrailingBackslash(folder);
    Result := 0;
    stopEnum := false;
    _DSiEnumFilesEx(folder, mask, attr, enumSubfolders, enumCallback, Result, stopEnum,
      nil, false, 1, maxEnumDepth);
  end; { DSiEnumFilesEx }

  {:Enumerates files (optionally in subfolders) and stores results into caller-provided
    TStrings object.
    @since   2006-05-14
  }
  procedure DSiEnumFilesToSL(const fileMask: string; attr: integer; fileList: TStrings;
    storeFullPath: boolean; enumSubfolders: boolean; maxEnumDepth: integer);
  var
    folder    : string;
    mask      : string;
    stopEnum  : boolean;
    totalFiles: integer;
  begin
    fileList.Clear;
    mask := fileMask;
    folder := ExtractFilePath(mask);
    Delete(mask, 1, Length(folder));
    if folder <> '' then
      folder := IncludeTrailingBackslash(folder);
    stopEnum := false;
    _DSiEnumFilesEx(folder, mask, attr, enumSubfolders, nil, totalFiles, stopEnum,
      fileList, storeFullPath, 1, maxEnumDepth);
  end; { DSiEnumFilesToSL }

  {:Wide version of SysUtils.FileExists.
    @author  gabr
    @since   2006-08-14
  }
  function DSiFileExistsW(const fileName: WideString): boolean;
  var
    code: integer;
  begin
    code := GetFileAttributesW(PWideChar(fileName));
    Result := (code <> -1) and ((FILE_ATTRIBUTE_DIRECTORY AND code) = 0);
  end; { DSiFileExistsW }

  {:Checks if fileName extension is equal to the extension parameter.
    Extension can be provided with or without leading period.
    @author  gabr
    @since   2007-06-08
  }
  function DSiFileExtensionIs(const fileName, extension: string): boolean; overload;
  var
    fExt: string;
  begin
    fExt := ExtractFileExt(fileName);
    if (Length(extension) = 0) or (extension[1] <> '.') and (fExt <> '') and (fExt[1] = '.') then
      Delete(fExt, 1, 1);
    Result := SameText(fExt, extension);
  end; { DSiFileExtensionIs }

  {:Checks if fileName extension is equal to any of the extensions listed in the extension
    array.
    @author  gabr
    @since   2007-06-08
  }
  function DSiFileExtensionIs(const fileName: string; extension: array of string):
    boolean; overload;
  var
    fExtDot  : string;
    fExtNoDot: string;
    iExt     : integer;
    testExt  : string;
  begin
    Result := true;
    fExtDot := ExtractFileExt(fileName);
    fExtNoDot := fExtDot;
    if (fExtDot = '') or (fExtDot[1] <> '.') then
       fExtDot := '.' + fExtDot;
    if (fExtNoDot <> '') and (fExtNoDot[1] = '.') then
      Delete(fExtNoDot, 1, 1);
    for iExt := Low(extension) to High(extension) do begin
      testExt := extension[iExt];
      if (Length(testExt) = 0) or (testExt[1] <> '.') then begin
        if SameText(fExtNoDot, testExt) then
          Exit
        else if SameText(fExtDot, testExt) then
          Exit;
      end;
    end;
    Result := false;
  end; { DSiFileExtensionIs }
  
  {:Retrieves file size.
    @returns -1 for unexisting/unaccessible file or file size.
    @author  gabr
    @since   2003-06-17
  }
  function DSiFileSize(const fileName: string): int64;
  var
    fHandle: DWORD;
  begin
    fHandle := CreateFile(PChar(fileName), 0, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if fHandle = INVALID_HANDLE_VALUE then
      Result := -1
    else try
      Int64Rec(Result).Lo := GetFileSize(fHandle, @Int64Rec(Result).Hi);
    finally CloseHandle(fHandle); end;
  end; { DSiFileSize }

  {:Wide version of DSiFileSize.
    @returns -1 for unexisting/unaccessible file or file size.
    @author  gabr
    @since   2006-08-14
  }
  function DSiFileSizeW(const fileName: WideString): int64;
  var
    fHandle: DWORD;
  begin
    fHandle := CreateFileW(PWideChar(fileName), 0, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if fHandle = INVALID_HANDLE_VALUE then
      Result := -1
    else try
      Int64Rec(Result).Lo := GetFileSize(fHandle, @Int64Rec(Result).Hi);
    finally CloseHandle(fHandle); end;
  end; { DSiFileSizeW }

  {:Calculates size of all files in a specified folder.
    @author  radicalb, gabr
    @since   2007-05-30
  }
  function DSiGetFolderSize(const folder: string; includeSubfolders: boolean): int64;
  var
    err     : integer;
    folderBk: string;
    rec     : TSearchRec;
  begin
    Result := 0;
    folderBk := IncludeTrailingBackslash(folder);
    err := FindFirst(folderBk + '*.*', faAnyFile, rec);
    if err = 0 then try
      repeat
        Inc(Result, rec.Size);
        if includeSubfolders and ((rec.Attr and faDirectory) > 0) and
           (rec.Name <> '.') and (rec.Name <> '..')
        then
          Inc(Result, DSiGetFolderSize(folderBk + rec.Name, true));
        err := FindNext(rec);
      until (err <> 0);
    finally FindClose(rec); end;
  end; { DSiGetFolderSize }
  
  {:Returns one of the file times - creation time, last access time, last write time.
    Returns 0 if file cannot be accessed.
    @author  Lee_Nover
    @since   2006-03-01
  }
  function DSiGetFileTime(const fileName: string; whatTime: TDSiFileTime): TDateTime;
  var
    creationTime        : TDateTime;
    lastAccessTime      : TDateTime;
    lastModificationTime: TDateTime;
  begin
    if not DSiGetFileTimes(fileName, creationTime, lastAccessTime, lastModificationTime) then
      Result := 0
    else case whatTime of
      ftCreation:         Result := creationTime;
      ftLastAccess:       Result := lastAccessTime;
      ftLastModification: Result := lastModificationTime;
      else raise Exception.Create('DSiGetFileTime: Invalid time selector');
    end;
  end; { DSiGetFileTime }

  {:Returns file creation, last access and last write time.
    @author  gabr
    @since   2006-12-20
  }
  function  DSiGetFileTimes(const fileName: string; var creationTime, lastAccessTime,
    lastModificationTime: TDateTime): boolean; 
  var
    fileHandle            : cardinal;
    fsCreationTime        : TFileTime;
    fsLastAccessTime      : TFileTime;
    fsLastModificationTime: TFileTime;
  begin
    Result := false;
    fileHandle := CreateFile(PChar(fileName), GENERIC_READ, FILE_SHARE_READ, nil,
      OPEN_EXISTING, 0, 0);
    if fileHandle <> INVALID_HANDLE_VALUE then try
      Result :=
        GetFileTime(fileHandle, @fsCreationTime, @fsLastAccessTime,
           @fsLastModificationTime) and
        DSiFileTimeToDateTime(fsCreationTime, creationTime) and
        DSiFileTimeToDateTime(fsLastAccessTime, lastAccessTime) and
        DSiFileTimeToDateTime(fsLastModificationTime, lastModificationTime);
    finally CloseHandle(fileHandle); end;
  end; { DSiGetFileTimes }

  {:Returns the long pathname representation.
    @author  Lee_Nover
    @since   2006-03-01
  }
  function DSiGetLongPathName(const fileName: string): string;
  begin
    if not assigned(GGetLongPathName) then
      GGetLongPathName := DSiGetProcAddress('kernel32.dll', 'GetLongPathName' + CAPISuffix);
    if assigned(GGetLongPathName) then begin
      SetLength(Result, MAX_PATH);
      SetLength(Result, GGetLongPathName(PChar(fileName), PChar(Result), Length(Result)));
    end
    else begin
      Result := '';
      SetLastError(ERROR_NOT_SUPPORTED);
    end;
  end; { DSiGetLongPathName }

  {:Returns information on mapped network resource.
    @author  gabr
    @since   2009-01-28
  }
  function DSiGetNetworkResource(mappedLetter: char; var networkResource: string; var
    connectionIsAvailable: boolean): boolean;
  var
    bufferSize: cardinal;
    driveName : string;
    remoteName: PChar;
    wnetResult: integer;
  begin
    connectionIsAvailable := false;
    networkResource := '';
    driveName := mappedLetter + ':';
    wnetResult := GetDriveType(PChar(driveName + '\'));
    if (wnetResult <> DRIVE_REMOTE) and (wnetResult <> DRIVE_NO_ROOT_DIR) then
      Result := true
    else begin
      bufferSize := MAX_PATH * SizeOf(char);
      GetMem(remoteName, bufferSize + SizeOf(char));
      try
        wnetResult := WNetGetConnection(PChar(driveName), remoteName, bufferSize);
        if wnetResult = ERROR_MORE_DATA then begin
          FreeMem(remoteName);
          GetMem(remoteName, bufferSize);
          wnetResult := WNetGetConnection(PChar(driveName), remoteName, bufferSize);
        end;
        Result := (wnetResult = NO_ERROR) or (wnetResult = ERROR_CONNECTION_UNAVAIL);
        if Result then begin
          connectionIsAvailable := (wnetResult = NO_ERROR);
          networkResource := Trim(StrPas(PChar(remoteName)));
        end;
      finally FreeMem(remoteName); end;
    end;
  end; { DSiGetNetworkResource }

  {:Converts SUBSTed drive letter into true path.
    Returns empty string if letter does not belog to a SUBSTed drive.
    @author  gabr
    @since   2009-10-20
  }
  function DSiGetSubstDrive(mappedLetter: char): string;
  var
    buffer    : PChar;
    bufferSize: integer;
    device    : string;
  begin
    Result := '';
    device := mappedLetter + ':';
    bufferSize := 256;
    GetMem(buffer, bufferSize * SizeOf(char));
    try
      repeat
        if QueryDosDevice(PChar(device), buffer, bufferSize-1) = 0 then begin
          if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
            break //repeat
          else begin
            FreeMem(buffer);
            bufferSize := 2 * bufferSize;
            GetMem(buffer, bufferSize * SizeOf(char));
          end;
        end
        else begin
          device := string(buffer);
          if Copy(device, 1, 4) = '\??\' then begin
            Result := device;
            Delete(Result, 1, 4);
          end;
          break; //repeat
        end;
      until false;
    finally FreeMem(buffer); end;
  end; { DSiGetSubstDrive }

  {:Converts SUBSTed path into true path.
    Returns original path if it does not lie on a SUBSTed drive.
    @author  gabr
    @since   2009-10-20
  }
  function DSiGetSubstPath(const path: string): string;
  var
    origPath: string;
  begin
    Result := path;
    if Length(Result) < 2 then
      Exit;
    if path[2] = ':' then begin
      origPath := DSiGetSubstDrive(path[1]);
      if origPath <> '' then begin
        Delete(Result, 1, 2);
        Result := origPath + Result;
      end;
    end;
  end; { DSiGetSubstPath }

  {:Returns temporary file name, either in the specified path or in the default temp path
    (if 'tempPath' is empty).
    @author  Miha-R
    @since   2002-11-25
  }
  function DSiGetTempFileName(const prefix, tempPath: string): string;
  var
    tempFileName: PChar;
    usePrefix   : string;
    useTempPath : string;
  begin
    Result := '';
    GetMem(tempFileName, MAX_PATH * SizeOf(char));
    try
      if tempPath = '' then
        useTempPath := DSiGetTempPath
      else begin
        useTempPath := tempPath;
        UniqueString(useTempPath);
      end;
      usePrefix := prefix;
      UniqueString(usePrefix);
      if GetTempFileName(PChar(useTempPath), PChar(usePrefix), 0, tempFileName) <> 0 then
        Result := StrPas(tempFileName)
      else
        Result := '';
    finally FreeMem(tempFileName); end;
  end; { DSiGetTempFileName }

  {:Returns path designated for temporary files.
    @author  Miha-R
    @since   2002-11-25
  }
  function DSiGetTempPath: string;
  var
    bufSize : DWORD;
    tempPath: PChar;
  begin
    bufSize := GetTempPath(0, nil);
    GetMem(tempPath, bufSize * SizeOf(char));
    try
      GetTempPath(bufSize, tempPath);
      Result := StrPas(tempPath);
    finally FreeMem(tempPath); end;
  end; { DSiGetTempPath }

  {:Returns unique file name with the specified extension.
    @author  Miha-R
    @since   2002-11-25
  }
  function DSiGetUniqueFileName(const extension: string): string;
  var
    GUID: TGUID;
  begin
    OleCheck(CoCreateGUID(GUID));
    Result := Copy(GUIDToString(GUID), 2, 36) + Extension;
  end; { DSiGetUniqueFileName }

  {:Checks if NTFS file is compressed.
    @author  gabr
    @since   2006-08-14
  }
  function  DSiIsFileCompressed(const fileName: string): boolean;
  begin
    if Win32Platform <> VER_PLATFORM_WIN32_NT then //only NT can compress files
      Result := false
    else
      Result := (GetFileAttributes(PChar(fileName)) AND FILE_ATTRIBUTE_COMPRESSED) =
        FILE_ATTRIBUTE_COMPRESSED;
  end; { DSiIsFileCompressed }

  {:Wide version of DSiIsFileCompressed.
    @author  gabr
    @since   2006-08-14
  }
  function  DSiIsFileCompressedW(const fileName: WideString): boolean;
  begin
    if Win32Platform <> VER_PLATFORM_WIN32_NT then //only NT can compress files
      Result := false
    else
      Result := (GetFileAttributesW(PWideChar(fileName)) AND FILE_ATTRIBUTE_COMPRESSED) =
        FILE_ATTRIBUTE_COMPRESSED;
  end; { DSiIsFileCompressedW }

  {:Deletes file, even if it is readonly.
    @author  gabr
    @since   2002-11-25
  }
  function DSiKillFile(const fileName: string): boolean;
  var
    oldAttr: DWORD;
  begin
    if not FileExists(fileName) then
      Result := true
    else begin
      oldAttr := GetFileAttributes(PChar(fileName));
      SetFileAttributes(PChar(fileName), 0);
      Result := DeleteFile(fileName);
      if not Result then
        SetFileAttributes(PChar(fileName), oldAttr);
    end;
  end; { DSiKillFile }

  {gp}
  function DSiLoadMedia(deviceLetter: char): boolean;
  var
    cd : THandle;
    ret: DWORD;
  begin
    Result := false;
    cd := CreateFile(PChar('\\.\'+deviceLetter+':'), GENERIC_READ, 0, nil, OPEN_EXISTING, 0, 0);
    if cd <> INVALID_HANDLE_VALUE then begin
      Result := DeviceIoControl(cd, IOCTL_STORAGE_LOAD_MEDIA, nil, 0, nil, 0, ret, nil);
      CloseHandle(cd);
    end;
  end; { DSiLoadMedia }

  ///<summary>Moves a file to another file, even if target exists.</summary>
  ///<author>gabr</author>
  ///<since>2007-07-24</since>
  function DSiMoveFile(const srcName, destName: string; overwrite: boolean): boolean;
  var
    flags: DWORD;
  begin
    Result := false;
    if DSiIsWinNT then begin
      flags := MOVEFILE_COPY_ALLOWED;
      if overwrite then
        flags := flags OR MOVEFILE_REPLACE_EXISTING;
      Result := MoveFileEx(PChar(srcName), PChar(destName), flags);
    end
    else begin
      if overwrite and FileExists(destName) then
        if not DeleteFile(destName) then
          Exit;
      Result := MoveFile(PChar(srcName), PChar(destName));
    end;
  end; { DSiMoveFile }

  {gp}
  function DSiMoveOnReboot(const srcName, destName: string): boolean;
  var
    wfile: string;
    winit: text;
    wline: string;
    cont : TStringList;
    i    : integer;
    found: boolean;
    dest : PChar;
  begin
    if destName = '' then
      dest := nil
    else
      dest := PChar(destName);
    if DSiIsWinNT then
      Result := MoveFileEx(PChar(srcName), dest, MOVEFILE_DELAY_UNTIL_REBOOT)
    else
      Result := false;
    if not Result then begin
      // not NT, write a Rename entry to WININIT.INI
      wfile := DSiGetWindowsFolder+'\wininit.ini';
      if FileOpenSafe(wfile,winit,500,120{one minute}) then begin
        try
          cont := TStringList.Create;
          try
            Reset(winit);
            while not Eof(winit) do begin
              Readln(winit,wline);
              cont.Add(wline);
            end; //while
            if destName = '' then
              wline := 'NUL='+srcName
            else
              wline := destName+'='+srcName;
            found := false;
            for i := 0 to cont.Count - 1 do begin
              if UpperCase(cont[i]) = '[RENAME]' then begin
                cont.Insert(i+1,wline);
                found := true;
                break;
              end;
            end; //for
            if not found then begin
              cont.Add('[Rename]');
              cont.Add(wline);
            end;
            Rewrite(winit);
            for i := 0 to cont.Count - 1 do
              Writeln(winit,cont[i]);
            Result := true;
          finally cont.Free; end;
        finally Close(winit); end;
      end;
    end;
  end; { DSiMoveOnReboot }

  {:Deletes all files and folders in the specified folder (recursively), then deletes the
    folder.
    @author  gabr
    @since   2002-12-19
  }
  procedure DSiRemoveFolder(const folder: string);
  begin
    DSiEmptyFolder(folder);
    if DirectoryExists(folder) then
      RemoveDir(folder);
  end; { DSiRemoveFolder }

  {:Try to revert WOW64 file system redirection status if running on 64-bit Windows.
    Always succeeds on 32-bit windows.
    @author  gabr
    @since   2009-03-17
  }
  function DSiRevertWow64FsRedirection(const oldStatus: pointer): boolean;
  begin
    if DSiIsWow64 then
      Result := DSiWow64RevertWow64FsRedirection(oldStatus)
    else
      Result := true;
  end; { DSiRevertWow64FsRedirection }

  {:Sets one of the file times - creation time, last access time, last write time.
    @author  gabr
    @since   2012-02-06
  }
  function DSiSetFileTime(const fileName: string; dateTime: TDateTime;
    whatTime: TDSiFileTime): boolean;
  var
    creationTime        : TDateTime;
    lastAccessTime      : TDateTime;
    lastModificationTime: TDateTime;
  begin
    Result := DSiGetFileTimes(fileName, creationTime, lastAccessTime, lastModificationTime);
    if not Result then
      Exit;
    case whatTime of
      ftCreation:         creationTime := dateTime;
      ftLastAccess:       lastAccessTime := dateTime;
      ftLastModification: lastModificationTime := dateTime;
      else raise Exception.CreateFmt('DSiSetFileTime: Unexpected whatTime: %d', [Ord(whatTime)]);
    end;
    Result := DSiSetFileTimes(fileName, creationTime, lastAccessTime, lastModificationTime);
  end; { DSiSetFileTime }

  {:Setsfile creation, last access and last write time.
    @author  gabr
    @since   2012-02-06
  }
  function  DSiSetFileTimes(const fileName: string; creationTime, lastAccessTime,
    lastModificationTime: TDateTime): boolean;
  var
    fileHandle            : cardinal;
    fsCreationTime        : TFileTime;
    fsLastAccessTime      : TFileTime;
    fsLastModificationTime: TFileTime;
  begin
    Result := false;
    fileHandle := CreateFile(PChar(fileName), GENERIC_READ + GENERIC_WRITE, 0, nil,
      OPEN_EXISTING, 0, 0);
    if fileHandle <> INVALID_HANDLE_VALUE then try
      Result :=
        DSiDateTimeToFileTime(creationTime, fsCreationTime) and
        DSiDateTimeToFileTime(lastAccessTime, fsLastAccessTime) and
        DSiDateTimeToFileTime(lastModificationTime, fsLastModificationTime) and
        SetFileTime(fileHandle, @fsCreationTime, @fsLastAccessTime,
           @fsLastModificationTime);
    finally CloseHandle(fileHandle); end;
  end; { DSiSetFileTimes }

  {ales}
  function DSiShareFolder(const folder, shareName, comment: string): boolean;
  var
    ntComment   : WideString;
    ntFolder    : WideString;
    ntShareName : WideString;
    paramError  : integer;
    shareInfo9x : SHARE_INFO_50_9x;
    shareInfoNT : SHARE_INFO_2_NT;
    w9xShareName: string;
  begin
    if folder = '' then
      raise Exception.Create('DSiShareFolder: empty folder');
    if shareName = '' then
      raise Exception.Create('DSiShareFolder: empty share name');
    if DSiIsWinNT then begin
      ntFolder := folder;
      ntShareName := shareName;
      ntComment := comment;
      with ShareInfoNT do begin
        shi2_NetName := PWideChar(ntShareName);
        shi2_Type := STYPE_DISKTREE;
        shi2_Remark := PWideChar(ntComment);
        shi2_Permissions := 0;
        shi2_Max_Uses := -1;
        shi2_Current_Uses := 0;
        shi2_Path := PWideChar(ntFolder);
        shi2_Passwd := nil;
      end;
      ParamError := 0;
      Result := (DSiNTNetShareAdd(nil, 2, @ShareInfoNT, paramError) = 0);
    end
    else begin
      with ShareInfo9x do begin
        FillChar(shi50_NetName, 13, 0);
        w9xShareName := Copy(shareName, 1, 13);
        Move(w9xShareName[1], shi50_NetName[1], Length(w9xShareName));
        shi50_Type := STYPE_DISKTREE;
        shi50_Remark := PChar(comment);
        shi50_rw_password[1] := #0;
        shi50_ro_password[1] := #0;
        shi50_flags := SHI50F_FULL;
        shi50_Path := PChar(ANSIUpperCase(folder));
      end;
      Result := (DSi9xNetShareAdd(nil, 50, @ShareInfo9x, SizeOf(SHARE_INFO_50_9x)) = 0);
    end;
  end; { DSiShareFolder }

  {:Uncompresses file on NTFS filesystem.
    @author  gabr
    @since   2006-08-14
  }
  function  DSiUncompressFile(fileHandle: THandle): boolean;
  begin
    Result := DSiSetCompression(fileHandle, COMPRESSION_FORMAT_NONE);
  end; { DSiUncompressFile }

  {ales}
  function DSiUnShareFolder(const shareName: string): boolean;
  var
    ntShareName: WideString;
  begin
    if DSiIsWinNT then begin
      ntShareName := shareName;
      Result := (DSiNTNetShareDel(nil, PWideChar(ntShareName), 0) = 0);
    end
    else
      Result := (DSi9xNetShareDel(nil, PChar(shareName), 0) = 0);
  end; { DSiUnShareFolder }

{ Processes }

  const
    DESKTOP_ALL = DESKTOP_READOBJECTS or DESKTOP_CREATEWINDOW or DESKTOP_CREATEMENU or DESKTOP_HOOKCONTROL or
      DESKTOP_JOURNALRECORD or DESKTOP_JOURNALPLAYBACK or DESKTOP_ENUMERATE or DESKTOP_WRITEOBJECTS or
      DESKTOP_SWITCHDESKTOP or STANDARD_RIGHTS_REQUIRED;

    WINSTA_ALL = WINSTA_ENUMDESKTOPS or WINSTA_READATTRIBUTES or WINSTA_ACCESSCLIPBOARD or WINSTA_CREATEDESKTOP or
      WINSTA_WRITEATTRIBUTES or WINSTA_ACCESSGLOBALATOMS or WINSTA_EXITWINDOWS or WINSTA_ENUMERATE or
      WINSTA_READSCREEN or STANDARD_RIGHTS_REQUIRED;

    GENERIC_ACCESS = GENERIC_READ or GENERIC_WRITE or GENERIC_EXECUTE or GENERIC_ALL;

    HEAP_ZERO_MEMORY         = 8;
    ACL_REVISION             = 2;
    ACCESS_ALLOWED_ACE_TYPE  = 0;
    CONTAINER_INHERIT_ACE    = 2;
    INHERIT_ONLY_ACE         = 8;
    OBJECT_INHERIT_ACE       = 1;
    NO_PROPAGATE_INHERIT_ACE = 4;
    SE_GROUP_LOGON_ID        = $C0000000;

  type
    ACL_SIZE_INFORMATION = record
      AceCount: DWORD;
      AclBytesInUse: DWORD;
      AclBytesFree: DWORD;
    end;

    ACE_HEADER = record
      AceType: BYTE;
      AceFlags: BYTE;
      AceSize: WORD;
    end;
    PACE_HEADER = ^ACE_HEADER;

    ACCESS_ALLOWED_ACE = record
      Header: ACE_HEADER;
      Mask: ACCESS_MASK;
      SidStart: DWORD;
    end;
    PACCESS_ALLOWED_ACE = ^ACCESS_ALLOWED_ACE;

  function  DSiAddAceToDesktop(desktop: HDESK; sid: PSID): boolean;
  var
    aclSizeInfo : ACL_SIZE_INFORMATION;
    dacl        : PACL;
    daclExists  : LongBool;
    daclPresent : LongBool;
    iAce        : integer;
    newAcl      : PACL;
    newAclSize  : DWORD;
    newSecDescr : PSECURITY_DESCRIPTOR;
    sdSize      : DWORD;
    sdSizeNeeded: DWORD;
    secDescr    : PSECURITY_DESCRIPTOR;
    secInfo     : SECURITY_INFORMATION;
    skipAdd     : boolean;
    tempAce     : PACE_HEADER;
  begin
    Result := false;
    secDescr := nil;
    newSecDescr := nil;
    newAcl := nil;
    secInfo := DACL_SECURITY_INFORMATION;
    sdSize := 0;
    try
      if not GetUserObjectSecurity(desktop, secInfo, secDescr, sdSize, sdSizeNeeded) then begin
        if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
          Exit;
        secDescr := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, sdSizeNeeded);
        if secDescr = nil then
          Exit;
        newSecDescr := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, sdSizeNeeded);
        if newSecDescr = nil then
          Exit;
        sdSize := sdSizeNeeded;
        if not GetUserObjectSecurity(desktop, secInfo, secDescr, sdSize, sdSizeNeeded) then
          Exit;
      end;
      if not InitializeSecurityDescriptor(newSecDescr, SECURITY_DESCRIPTOR_REVISION) then
        Exit;
      if not GetSecurityDescriptorDacl(secDescr, daclPresent, dacl, daclExists) then
        Exit;
      ZeroMemory(@aclSizeInfo, SizeOf(ACL_SIZE_INFORMATION));
      aclSizeInfo.AclBytesInUse := SizeOf(ACL);
      if assigned(dacl) then begin
        if not GetAclInformation(dacl^, @aclSizeInfo, SizeOf(ACL_SIZE_INFORMATION), AclSizeInformation) then
          Exit;
      end;
      newAclSize := aclSizeInfo.AclBytesInUse + SizeOf(ACCESS_ALLOWED_ACE) +
        GetLengthSid(sid) - SizeOf(DWORD);
      newAcl := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, newAclSize);
      if (newAcl = nil) or (not InitializeAcl(newAcl^, newAclSize, ACL_REVISION)) then
        Exit;
      skipAdd := false;
      if daclPresent then begin
        // Copy the ACEs to the new ACL.
        for iAce := 0 to aclSizeInfo.AceCount - 1 do begin
          if not (GetAce(dacl^, iAce, pointer(tempAce)) and
                  AddAce(newAcl^, ACL_REVISION, MAXDWORD, tempAce, tempAce.AceSize))
          then
            Exit;
          if (tempAce.AceType = ACCESS_ALLOWED_ACE_TYPE) and
             EqualSID(@PACCESS_ALLOWED_ACE(tempAce)^.SidStart, sid)
          then
            skipAdd := true;
        end;
      end;
      if not skipAdd then begin
        if not (AddAccessAllowedAce(newAcl^, ACL_REVISION, DESKTOP_ALL, sid) and
                SetSecurityDescriptorDacl(newSecDescr, true, newAcl, false) and
                SetUserObjectSecurity(desktop, secInfo, newSecDescr))
        then
          Exit;
      end;
      Result := true;
    finally
      if assigned(newAcl) then
        HeapFree(GetProcessHeap, 0, newAcl);
      if assigned(secDescr) then
        HeapFree(GetProcessHeap, 0, secDescr);
      if assigned(newSecDescr) then
        HeapFree(GetProcessHeap, 0, newSecDescr);
    end;
  end; { DSiAddAceToDesktop }

  function DSiAddAceToWindowStation(station: HWINSTA; sid: PSID): boolean;
  var
    ace        : ^ACCESS_ALLOWED_ACE;
    aclSizeInfo: ACL_SIZE_INFORMATION;
    dacl       : PACL;
    daclExists : LongBool;
    daclPresent: LongBool;
    iAce       : integer;
    newAcl     : PACL;
    newAclSize : DWORD;
    newSecDecr : PSECURITY_DESCRIPTOR;
    reqSdSize  : DWORD;
    sdSize     : DWORD;
    secDescr   : PSECURITY_DESCRIPTOR;
    secInfo    : SECURITY_INFORMATION;
    skipAdd    : boolean;
    tempAce    : PACE_HEADER;
  begin
    Result := false;
    secInfo := DACL_SECURITY_INFORMATION;
    ace := nil;
    secDescr := nil;
    sdSize := 0;
    newAcl := nil;
    newSecDecr := nil;
    try
      if not GetUserObjectSecurity(station, secInfo, secDescr, sdSize, reqSdSize) then begin
        if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
          Exit;
        secDescr := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, reqSdSize);
        if secDescr = nil then
          Exit;
        newSecDecr := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, reqSdSize);
        if newSecDecr = nil then
          Exit;
        sdSize := reqSdSize;
        if not GetUserObjectSecurity(station, secInfo, secDescr, sdSize, reqSdSize) then
          Exit;
      end;
      if not InitializeSecurityDescriptor(newSecDecr, SECURITY_DESCRIPTOR_REVISION) then
        Exit;
      if not GetSecurityDescriptorDacl(secDescr, daclPresent, dacl, daclExists) then
        Exit;
      ZeroMemory(@aclSizeInfo, SizeOf(ACL_SIZE_INFORMATION));
      aclSizeInfo.AclBytesInUse := SizeOf(ACL);
      if dacl <> nil then
        if not GetAclInformation(dacl^, @aclSizeInfo, SizeOf(ACL_SIZE_INFORMATION), AclSizeInformation) then
          Exit;
      newAclSize := aclSizeInfo.AclBytesInUse + (2 * SizeOf(ACCESS_ALLOWED_ACE)) +
        (2 * GetLengthSid(sid)) - (2 * SizeOf(DWORD));
      newAcl := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, newAclSize);
      if newAcl = nil then
        Exit;
      if not InitializeAcl(newAcl^, newAclSize, ACL_REVISION) then
        Exit;
      skipAdd := false;
      if daclPresent then begin
        for iAce := 0 to aclSizeInfo.AceCount - 1 do begin
          if not (GetAce(dacl^, iAce, pointer(tempAce)) and
                  AddAce(newAcl^, ACL_REVISION, MAXDWORD, tempAce, tempAce.AceSize))
          then
            Exit;
          if (tempAce.AceType = ACCESS_ALLOWED_ACE_TYPE) and
             EqualSID(@PACCESS_ALLOWED_ACE(tempAce)^.SidStart, sid)
          then
            skipAdd := true;
        end;
      end;
      if not skipAdd then begin
        ace := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, SizeOf(ACCESS_ALLOWED_ACE) +
          GetLengthSid(sid) - SizeOf(DWORD));
        if ace = nil then
          Exit;
        ace.Header.AceType := ACCESS_ALLOWED_ACE_TYPE;
        ace.Header.AceFlags := CONTAINER_INHERIT_ACE or INHERIT_ONLY_ACE or OBJECT_INHERIT_ACE;
        ace.Header.AceSize := SizeOf(ACCESS_ALLOWED_ACE) + GetLengthSid(sid) - SizeOf(DWORD);
        ace.Mask := GENERIC_ACCESS;
        if not (CopySid(GetLengthSid(sid), @ace.SidStart, sid) and
                AddAce(newAcl^, ACL_REVISION, MAXDWORD, ace, ace.Header.AceSize))
        then
          Exit;
        ace.Header.AceFlags := NO_PROPAGATE_INHERIT_ACE;
        ace.Mask := WINSTA_ALL;
        if not (AddAce(newAcl^, ACL_REVISION, MAXDWORD, ace, ace.Header.AceSize) and
                SetSecurityDescriptorDacl(newSecDecr, true, newAcl, false) and
                SetUserObjectSecurity(station, secInfo, newSecDecr))
        then
          Exit;
      end;
      Result := true;
    finally
      if assigned(ace) then
        HeapFree(GetProcessHeap, 0, ace);
      if assigned(newAcl) then
        HeapFree(GetProcessHeap, 0, newAcl);
      if assigned(secDescr) then
        HeapFree(GetProcessHeap, 0, secDescr);
      if assigned(newSecDecr) then
        HeapFree(GetProcessHeap, 0, newSecDecr);
    end;
  end; { DSiAddAceToWindowStation }

  {:Convert affinity mask into a string representation (0..9, A..V).
    @author  gabr
    @since   2003-11-14
  }        
  function DSiAffinityMaskToString(affinityMask: DWORD): string;
  var
    idxID: integer;
  begin
    Result := '';
    for idxID := 1 to 32 do begin
      if Odd(affinityMask) then
        Result := Result + DSiCPUIDs[idxID];
      affinityMask := affinityMask SHR 1;
    end;
  end; { DSiAffinityMaskToString }

  {:Converts 'current process' pseudo-handle into a real handle belonging to the same
    process. Don't forget to close the returned handle!
    @author  gabr
    @since   2009-03-16
  }
  function  DSiGetCurrentProcessHandle: THandle;
  begin
    DuplicateHandle(GetCurrentProcess, GetCurrentProcess, GetCurrentProcess, @Result, 0,
      false, DUPLICATE_SAME_ACCESS);
  end; { DSiGetCurrentProcessHandle } 

  {:Converts 'current thread' pseudo-handle into a real handle belonging to the same
    process. Don't forget to close the returned handle!
    @author  gabr
    @since   2009-03-16
  }
  function  DSiGetCurrentThreadHandle: THandle;
  begin
    DuplicateHandle(GetCurrentProcess, GetCurrentThread, GetCurrentProcess, @Result, 0,
      false, DUPLICATE_SAME_ACCESS);
  end; { DSiGetCurrentThreadHandle }

  {:Enables specified privilege for the current process.
    @author  Gre-Gor
    @since   2004-02-12
  }
  function DSiEnablePrivilege(const privilegeName: string): boolean;
  var
    hToken   : THandle;
    retLength: DWORD;
    tokenPriv: TTokenPrivileges;
  begin
    if not DSiIsWinNT then
      Result := true
    else begin
      Result := false;
      if not OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then
        Exit;
      try
        tokenPriv.PrivilegeCount := 1;
        if not LookupPrivilegeValue(nil, PChar(privilegeName), tokenPriv.Privileges[0].Luid) then
          Exit;
        tokenPriv.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
        if not AdjustTokenPrivileges(hToken, false, tokenPriv, SizeOf(tokenPriv), nil, retLength) then
          Exit;
        Result := true;
      finally CloseHandle(hToken); end;
    end;
  end; { DSiEnablePrivilege }

  {:Executes an external program.
    @author  Miha-R
    @returns MaxInt if CreateProcess fails or process exit code if wait is specified or 0
             in other cases.
    @since   2002-11-25
  }
  function DSiExecute(const commandLine: string; visibility: integer;
    const workDir: string; wait: boolean): cardinal;
  var
    processInfo: TProcessInformation;
    startupInfo: TStartupInfo;
    tmpCmdLine : string;
    useWorkDir : string;
  begin
    if workDir = '' then
      GetDir(0, useWorkDir)
    else
      useWorkDir := workDir;
    FillChar(startupInfo, SizeOf(startupInfo), #0);
    startupInfo.cb := SizeOf(startupInfo);
    startupInfo.dwFlags := STARTF_USESHOWWINDOW;
    startupInfo.wShowWindow := visibility;
    tmpCmdLine := commandLine;
    {$IFDEF Unicode}UniqueString(tmpCmdLine);{$ENDIF Unicode}
    if not CreateProcess(nil, PChar(tmpCmdLine), nil, nil, false,
             CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil,
             PChar(useWorkDir), startupInfo, processInfo)
    then
      Result := MaxInt
    else begin
      if wait then begin
        WaitForSingleObject(processInfo.hProcess, INFINITE);
        GetExitCodeProcess(processInfo.hProcess, Result);
      end
      else
        Result := 0;
      CloseHandle(processInfo.hProcess);
      CloseHandle(processInfo.hThread);
    end;
  end; { DSiExecute }


  {:Simplified DSiExecuteAsUser.
    @author  gabr
    @returns Returns MaxInt if any Win32 API fails or process exit code if wait is
             specified or 0 in other cases.
    @since   2010-06-18
  }
  function  DSiExecuteAsUser(const commandLine, username, password: string;
    var winErrorCode: cardinal; const domain: string; visibility: integer;
    const workDir: string; wait: boolean): cardinal;
  var
    processInfo: TProcessInformation;
  begin
    Result := DSiExecuteAsUser(commandLine, username, password, winErrorCode, processInfo,
      domain, visibility, workDir, wait, nil);
    if (Result <> cardinal(MaxInt)) and wait then begin
      DSiCloseHandleAndNull(processInfo.hProcess);
      DSiCloseHandleAndNull(processInfo.hThread);
    end;
  end; { DSiExecuteAsUser }

  {:Executes process as another user. Same as DSiExecute on 9x architecture.
    @author  gabr
    @returns Returns MaxInt if any Win32 API fails or process exit code if wait is
             specified or 0 in other cases.
    @since   2002-12-19
  }
  function DSiExecuteAsUser(const commandLine, username, password: string;
    var winErrorCode: cardinal; var processInfo: TProcessInformation;
    const domain: string; visibility: integer; const workDir: string; wait: boolean;
    startInfo: PStartupInfo): cardinal;
  var
    dwSize            : DWORD;
    hProcess          : THandle;
    interDesktop      : HDESK;
    interWindowStation: HWINSTA;
    lastError         : DWORD;
    logonHandle       : THandle;
    logonSID          : PSID;
    lpvEnv            : pointer;
    origWindowStation : HWINSTA;
    startupInfo       : TStartupInfo;
    startupInfoW      : TStartupInfoW;
    szUserProfile     : PWideChar;
    tmpCmdLine        : string;
    useStartInfo      : pointer;
    workDirW          : WideString;
  begin
    Result := MaxInt;
    logonHandle := 0;
    lpvEnv := nil;
    szUserProfile := nil;
    origWindowStation := 0;
    logonSID := nil;
    interWindowStation := 0;
    interDesktop := 0;
    logonHandle := 0;
    hProcess := 0;
    try
      if not DSiIsWinNT then
        Result := DSiExecute(commandLine, visibility, workDir, wait)
      else begin
        try
          if username <> '' then begin
            if not DSiLogonAs(username, password, domain, logonHandle) then
              Exit;
          end
          else begin
            hProcess := OpenProcess(STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $FFF, true, GetCurrentProcessID);
            if hProcess = 0 then
              Exit;
            if not OpenProcessToken(hProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, logonHandle) then
              Exit;
          end;
            
          if workDir <> '' then
            workDirW := workDir
          else begin
            GetMem(szUserProfile, SizeOf(WideChar) * 257);
            dwSize := 256;
            if not DSiGetUserProfileDirectoryW(logonHandle, szUserProfile, dwSize) then
              Exit;
            if workDir = '' then
              workDirW := szUserProfile;
          end;
          
          origWindowStation := GetProcessWindowStation;
          if origWindowStation = 0 then
            Exit;
          interWindowStation := OpenWindowStation('winsta0', false, READ_CONTROL OR WRITE_DAC);
          if interWindowStation = 0 then
            Exit;
          if not SetProcessWindowStation(interWindowStation) then
            Exit;
          interDesktop := OpenDesktop('default', 0, false, READ_CONTROL OR WRITE_DAC OR
            DESKTOP_WRITEOBJECTS OR DESKTOP_READOBJECTS);
          if (not SetProcessWindowStation(origWindowStation)) or
             (interDesktop = 0) or
             (not DSiGetLogonSID(logonHandle, logonSID))
          then
            Exit;
          if (not assigned(logonSID)) or
             (not DSiAddAceToWindowStation(interWindowStation, logonSID)) or
             (not DSiAddAceToDesktop(interDesktop, logonSID)) then 
            Exit;

          FillChar(startupInfo, SizeOf(startupInfo), #0);
          startupInfo.cb := SizeOf(startupInfo);
          startupInfo.dwFlags := STARTF_USESHOWWINDOW;
          startupInfo.lpDesktop := PChar('winsta0\default');
          startupInfo.wShowWindow := visibility;
          if username = '' then begin
            useStartInfo := startInfo;
            if not assigned(useStartInfo) then
              useStartInfo := @startupInfo;
            tmpCmdLine := commandLine;
            {$IFDEF Unicode}UniqueString(tmpCmdLine);{$ENDIF Unicode}
            if CreateProcess(nil, PChar(tmpCmdLine), nil, nil, false,
                 CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil,
                 PChar(string(workDirW)), PStartupInfo(useStartInfo)^, processInfo)
            then
              Result := 0;
          end
          else begin
            // Impersonate client to ensure access to executable file.
            if (not ImpersonateLoggedOnUser(logonHandle)) then
              Exit;
            try
              useStartInfo := startInfo;
              if not assigned(useStartInfo) then
                useStartInfo := @startupInfo;
              tmpCmdLine := commandLine;
              {$IFDEF Unicode}UniqueString(tmpCmdLine);{$ENDIF Unicode}
              if DSiCreateProcessAsUser(logonHandle, nil, PChar(tmpCmdLine), nil,
                   nil, false, {CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS} CREATE_DEFAULT_ERROR_MODE, nil,
                   PChar(string(workDirW)), PStartupInfo(useStartInfo)^, processInfo)
              then
                Result := 0;
              lastError := GetLastError;
            finally
              if (username <> '') then
                RevertToSelf;
            end;
            SetLastError(lastError);
          end;

          if (Result <> 0) and (GetLastError = ERROR_PRIVILEGE_NOT_HELD) and (username <> '') then begin
            if not DSiCreateEnvironmentBlock(lpvEnv, logonHandle, true) then
              Exit;
            Assert(SizeOf(TStartupInfoA) = SizeOf(TStartupInfoW));
            Move(useStartInfo^, startupInfoW, SizeOf(TStartupInfoW));
            startupInfoW.lpDesktop := nil;
            tmpCmdLine := commandLine;
            {$IFDEF Unicode}UniqueString(tmpCmdLine);{$ENDIF Unicode}
            if DSiCreateProcessWithLogonW(PWideChar(WideString(username)),
              PWideChar(WideString(domain)), PWideChar(WideString(password)),
              1 {LOGON_WITH_PROFILE}, nil, PWideChar(WideString(tmpCmdLine)),
              CREATE_UNICODE_ENVIRONMENT, lpvEnv, PWideChar(WideString(workDirW)),
              startupInfoW, processInfo)
            then
              Result := 0;
          end;

          if Result = 0 then begin
            lastError := GetLastError;
            if wait then begin
              WaitForSingleObject(processInfo.hProcess, INFINITE);
              GetExitCodeProcess(processInfo.hProcess, Result);
              DSiCloseHandleAndNull(processInfo.hProcess);
              DSiCloseHandleAndNull(processInfo.hThread);
            end
            else begin
              Result := 0;
              if username <> '' then begin
                CreateProcessWatchdog(TCleanupAces.Create(processInfo.hProcess, interWindowStation, interDesktop, logonSID));
                interWindowStation := 0;
                interDesktop := 0;
                logonSID := nil;
              end;
            end;
            SetLastError(lastError);
          end;

        finally
          lastError := GetLastError;
          if assigned(lpvEnv) then
            DSiDestroyEnvironmentBlock(lpvEnv);
          DSiCloseHandleAndNull(logonHandle);
          DSiCloseHandleAndNull(hProcess);
          DSiFreeMemAndNil(pointer(szUserProfile));
          if origWindowStation <> 0 then
            SetProcessWindowStation(origWindowStation);
          if assigned(logonSID) and (username <> '') then begin
            if interWindowStation <> 0 then
              DSiRemoveAceFromWindowStation(interWindowStation, logonSID);
            if interDesktop <> 0 then
              DSiRemoveAceFromDesktop(interDesktop, logonSID);
            HeapFree(GetProcessHeap, 0, logonSID);
          end;
          if interWindowStation <> 0 then
            CloseWindowStation(interWindowStation);
          if interDesktop <> 0 then
            CloseDesktop(interDesktop);
          SetLastError(lastError);
        end;
      end;
    finally winErrorCode := GetLastError; end;
  end; { DSiExecuteAsUser }

  {:Executes console process in a hidden window and captures its output in a TStrings
    object.
    Totaly reworked on 2006-01-23. New code contributed by matej.
    Handles only up to 1 MB of console process output.
    @returns ID of the console process or 0 if process couldn't be started.
    @author  aoven, Lee_Nover, gabr, matej, mitja
    @since   2003-05-24
  }
  function DSiExecuteAndCapture(const app: string; output: TStrings; const workDir: string;
    var exitCode: longword; waitTimeout_sec: integer; onNewLine: TDSiOnNewLineCallback): cardinal;
  var
    endTime_ms         : int64;
    lineBuffer         : PAnsiChar;
    lineBufferSize     : integer;
    partialLine        : string;
    runningTimeLeft_sec: integer;

    procedure ProcessPartialLine(buffer: PAnsiChar; numBytes: integer);
    var
      now_ms: int64;
      p     : integer;
    begin
      if lineBufferSize < (numBytes + 1) then begin
        lineBufferSize := numBytes + 1;
        ReallocMem(lineBuffer, lineBufferSize);
      end;
      // called made sure that buffer is zero terminated
      OemToCharA(buffer, lineBuffer);
      {$IFDEF Unicode}
      partialLine := partialLine + UnicodeString(StrPas(lineBuffer));
      {$ELSE}
      partialLine := partialLine + StrPas(lineBuffer);
      {$ENDIF Unicode}
      repeat
        p := Pos(#13#10, partialLine);
        if p <= 0 then
          break; //repeat
        now_ms := DSiTimeGetTime64;
        runningTimeLeft_sec := (endTime_ms - now_ms) div 1000;
        onNewLine(Copy(partialLine, 1, p-1), runningTimeLeft_sec);
        endTime_ms := now_ms + runningTimeLeft_sec * 1000;
        Delete(partialLine, 1, p+1);
      until false;
    end; { ProcessPartialLine }

  const
    SizeReadBuffer = 1048576;  // 1 MB Buffer

  var
    appRunning      : integer;
    appW            : string;
    buffer          : PAnsiChar;
    bytesLeftThisMsg: integer;
    bytesRead       : DWORD;
    err             : cardinal;
    processInfo     : TProcessInformation;
    readPipe        : THandle;
    security        : TSecurityAttributes;
    start           : TStartUpInfo;
    totalBytesAvail : integer;
    totalBytesRead  : DWORD;
    useWorkDir      : string;
    writePipe       : THandle;

  begin { DSiExecuteAndCapture }
    Result := 0;
    err := 0;
    partialLine := '';
    lineBufferSize := 0;
    lineBuffer := nil;
    endTime_ms := DSiTimeGetTime64 + waitTimeout_sec * 1000;
    security.nLength := SizeOf(TSecurityAttributes);
    security.bInheritHandle := true;
    security.lpSecurityDescriptor := nil;
    if CreatePipe (readPipe, writePipe, @security, 0) then begin
      buffer := AllocMem(SizeReadBuffer + 1);
      FillChar(Start,Sizeof(Start),#0);
      start.cb := SizeOf(start);
      start.hStdOutput := writePipe;
      start.hStdInput := readPipe;
      start.hStdError := writePipe;
      start.dwFlags := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
      start.wShowWindow := SW_HIDE;
      if workDir = '' then
        GetDir(0, useWorkDir)
      else
        useWorkDir := workDir;
      appW := app;
      {$IFDEF Unicode}UniqueString(appW);{$ENDIF Unicode}
      if CreateProcess(nil, PChar(appW), @security, @security, true,
           CREATE_NO_WINDOW or NORMAL_PRIORITY_CLASS, nil, PChar(useWorkDir), start,
           processInfo) then
      begin
        SetLastError(0); // [Mitja] found a situation where CreateProcess succeeded but the last error was 126
        Result := processInfo.hProcess;
        totalBytesRead := 0;
        repeat
          appRunning := WaitForSingleObject(processInfo.hProcess, 100);
          if not PeekNamedPipe(readPipe, @buffer[totalBytesRead],
                   SizeReadBuffer - totalBytesRead, @bytesRead, @totalBytesAvail,
                   @bytesLeftThisMsg)
          then
            break //repeat
          else if bytesRead > 0 then
            ReadFile(readPipe, buffer[totalBytesRead], bytesRead, bytesRead, nil);
          buffer[totalBytesRead + bytesRead] := #0; // required for ProcessPartialLine
          if assigned(onNewLine) then
            ProcessPartialLine(@buffer[totalBytesRead], bytesRead);
          totalBytesRead := totalBytesRead + bytesRead;
          if totalBytesRead = SizeReadBuffer then
            raise Exception.Create('DSiExecuteAndCapture: Buffer full!');
        until (appRunning <> WAIT_TIMEOUT) or (DSiTimeGetTime64 > endTime_ms);
        if partialLine <> '' then begin
          runningTimeLeft_sec := 0;
          onNewLine(partialLine, runningTimeLeft_sec);
        end;
        OemToCharA(buffer, buffer);
        {$IFDEF Unicode}
        output.Text := UnicodeString(StrPas(Buffer));
        {$ELSE}
        output.Text := StrPas(buffer);
        {$ENDIF Unicode}
      end
      else
        err := GetLastError;
      FreeMem(buffer);
      GetExitCodeProcess(processInfo.hProcess, exitCode);
      CloseHandle(processInfo.hProcess);
      CloseHandle(processInfo.hThread);
      CloseHandle(readPipe);
      CloseHandle(writePipe);
      DSiFreeMemAndNil(pointer(lineBuffer));
    end;
    if err <> 0 then
      SetLastError(err);
  end; { DSiExecuteAndCapture }

  {:Retrieves affinity mask of the current process as a list of CPU IDs (0..9, A..V).
    @author  gabr
    @since   2003-11-12
  }
  function DSiGetProcessAffinity: string;
  begin
    Result := DSiAffinityMaskToString(DSiGetProcessAffinityMask);
  end; { DSiGetProcessAffinity }

  {:Retrieves current process' affinity mask as a DWORD.
    @author  gabr
    @since   2003-11-14
  }        
  function DSiGetProcessAffinityMask: DWORD;
  var
    processAffinityMask: {$IF CompilerVersion >= 23}NativeUInt{$ELSE}Cardinal{$IFEND};
    systemAffinityMask : {$IF CompilerVersion >= 23}NativeUInt{$ELSE}Cardinal{$IFEND};
  begin
    if not DSiIsWinNT then
      Result := 1
    else begin
      GetProcessAffinityMask(GetCurrentProcess, processAffinityMask, systemAffinityMask);
      Result := processAffinityMask;
    end;
  end; { DSiGetProcessAffinityMask }

  {:Returns memory counters for the current process.
    Requires Windows NT.
    @author  gabr
    @since   2006-12-20
  }
  function DSiGetProcessMemory(var memoryCounters: TProcessMemoryCounters): boolean;
  begin
    Result := DSiGetProcessMemory(GetCurrentProcess, memoryCounters);
  end; { DSiGetProcessMemory }

  {:Returns memory counters for a process.
    Requires Windows NT.
    @author  gabr
    @since   2006-12-20
  }
  function DSiGetProcessMemory(process: THandle; var memoryCounters:
    TProcessMemoryCounters): boolean;
  begin
    FillChar(memoryCounters, SizeOf(memoryCounters), 0);
    memoryCounters.cb := SizeOf(memoryCounters);
    Result := DSiGetProcessMemoryInfo(process, @memoryCounters, memoryCounters.cb);
  end; { DSiGetProcessMemory }

  {:Returns exe file name of a process.
    Requires Windows NT.
    @author  gabr
    @since   2007-11-07
  }
  function DSiGetProcessFileName(process: THandle; var processName: string): boolean;
  var
    count     : DWORD;
    fileName  : array [0..MAX_PATH] of char;
    mainModule: HMODULE;
  begin
    Result := false; 
    if DSiEnumProcessModules(process, @mainModule, 1, count) then begin
      Result := (DSiGetModuleFileNameEx(process, mainModule, fileName, MAX_PATH) > 0);
      if (not Result) and (GetLastError = ERROR_INVALID_HANDLE) then
        Result := (DSiGetProcessImageFileName(process, fileName, MAX_PATH) > 0);
      if Result then
        processName := string(fileName);
    end;
  end; { DSiGetProcessFileName }

  {:Returns owner (user and domain) of the specified process. Requires Toolhelp API (e.g.
    non-NT4 OS).
    @author  Gre-Gor
    @since   2004-02-12
  }
  function DSiGetProcessOwnerInfo(const processName: string; var user,
    domain: string): boolean;
  var
    processID: DWORD;
  begin
    if not DSiGetProcessID(processName, processID) then
      Result := false
    else
      Result := DSiGetProcessOwnerInfo(processID, user, domain);
  end; { DSiGetProcessOwnerInfo }

  procedure RetrieveSIDInfo(sid: PSID; var user, domain: string);
  var
    domainSize: DWORD;
    sidUse    : SID_NAME_USE;
    userSize  : DWORD;
  begin
    userSize := 257;
    domainSize := 257;
    SetLength(user, userSize);
    SetLength(domain, domainSize);
    if not LookupAccountSID(nil, sid, PChar(user), userSize, PChar(domain), domainSize, sidUse) then
      if GetLastError = ERROR_NONE_MAPPED then
        user := '?';
    user := PChar(user);
    domain := PChar(domain);
  end; { RetrieveSIDInfo }

  function GetOwnerName(descriptor: PSecurityDescriptor; var user,
    domain: string): boolean;
  var
    defaulted: BOOL;
    sid      : PSID;
  begin
    Result := false;
    if not GetSecurityDescriptorOwner(descriptor, sid, defaulted) then
      Exit;
    RetrieveSIDInfo(sid, user, domain);
    Result := true;
  end; { GetOwnerName }

  {:Returns owner (user and domain) of the specified process.
    @author  Gre-Gor
    @since   2004-02-12
  }
  function DSiGetProcessOwnerInfo(processID: DWORD; var user, domain: string): boolean;
  var
    descrSize    : DWORD;
    neededSize   : DWORD;
    process      : THandle;
    securityDescr: PSecurityDescriptor;
    tmpResult    : boolean;
  begin
    Result := false;
    if not DSiEnablePrivilege('SeDebugPrivilege') then
      Exit;
    process := OpenProcess(PROCESS_ALL_ACCESS, false, processID);
    try
      descrSize := 4096;
      neededSize := 0;
      securityDescr := AllocMem(descrSize);
      while true do begin
        tmpResult := GetKernelObjectSecurity(process, OWNER_SECURITY_INFORMATION,
          securityDescr, descrSize, neededSize);
        if tmpResult then begin
          if (neededSize > 0) and (descrSize <> neededSize) then begin
            descrSize := neededSize;
            ReallocMem(securityDescr, descrSize);
          end
          else begin
            Result := GetOwnerName(securityDescr, user, domain);
            break; //while
          end;
        end
        else if GetLastError <> ERROR_INSUFFICIENT_BUFFER then begin
          ReallocMem(securityDescr, 0);
          break; //while
        end;
      end;
    finally CloseHandle(process); end;
  end; { TDSiRegistry.DSiGetProcessOwnerInfo}

  {:Returns various times of the current process.
    @author  gabr
    @since   2006-12-20
  }
  function DSiGetProcessTimes(var creationTime: TDateTime; var userTime, kernelTime:
    int64): boolean;
  var
    exitTime: TDateTime;
  begin
    Result := DSiGetProcessTimes(GetCurrentProcess, creationTime, exitTime, userTime,
      kernelTime);
  end; { DSiGetProcessTimes }

  {:Returns various times of a process.
    @author  gabr
    @since   2006-12-20
  }
  function DSiGetProcessTimes(process: THandle; var creationTime, exitTime: TDateTime; var
    userTime, kernelTime: int64): boolean;
  var
    fsCreationTime: TFileTime;
    fsExitTime    : TFileTime;
    fsKernelTime  : TFileTime;
    fsUserTime    : FileTime;
  begin
    Result :=
      GetProcessTimes(process, fsCreationTime, fsExitTime, fsKernelTime, fsUserTime) and
      DSiFileTimeToDateTime(fsCreationTime, creationTime) and
      DSiFileTimeToDateTime(fsExitTime, exitTime);
    if Result then begin
      kernelTime := DSiFileTimeToMicroSeconds(fsKernelTime);
      userTime := DSiFileTimeToMicroSeconds(fsUserTime);
    end;
  end; { DSiGetProcessTimes }

  {:Retrieves ID of the specified process. Requires Toolhelp API.
    @returns False if ID cannot be retrieved. Check GetLastError - if it is 0, process
             doesn't exist; otherwise it contains the Win32 error code.
    @author  gabr
    @since   2004-02-12
  }
  function DSiGetProcessID(const processName: string; var processID: DWORD): boolean;
  var
    hSnapshot: THandle;
    procEntry: TProcessEntry32;
  begin
    Result := false;
    hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if hSnapshot = 0 then
      Exit;
    try
      procEntry.dwSize := Sizeof(procEntry);
      if not Process32First(hSnapshot, procEntry) then
        Exit;
      repeat
        if AnsiSameText(procEntry.szExeFile, processName) then begin
          processID := procEntry.th32ProcessID;
          Result := true;
          break; // repeat
        end;
      until not Process32Next(hSnapshot, procEntry);
    finally DSiCloseHandleAndNull(hSnapshot); end;
  end; { DSiGetProcessID }

  {:Retrieves system affinity mask as a list of CPU IDs (0..9, A..V).
    @author  gabr
    @since   2003-11-12
  }
  function DSiGetSystemAffinity: string;
  begin
    Result := DSiAffinityMaskToString(DSiGetSystemAffinityMask);
  end; { DSiGetSystemAffinity }

  {:Retrieves system affinity mask as a DWORD.
    @author  gabr
    @since   2003-11-14
  }
  function DSiGetSystemAffinityMask: DWORD;
  var
    processAffinityMask: {$IF CompilerVersion >= 23}NativeUInt{$ELSE}Cardinal{$IFEND};
    systemAffinityMask : {$IF CompilerVersion >= 23}NativeUInt{$ELSE}Cardinal{$IFEND};
  begin
    if not DSiIsWinNT then
      Result := 1
    else begin
      GetProcessAffinityMask(GetCurrentProcess, processAffinityMask, systemAffinityMask);
      Result := systemAffinityMask;
    end;
  end; { TDSiRegistry.DSiGetSystemAffinityMask }
  
  {:Retrieves affinity mask of the current thread as a list of CPU IDs (0..9,
    A..V).
    @author  gabr
    @since   2003-11-12
  }
  function DSiGetThreadAffinity: string;
  begin
    if not DSiIsWinNT then
      Result := '0'
    else
      Result := DSiAffinityMaskToString(DSiGetThreadAffinityMask);
  end; { DSiGetThreadAffinity }

  {:Retrieves affinity mask of the current thread as a DWORD.
    @author  gabr
    @since   2003-11-14
  }
  function DSiGetThreadAffinityMask: DWORD;
  var
    processAffinityMask: {$IF CompilerVersion >= 23}NativeUInt{$ELSE}Cardinal{$IFEND};
    systemAffinityMask : {$IF CompilerVersion >= 23}NativeUInt{$ELSE}Cardinal{$IFEND};
  begin
    if not DSiIsWinNT then
      Result := 1
    else begin
      GetProcessAffinityMask(GetCurrentProcess, processAffinityMask, systemAffinityMask);
      Result := SetThreadAffinityMask(GetCurrentThread, processAffinityMask);
      SetThreadAffinityMask(GetCurrentThread, Result);
    end;
  end; { DSiGetThreadAffinityMask }

  {:Returns [kernel time] + [user time] for the current thread.
    @author  gabr
    @since   2011-09-26
  }
  function DSiGetThreadTime: int64;
  var
    creationTime: TDateTime;
    kernelTime  : int64;
    userTime    : int64;
  begin
    Result := 0;
    if DSiGetThreadTimes(creationTime, userTime, kernelTime) then
      Result := userTime + kernelTime;
  end; { DSiGetThreadTime }

  function DSiGetThreadTime(thread: THandle): int64;
  var
    creationTime: TDateTime;
    exitTime    : TDateTime;
    kernelTime  : int64;
    userTime    : int64;
  begin
    Result := 0;
    if DSiGetThreadTimes(thread, creationTime, exitTime, userTime, kernelTime) then
      Result := userTime + kernelTime;
  end; { DSiGetThreadTime }

  {:Returns various times of the current thread.
    @author  gabr
    @since   2007-07-11
  }
  function DSiGetThreadTimes(var creationTime: TDateTime; var userTime, kernelTime:
    int64): boolean;
  var
    exitTime: TDateTime;
  begin
    Result := DSiGetThreadTimes(GetCurrentThread, creationTime, exitTime, userTime,
      kernelTime);
  end; { DSiGetThreadTimes }

  {:Returns various times of a thread.
    @author  gabr
    @since   2007-07-11
  }
  function DSiGetThreadTimes(thread: THandle; var creationTime, exitTime: TDateTime; var
    userTime, kernelTime: int64): boolean;
  var
    fsCreationTime: TFileTime;
    fsExitTime    : TFileTime;
    fsKernelTime  : TFileTime;
    fsUserTime    : FileTime;
  begin
    Result :=
      GetThreadTimes(thread, fsCreationTime, fsExitTime, fsKernelTime, fsUserTime) and
      DSiFileTimeToDateTime(fsCreationTime, creationTime) and
      DSiFileTimeToDateTime(fsExitTime, exitTime);
    if Result then begin
      kernelTime := DSiFileTimeToMicroSeconds(fsKernelTime);
      userTime := DSiFileTimeToMicroSeconds(fsUserTime);
    end;
  end; { DSiGetThreadTimes }

  {gp}
  // Returns True if user can be impersonated. Always True on 9x architecture.
  function DSiImpersonateUser(const username, password, domain: string): boolean;
  var
    lastError  : DWORD;
    logonHandle: THandle;
  begin
    if not DSiIsWinNT then
      Result := true
    else begin
      Result := false;
      if DSiLogonAs(username, password, domain, logonHandle) then begin
        Result := DSiImpersonateLoggedOnUser(logonHandle);
        lastError := GetLastError;
        CloseHandle(logonHandle);
        SetLastError(lastError);
      end;
    end;
  end; { DSiImpersonateUser }

  {:Increments working set for the current program by a specified ammount of bytes.
    @param   incMinSize Number of bytes to increment the minimum working set by (may be 0).
    @param   incMaxSize Number of bytes to increment the maximum working set by (may be 0).
    @author  gabr
    @since   2004-09-21
  }
  function DSiIncrementWorkingSet(incMinSize, incMaxSize: integer): boolean;
  var
    hProcess         : THandle;
    maxWorkingSetSize: {$IF CompilerVersion >= 23}NativeUInt{$ELSE}Cardinal{$IFEND};
    minWorkingSetSize: {$IF CompilerVersion >= 23}NativeUInt{$ELSE}Cardinal{$IFEND};
  begin
    Result := false;
    hProcess := OpenProcess(PROCESS_QUERY_INFORMATION OR PROCESS_SET_QUOTA, false,
      GetCurrentProcessId);
    try
      if not GetProcessWorkingSetSize(hProcess, minWorkingSetSize, maxWorkingSetSize) then
        Exit;
      Inc(minWorkingSetSize, incMinSize);
      Inc(maxWorkingSetSize, incMaxSize);
      if minWorkingSetSize > maxWorkingSetSize then
        maxWorkingSetSize := minWorkingSetSize;
      Result := SetProcessWorkingSetSize(hProcess, minWorkingSetSize, maxWorkingSetSize);
    finally CloseHandle(hProcess); end;
  end; { DSiIncrementWorkingSet }

  {mr}
  function DSiIsDebugged: boolean;
  var
    isDebuggerPresent: function: Boolean; stdcall;
    kernelHandle     : THandle;
    p                : pointer;
  begin
    kernelHandle := GetModuleHandle(kernel32);
    @isDebuggerPresent := GetProcAddress(kernelHandle, 'IsDebuggerPresent');
    if assigned(isDebuggerPresent) then // Win98+/NT4+ only
      Result := isDebuggerPresent
    else
    begin // Win9x uses thunk pointer outside the module when under a debugger
      p := GetProcAddress(kernelHandle, 'GetProcAddress');
      Result := (DWORD(p) < kernelHandle);
    end;
  end; { DSiIsDebugged }

  function DSiLogonAs(const username, password: string;
    var logonHandle: THandle): boolean; overload;
  begin
    Result := DSiLogonAs(username, password, '.', logonHandle);
  end; { DSiLogonAs }
  
  {:A simple wrapper arount the LogonUser API that handles 'domain\user' input in the
    'username' field.
    @author  gabr
    @since   2010-04-08
  }
  function DSiLogonAs(const username, password, domain: string;
    var logonHandle: THandle): boolean; overload;
  var
    dsiDomain  : string;
    dsiUsername: string;
    posDomain  : integer;
  begin
    if not DSiIsWinNT then begin
      logonHandle := 0;
      Result := true;
    end
    else begin
      dsiDomain := domain;
      dsiUsername := username;
      if dsiDomain = '.' then begin
        posDomain := Pos('\', dsiUsername);
        if posDomain > 0 then begin
          dsiDomain := Copy(dsiUsername, 1, posDomain-1);
          Delete(dsiUsername, 1, posDomain);
        end;
      end;
      Result := DSiLogonUser(PChar(dsiUsername), PChar(dsiDomain), PChar(password),
        LOGON32_LOGON_INTERACTIVE, LOGON32_PROVIDER_DEFAULT, logonHandle);
      if not Result then
        logonHandle := 0;
    end;
  end; { DSiLogonAs }

  {ln}
  function DSiOpenURL(const URL: string; newBrowser: boolean): boolean;
  begin
    if NewBrowser then begin
      ShellExecute(0, nil, PChar(DSiGetDefaultBrowser), nil, nil, SW_SHOW);
      Sleep(500); // wait a bit to load the browser
    end;
    Result := (ShellExecute(0, nil, PChar(URL), nil, nil, SW_SHOW) > 32);
  end; { DSiOpenURL }

  {:Process all messages waiting in the current thread's message queue.
    @author  gabr
    @since   2003-08-25
  }        
  procedure DSiProcessThreadMessages;
  var
    msg: TMsg;
  begin
    while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) and (Msg.Message <> WM_QUIT) do begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
  end; { DSiProcessThreadMessages }

  {mr}
  function DSiRealModuleName: string;
  var
    FileName: array[0..MAX_PATH - 1] of Char;
  begin
    GetModuleFileName(HInstance, FileName, SizeOf(FileName));
    Result := string(FileName);
  end; { DSiRealModuleName }

  function DSiRemoveAceFromDesktop(desktop: HDESK; sid: PSID): boolean;
  var
    aclSizeInfo : ACL_SIZE_INFORMATION;
    dacl        : PACL;
    daclExists  : LongBool;
    daclPresent : LongBool;
    iAce        : integer;
    newAcl      : PACL;
    newAclSize  : DWORD;
    newSecDescr : PSECURITY_DESCRIPTOR;
    sdSize      : DWORD;
    sdSizeNeeded: DWORD;
    secDescr    : PSECURITY_DESCRIPTOR;
    secInfo     : SECURITY_INFORMATION;
    tempAce     : PACE_HEADER;
  begin
    Result := false;
    secDescr := nil;
    newSecDescr := nil;
    newAcl := nil;
    secInfo := DACL_SECURITY_INFORMATION;
    sdSize := 0;
    try
      if not GetUserObjectSecurity(desktop, secInfo, secDescr, sdSize, sdSizeNeeded) then begin
        if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
          Exit;
        secDescr := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, sdSizeNeeded);
        if secDescr = nil then
          Exit;
        newSecDescr := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, sdSizeNeeded);
        if newSecDescr = nil then
          Exit;
        sdSize := sdSizeNeeded;
        if not GetUserObjectSecurity(desktop, secInfo, secDescr, sdSize, sdSizeNeeded) then
          Exit;
      end;
      if not InitializeSecurityDescriptor(newSecDescr, SECURITY_DESCRIPTOR_REVISION) then
        Exit;
      if not GetSecurityDescriptorDacl(secDescr, daclPresent, dacl, daclExists) then
        Exit;
      ZeroMemory(@aclSizeInfo, SizeOf(ACL_SIZE_INFORMATION));
      aclSizeInfo.AclBytesInUse := SizeOf(ACL);
      if assigned(dacl) then begin
        if not GetAclInformation(dacl^, @aclSizeInfo, SizeOf(ACL_SIZE_INFORMATION), AclSizeInformation) then
          Exit;
      end;
      newAclSize := aclSizeInfo.AclBytesInUse + SizeOf(ACCESS_ALLOWED_ACE) +
        GetLengthSid(sid) - SizeOf(DWORD);
      newAcl := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, newAclSize);
      if (newAcl = nil) or (not InitializeAcl(newAcl^, newAclSize, ACL_REVISION)) then
        Exit;
      if daclPresent then begin
        // Copy the ACEs to the new ACL.
        for iAce := 0 to aclSizeInfo.AceCount - 1 do begin
          if not GetAce(dacl^, iAce, pointer(tempAce)) then
            Exit;
          if (tempAce.AceType <> ACCESS_ALLOWED_ACE_TYPE) or
             (not EqualSID(@PACCESS_ALLOWED_ACE(tempAce)^.SidStart, sid))
          then
            if not AddAce(newAcl^, ACL_REVISION, MAXDWORD, tempAce, tempAce.AceSize) then
              Exit;
        end;
      end;
      if not (SetSecurityDescriptorDacl(newSecDescr, true, newAcl, false) and
              SetUserObjectSecurity(desktop, secInfo, newSecDescr))
      then
        Exit;
      Result := true;
    finally
      if assigned(newAcl) then
        HeapFree(GetProcessHeap, 0, newAcl);
      if assigned(secDescr) then
        HeapFree(GetProcessHeap, 0, secDescr);
      if assigned(newSecDescr) then
        HeapFree(GetProcessHeap, 0, newSecDescr);
    end;
  end; { DSiRemoveAceFromDesktop }

  function DSiRemoveAceFromWindowStation(station: HWINSTA; sid: PSID): boolean;
  var
    ace        : ^ACCESS_ALLOWED_ACE;
    aclSizeInfo: ACL_SIZE_INFORMATION;
    dacl       : PACL;
    daclExists : LongBool;
    daclPresent: LongBool;
    iAce       : integer;
    newAcl     : PACL;
    newAclSize : DWORD;
    newSecDecr : PSECURITY_DESCRIPTOR;
    reqSdSize  : DWORD;
    sdSize     : DWORD;
    secDescr   : PSECURITY_DESCRIPTOR;
    secInfo    : SECURITY_INFORMATION;
    tempAce    : PACE_HEADER;
  begin
    Result := false;
    secInfo := DACL_SECURITY_INFORMATION;
    ace := nil;
    secDescr := nil;
    sdSize := 0;
    newAcl := nil;
    newSecDecr := nil;
    try
      if not GetUserObjectSecurity(station, secInfo, secDescr, sdSize, reqSdSize) then begin
        if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
          Exit;
        secDescr := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, reqSdSize);
        if secDescr = nil then
          Exit;
        newSecDecr := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, reqSdSize);
        if newSecDecr = nil then
          Exit;
        sdSize := reqSdSize;
        if not GetUserObjectSecurity(station, secInfo, secDescr, sdSize, reqSdSize) then
          Exit;
      end;
      if not InitializeSecurityDescriptor(newSecDecr, SECURITY_DESCRIPTOR_REVISION) then
        Exit;
      if not GetSecurityDescriptorDacl(secDescr, daclPresent, dacl, daclExists) then
        Exit;
      ZeroMemory(@aclSizeInfo, SizeOf(ACL_SIZE_INFORMATION));
      aclSizeInfo.AclBytesInUse := SizeOf(ACL);
      if dacl <> nil then
        if not GetAclInformation(dacl^, @aclSizeInfo, SizeOf(ACL_SIZE_INFORMATION), AclSizeInformation) then
          Exit;
      newAclSize := aclSizeInfo.AclBytesInUse + (2 * SizeOf(ACCESS_ALLOWED_ACE)) +
        (2 * GetLengthSid(sid)) - (2 * SizeOf(DWORD));
      newAcl := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, newAclSize);
      if newAcl = nil then
        Exit;
      if not InitializeAcl(newAcl^, newAclSize, ACL_REVISION) then
        Exit;
      if daclPresent then begin
        for iAce := 0 to aclSizeInfo.AceCount - 1 do begin
          if not GetAce(dacl^, iAce, pointer(tempAce)) then
            Exit;
          if (tempAce.AceType <> ACCESS_ALLOWED_ACE_TYPE) or
             (not EqualSID(@PACCESS_ALLOWED_ACE(tempAce)^.SidStart, sid))
          then 
            if not AddAce(newAcl^, ACL_REVISION, MAXDWORD, tempAce, tempAce.AceSize) then
              Exit;
        end;
      end;
      ace := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, SizeOf(ACCESS_ALLOWED_ACE) +
        GetLengthSid(sid) - SizeOf(DWORD));
      if ace = nil then
        Exit;
      ace.Header.AceType := ACCESS_ALLOWED_ACE_TYPE;
      ace.Header.AceFlags := CONTAINER_INHERIT_ACE or INHERIT_ONLY_ACE or OBJECT_INHERIT_ACE;
      ace.Header.AceSize := SizeOf(ACCESS_ALLOWED_ACE) + GetLengthSid(sid) - SizeOf(DWORD);
      ace.Mask := GENERIC_ACCESS;
      if not (CopySid(GetLengthSid(sid), @ace.SidStart, sid) and
              AddAce(newAcl^, ACL_REVISION, MAXDWORD, ace, ace.Header.AceSize))
      then
        Exit;
      ace.Header.AceFlags := NO_PROPAGATE_INHERIT_ACE;
      ace.Mask := WINSTA_ALL;
      if not (AddAce(newAcl^, ACL_REVISION, MAXDWORD, ace, ace.Header.AceSize) and
              SetSecurityDescriptorDacl(newSecDecr, true, newAcl, false) and
              SetUserObjectSecurity(station, secInfo, newSecDecr))
      then
        Exit;
      Result := true;
    finally
      if assigned(ace) then
        HeapFree(GetProcessHeap, 0, ace);
      if assigned(newAcl) then
        HeapFree(GetProcessHeap, 0, newAcl);
      if assigned(secDescr) then
        HeapFree(GetProcessHeap, 0, secDescr);
      if assigned(newSecDecr) then
        HeapFree(GetProcessHeap, 0, newSecDecr);
    end;
  end; { DSiRemoveAceFromWindowStation }

  {:Sets current process' affinity mask.
    @param   affinity List of CPUs to include in the affinity mask (0..9, A..V).
                      May contain processors not available on the system or
                      processors already excluded from the current process'
                      affinity mask.
    @returns CPUs that were actually included in the affinity mask.
    @author  gabr
    @since   2003-11-12
  }
  function DSiSetProcessAffinity(affinity: string): string;
  begin
    SetProcessAffinityMask(GetCurrentProcess,
      DSiValidateProcessAffinityMask(DSiStringToAffinityMask(affinity)));
    Result := DSiGetProcessAffinity;
  end; { DSiSetProcessAffinity }

  {:Sets priority class for all processes with the given name. Requires Toolhelp API (e.g.
    non-NT4 OS).
    @param   priority See SetPriorityClass API function.
    @author  gabr
    @since   2004-02-12
  }
  function DSiSetProcessPriorityClass(const processName: string;
    priority: DWORD): boolean;
  var
    hSnapshot: THandle;
    procEntry: TProcessEntry32;
    process  : THandle;
  begin
    Result := false;
    hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if hSnapshot = 0 then
      Exit;
    try
      procEntry.dwSize := Sizeof(procEntry);
      if not Process32First(hSnapshot, procEntry) then
        Exit;
      repeat
        if AnsiSameText(procEntry.szExeFile, processName) then begin
          process := OpenProcess(PROCESS_SET_INFORMATION, false, procEntry.th32ProcessID);
          if process = 0 then
            Exit;
          try
            SetPriorityClass(process, priority);
          finally DSiCloseHandleAndNull(process); end;
        end;
      until not Process32Next(hSnapshot, procEntry);
    finally DSiCloseHandleAndNull(hSnapshot); end;
  end; { TDSiRegistry.DSiSetProcessPriortyClass }
  
  {:Sets current thread's affinity mask.
    @param   affinity List of CPUs to include in the affinity mask (0..9, A..V).
                      May contain processors not available on the system or
                      processors already excluded from the current process' or
                      thread's affinity mask.
    @returns CPUs that were actually included in the affinity mask.
    @author  gabr
    @since   2003-11-12
  }        
  function DSiSetThreadAffinity(affinity: string): string;
  begin
    SetThreadAffinityMask(GetCurrentThread,
      DSiValidateThreadAffinityMask(DSiStringToAffinityMask(affinity)));
    Result := DSiGetThreadAffinity;
  end; { DSiSetThreadAffinity }

  {gp}
  // Reverts back to the original program 'personae'. Does nothing on the 9x architecture.
  procedure DSiStopImpersonatingUser;
  begin
    if DSiIsWinNT then 
      DSiRevertToSelf;
  end; { DSiStopImpersonatingUser }

  {:Convert affinity list (0..9, A..V) to the DWORD mask.
    @author  gabr
    @since   2003-11-14
  }        
  function DSiStringToAffinityMask(affinity: string): DWORD;
  var
    idxID: integer;
  begin
    Result := 0;
    for idxID := 32 downto 1 do begin
      Result := Result SHL 1;
      if Pos(DSiCPUIDs[idxID], affinity) > 0 then
        Result := Result OR 1;
    end; //for
  end; { DSiStringToAffinityMask }

  function DSiSendWMCloseToWindow(hWindow: THandle; lParam: integer): BOOL; stdcall;
  var
    idWindow: DWORD;
  begin
    GetWindowThreadProcessId(hWindow, @idWindow);
    if idWindow = DWORD(lParam) then
      PostMessage(hWindow, WM_CLOSE, 0, 0);
    Result := true;
  end; { DSiSendWMCloseToWindow }

  {:Terminates process by ID.
    Source: http://support.microsoft.com/kb/178893
    @author  M.C, gabr
    @since   2007-12-14
  }
  function DSiTerminateProcessById(processID: DWORD; closeWindowsFirst: boolean;
    maxWait_sec: integer): boolean;
  var
    hProcess: THandle;
  begin
    Result := false;
    hProcess := OpenProcess(SYNCHRONIZE OR PROCESS_TERMINATE, false, processID);
    if hProcess = 0 then
      Exit;
    try
      if closeWindowsFirst then begin
        EnumWindows(@DSiSendWMCloseToWindow, integer(processID));
        Result := (WaitForSingleObject(hProcess, maxWait_sec * 1000) = WAIT_OBJECT_0);
      end;
      if not Result then
        Result := TerminateProcess(hProcess, 0);
    finally CloseHandle(hProcess); end;
  end; { DSiTerminateProcessById }

  {mr}
  procedure DSiTrimWorkingSet;
  var
    hProcess: THandle;
  begin
    hProcess := OpenProcess(PROCESS_SET_QUOTA, false, GetCurrentProcessId);
    try
      SetProcessWorkingSetSize(hProcess, $FFFFFFFF, $FFFFFFFF);
    finally CloseHandle(hProcess); end;
  end; { DSiTrimWorkingSet }

  {:Validates process affinity mask (removes all CPUs that are not in the
    system affinity mask).
    @author  gabr
    @since   2003-11-14
  }
  function DSiValidateProcessAffinity(affinity: string): string;
  begin
    Result := DSiAffinityMaskToString(DSiValidateProcessAffinityMask(
                DSiStringToAffinityMask(affinity)));
  end; { DSiValidateProcessAffinity }

  {:Validates process affinity mask (removes all CPUs that are not in the
    system affinity mask).
    @author  gabr
    @since   2003-11-14
  }
  function DSiValidateProcessAffinityMask(affinityMask: DWORD): DWORD;
  begin
    Result := DSiGetSystemAffinityMask AND affinityMask;
  end; { TDSiRegistry.DSiValidateProcessAffinityMask }

  {:Validates process affinity mask (removes all CPUs that are not in the
    system affinity mask).
    @author  gabr
    @since   2003-11-14
  }
  function DSiValidateThreadAffinity(affinity: string): string;
  begin
    Result := DSiAffinityMaskToString(DSiValidateThreadAffinityMask(
                DSiStringToAffinityMask(affinity)));
  end; { DSiValidateThreadAffinityMask }

  function DSiValidateThreadAffinityMask(affinityMask: DWORD): DWORD;
  begin
    Result := DSiGetProcessAffinityMask AND affinityMask;
  end; { DSiValidateThreadAffinityMask }

  procedure DSiYield;
  begin
    if Win32Platform = VER_PLATFORM_WIN32_NT then
      SwitchToThread
    else
      Sleep(1);
  end; { DSiYield }

{ Memory }

  {mr}
  procedure DSiFreePidl(pidl: PItemIDList);
  var
    allocator: IMalloc;
  begin
    if Succeeded(SHGetMalloc(allocator)) then begin
      allocator.Free(pidl);
  {$IFDEF VER90}
      allocator.Release;
  {$ENDIF}
    end;
  end; { DSiFreePidl }

  {:Frees memory allocated with GetMem and nils the pointer to the memory.
    Does nothing if pointer is already nil.
    @author: gp
    @since   2003-05-24
  }
  procedure DSiFreeMemAndNil(var mem: pointer);
  var
    tmp: pointer;
  begin
    if assigned(mem) then begin
      tmp := mem;
      mem := nil;
      FreeMem(tmp);
    end;
  end; { DSiFreeMemAndNil }

  {:Wrapper for the GlobalMemoryStatusEx API. Required Windows 2000.
    @author: gp
    @since   2009-01-23
  }
  function DSiGetGlobalMemoryStatus(var memoryStatus: TMemoryStatusEx): boolean;
  begin
    FillChar(memoryStatus, SizeOf(memoryStatus), 0);
    memoryStatus.dwLength := SizeOf(memoryStatus);
    Result := DSiGlobalMemoryStatusEx(@memoryStatus);
  end; { DSiGlobalMemoryStatusEx }

{ Windows }

const //DSiAllocateHwnd window extra data offsets
  GWL_METHODCODE = SizeOf(pointer) * 0;
  GWL_METHODDATA = SizeOf(pointer) * 1;

  //DSiAllocateHwnd hidden window (and window class) name
  CDSiHiddenWindowName = 'DSiUtilWindow';

var
  //DSiAllocateHwnd lock
  GDSiWndHandlerCritSect: TRTLCriticalSection;
  //Count of registered windows in this instance
  GDSiWndHandlerCount: integer;
  GTerminateBackgroundTasks: THandle;

  {:Class message dispatcher for the DSiUtilWindow class. Fetches instance's WndProc from
    the window extra data and calls it.
  }
  function DSiClassWndProc(Window: HWND; Message, WParam, LParam: longint): longint; stdcall;
  var
    instanceWndProc: TMethod;
    msg            : TMessage;
  begin
    {$IFDEF CPUX64}
    instanceWndProc.Code := pointer(GetWindowLongPtr(Window, GWL_METHODCODE));
    instanceWndProc.Data := pointer(GetWindowLongPtr(Window, GWL_METHODDATA));
    {$ELSE}
    instanceWndProc.Code := pointer(GetWindowLong(Window, GWL_METHODCODE));
    instanceWndProc.Data := pointer(GetWindowLong(Window, GWL_METHODDATA));
    {$ENDIF ~CPUX64}
    if Assigned(TWndMethod(instanceWndProc)) then
    begin
      msg.msg := Message;
      msg.wParam := WParam;
      msg.lParam := LParam;
      msg.Result := 0;
      TWndMethod(instanceWndProc)(msg);
      Result := msg.Result
    end
    else
      Result := DefWindowProc(Window, Message, WParam,LParam);
  end; { DSiClassWndProc }

  {:Thread-safe AllocateHwnd.
    @author  gabr [based on http://fidoforum.ru/pages/new46s35o217746.ru.delphi and
                   TIcsWndHandler.AllocateHWnd from ICS v6 (http://www.overbyte.be)]
    @since   2007-05-30
  }
  function DSiAllocateHWnd(wndProcMethod: TWndMethod): HWND;
  var
    alreadyRegistered: boolean;
    tempClass        : TWndClass;
    utilWindowClass  : TWndClass;
  begin
    Result := 0;
    FillChar(utilWindowClass, SizeOf(utilWindowClass), 0);
    EnterCriticalSection(GDSiWndHandlerCritSect);
    try
      alreadyRegistered := GetClassInfo(HInstance, CDSiHiddenWindowName, tempClass);
      if (not alreadyRegistered) or (tempClass.lpfnWndProc <> @DSiClassWndProc) then begin
        if alreadyRegistered then
          {$IFDEF ScopedUnitNames}Winapi.{$ENDIF}Windows.UnregisterClass(CDSiHiddenWindowName, HInstance);
        utilWindowClass.lpszClassName := CDSiHiddenWindowName;
        utilWindowClass.hInstance := HInstance;
        utilWindowClass.lpfnWndProc := @DSiClassWndProc;
        utilWindowClass.cbWndExtra := SizeOf(TMethod);
        if {$IFDEF ScopedUnitNames}Winapi.{$ENDIF}Windows.RegisterClass(utilWindowClass) = 0 then
          raise Exception.CreateFmt('Unable to register DSiWin32 hidden window class. %s',
            [SysErrorMessage(GetLastError)]);
      end;
      Result := CreateWindowEx(WS_EX_TOOLWINDOW, CDSiHiddenWindowName, '', WS_POPUP,
        0, 0, 0, 0, 0, 0, HInstance, nil);
      if Result = 0 then
        raise Exception.CreateFmt('Unable to create DSiWin32 hidden window. %s',
                [SysErrorMessage(GetLastError)]);
      {$IFDEF CPUX64}
      SetWindowLongPtr(Result, GWL_METHODDATA, NativeInt(TMethod(wndProcMethod).Data));
      SetWindowLongPtr(Result, GWL_METHODCODE, NativeInt(TMethod(wndProcMethod).Code));
      {$ELSE}
      SetWindowLong(Result, GWL_METHODDATA, cardinal(TMethod(wndProcMethod).Data));
      SetWindowLong(Result, GWL_METHODCODE, cardinal(TMethod(wndProcMethod).Code));
      {$ENDIF ~CPUX64}
      Inc(GDSiWndHandlerCount);
    finally LeaveCriticalSection(GDSiWndHandlerCritSect); end;
  end; { DSiAllocateHWnd }

  {:Thread-safe DeallocateHwnd.
    @author  gabr [based on http://fidoforum.ru/pages/new46s35o217746.ru.delphi and
                   TIcsWndHandler.AllocateHWnd from ICS v6 (http://www.overbyte.be)]
    @since   2007-05-30
  }
  procedure DSiDeallocateHWnd(wnd: HWND);
  begin
    if wnd = 0 then
      Exit;
    DestroyWindow(wnd);
    EnterCriticalSection(GDSiWndHandlerCritSect);
    try
      Dec(GDSiWndHandlerCount);
      if GDSiWndHandlerCount <= 0 then
        {$IFDEF ScopedUnitNames}Winapi.{$ENDIF}Windows.UnregisterClass(CDSiHiddenWindowName, HInstance);
    finally LeaveCriticalSection(GDSiWndHandlerCritSect); end;
  end; { DSiDeallocateHWnd }

  {:Uses PowerCfg to disable standby and hibernation.
  }
  function DSiDisableStandby: boolean;

    function ExtractSchemeNumber(queryOut: TStringList; var schemeNum: integer): boolean;
    var
      ln  : string;
      pNum: integer;
    begin
      //c:\> powercfg /query
      //
      //Field Description          Value
      //-----------------          -----
      //Name                       Home/Office Desk
      //Numerical ID               0
      //...
      Result := false;
      while (queryOut.Count > 0) and (Copy(queryOut[0], 1, 1) <> '-') do
        queryOut.Delete(0);
      if queryOut.Count >= 2 then begin
        ln := queryOut[2];
        pNum := Length(ln);
        while (pNum > 0) and (ln[pNum] >= '0') and (ln[pNum] <= '9') do
          Dec(pNum);
        Inc(pNum);
        Result := TryStrToInt(Copy(ln, pNum, Length(ln)), schemeNum);
      end;
    end; { ExtractSchemeNumber }

  var
    execExit : cardinal;
    execOut  : TStringList;
    execRes  : cardinal;
    schemeNum: integer;
    winVer   : TDSiWindowsVersion;
  begin
    Result := false;
    SetLastError(ERROR_NOT_SUPPORTED);
    execOut := TStringList.Create;
    try
      winVer := DSiGetTrueWindowsVersion;
      if winVer >= wvWinXP then begin
        if winVer < wvWinVista then begin
          execRes := DSiExecuteAndCapture('powercfg.exe /query', execOut, '', execExit);
          if (execRes = 0) or (not ExtractSchemeNumber(execOut, schemeNum)) then
            Exit;
          Result := (0 = DSiExecute('powercfg.exe /numerical /change ' + IntToStr(schemeNum) +
                           ' /standby-timeout-ac 0 /standby-timeout-dc 0' +
                           ' /hibernate-timeout-ac 0 /hibernate-timeout-dc 0', SW_HIDE, '', true));
          if Result then
            Result := (0 = DSiExecute('powercfg /hibernate off', SW_HIDE, '', true));
        end
        else begin
          Result := (0 = DSiExecute('powercfg.exe -change -standby-timeout-ac 0', SW_HIDE, '', true));
          if Result then
            Result := (0 = DSiExecute('powercfg.exe -change -standby-timeout-dc 0', SW_HIDE, '', true));
          if Result then
            Result := (0 = DSiExecute('powercfg.exe -change -hibernate-timeout-ac 0', SW_HIDE, '', true));
          if Result then
            Result := (0 = DSiExecute('powercfg.exe -change -hibernate-timeout-dc 0', SW_HIDE, '', true));
          if Result then // -hibernate will fail when run without admin privileges
            DSiExecute('powercfg -hibernate off', SW_HIDE, '', true);
        end;
      end;
    finally execOut.Free; end;
  end; { DSiDisableStandby }

  {:Disables 'X' button and Alt+F4.
    @author  aoven
    @since   2003-09-02
  }
  procedure DSiDisableX(hwnd: THandle);
  begin
    EnableMenuItem(GetSystemMenu(hwnd, false), SC_CLOSE, MF_BYCOMMAND or MF_DISABLED);
  end; { DSiDisableX }

  {:Enables 'X' button and Alt+F4.
    @author  gabr
    @since   2003-09-02
  }
  procedure DSiEnableX(hwnd: THandle);
  begin
    EnableMenuItem(GetSystemMenu(hwnd, false), SC_CLOSE, MF_BYCOMMAND or MF_ENABLED);
  end; { DSiEnableX }

const
  EWX_LOGOFF_FORCE    = $00;
  EWX_POWEROFF_FORCE  = $0A;
  EWX_REBOOT_FORCE    = $06;
  EWX_SHUTDOWN_FORCE  = $05;

  CExitWindows: array [TDSiExitWindows] of integer = (EWX_LOGOFF, EWX_LOGOFF_FORCE,
    EWX_POWEROFF, EWX_POWEROFF_FORCE, EWX_REBOOT, EWX_REBOOT_FORCE, EWX_SHUTDOWN,
    EWX_SHUTDOWN_FORCE);

  {:Exits (logoff, shutdown, restart) the Windows.
    @author  xtreme
    @since   2005-02-13
  }        
  function DSiExitWindows(exitType: TDSiExitWindows): boolean;
  begin
    Result := false;
    if DSiEnablePrivilege('SeShutdownPrivilege') then
      Result := ExitWindowsEx(CExitWindows[exitType], 0);
  end; { DSiExitWindows }

  {gp}
  function DSiForceForegroundWindow(hwnd: THandle; restoreFirst: boolean): boolean;
  var
    ForegroundThreadID: DWORD;
    ThisThreadID      : DWORD;
    timeout           : DWORD;
  begin
    if restoreFirst then
      if IsIconic(hwnd) then ShowWindow(hwnd, SW_RESTORE);

    if GetForegroundWindow = hwnd then Result := true
    else begin

      // Windows 98/2000 doesn't want to foreground a window when some other
      // window has keyboard focus

      if ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 4)) or
        ((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
        ((Win32MajorVersion > 4) or ((Win32MajorVersion = 4) and (Win32MinorVersion > 0)))) then
      begin

        // Code from Karl E. Peterson, www.mvps.org/vb/sample.htm
        // Converted to Delphi by Ray Lischner
        // Published in The Delphi Magazine 55, page 16

        Result := false;
        ForegroundThreadID := GetWindowThreadProcessID(GetForegroundWindow,nil);
        ThisThreadID := GetWindowThreadPRocessId(hwnd,nil);
        if AttachThreadInput(ThisThreadID, ForegroundThreadID, true) then begin
          BringWindowToTop(hwnd); //IE 5.5 - related hack
          SetForegroundWindow(hwnd);
          AttachThreadInput(ThisThreadID, ForegroundThreadID, false);
          Result := (GetForegroundWindow = hwnd);
        end;
        if not Result then begin

          // Code by Daniel P. Stasinski <dannys@karemor.com>

          SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, @timeout, 0);
          SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(0), SPIF_SENDCHANGE);
          BringWindowToTop(hwnd); //IE 5.5 - related hack
          SetForegroundWindow(hWnd);
          SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(timeout), SPIF_SENDCHANGE);
        end;
      end
      else begin
        BringWindowToTop(hwnd); //IE 5.5 - related hack
        SetForegroundWindow(hwnd);
      end;

      Result := (GetForegroundWindow = hwnd);
    end;
  end; { DSiForceForegroundWindow }

  {gp}
  function DSiGetClassName(hwnd: THandle): string;
  var
    winClass: array [0..1024] of char;
  begin
    if GetClassName(hwnd, winClass, SizeOf(winClass)) <> 0 then
      Result := winClass
    else
      Result := '';
  end; { DSiGetClassName }

type
  TProcWndInfo = record
    TargetProcessID: DWORD;
    FoundWindow    : HWND;    
  end; { TProcWndInfo }
  PProcWndInfo = ^TProcWndInfo;

  function EnumGetProcessWindow(wnd: HWND; userParam: LPARAM): BOOL; stdcall;
  var
    wndProcessID: DWORD;
  begin
    GetWindowThreadProcessId(wnd, @wndProcessID);
    if (wndProcessID = PProcWndInfo(userParam)^.TargetProcessID) and
       (GetWindowLong(wnd, GWL_HWNDPARENT) = 0) then
    begin
      PProcWndInfo(userParam)^.FoundWindow := Wnd;
      Result := false;
    end
    else
      Result := true;
  end; { EnumGetProcessWindow }

  {ln}
  function DSiGetProcessWindow(targetProcessID: cardinal): HWND;
  var
    procWndInfo: TProcWndInfo;
  begin
    procWndInfo.TargetProcessID := targetProcessID;
    procWndInfo.FoundWindow := 0;
    EnumWindows(@EnumGetProcessWindow, LPARAM(@procWndInfo));
    Result := procWndInfo.FoundWindow;
  end; { DSiGetProcessWindow }

  {gp}
  function DSiGetWindowText(hwnd: THandle): string;
  var
    winText: array [0..1024] of char;
  begin
    if GetWindowText(hwnd, winText, SizeOf(winText)) <> 0 then
      Result := winText
    else
      Result := '';
  end; { DSiGetWindowTextStr }

  {:Processes all waiting window messages.
    @author  gabr
    @since   2003-08-18
  }
  procedure DSiProcessMessages(hwnd: THandle; waitForWMQuit: boolean);
  var
    bGet: longint;
    msg : TMsg;
  begin
    if hwnd = 0 then
      Exit;
    repeat
      if not waitForWMQuit then begin
        if not PeekMessage(msg, hwnd, 0, 0, PM_REMOVE) then
          break; //repeat
      end
      else begin
        bGet := longint(GetMessage(msg, hwnd, 0, 0));
        if (bGet = 0) or (bGet = -1) then
          break; //repeat
      end;
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    until msg.Message = WM_QUIT;
  end; { DSiProcessMessages }

  {xtreme}
  procedure DSiRebuildDesktopIcons;
  var
    dwResult: cardinal;
    oldSize : integer;
    registry: TRegistry;
  const
    CShellIconSize = 'Shell Icon Size';
    CTimeout = 10000;
  begin
    registry := TRegistry.Create;
    try
      if not registry.OpenKey('Control Panel\Desktop\WindowMetrics', false) then
        Exit;
      if registry.ValueExists(CShellIconSize) then
       oldSize := StrToInt(registry.ReadString(CShellIconSize))
      else
       oldSize := 0;
      registry.WriteString(CShellIconSize, IntToStr(oldSize + 1));
      SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE,
        SPI_SETNONCLIENTMETRICS, 0, SMTO_ABORTIFHUNG, CTimeout,
        {$IF CompilerVersion >= 23}@{$IFEND}dwResult);
      if oldSize > 0 then
        registry.WriteString(CShellIconSize, IntToStr(oldSize))
      else
        registry.DeleteValue(CShellIconSize);
      SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE,
        SPI_SETNONCLIENTMETRICS,0, SMTO_ABORTIFHUNG, CTimeout,
        {$IF CompilerVersion >= 23}@{$IFEND}dwResult);
      registry.CloseKey;
    finally FreeAndNil(registry);end;
  end; { DSiRebuildDesktopIcons }

  {xtreme}
  procedure DSiRefreshDesktop;
  var
    handleDesktop: HWND;
  begin
    handleDesktop := FindWindowEx(FindWindowEx(FindWindow('Progman',
      'Program Manager'), 0, 'SHELLDLL_DefView', ''), 0, 'SysListView32', '');
    PostMessage(handleDesktop, WM_DDE_FIRST, VK_F5, 0);
    PostMessage(handleDesktop, WM_DDE_LAST, VK_F5, 1 shl 31);
  end; { DSiRefreshDesktop }

  {ln}
  procedure DSiSetTopMost(hwnd: THandle; onTop, activate: boolean);
  const
    cTopMost : array [boolean] of THandle = (HWND_NOTOPMOST, HWND_TOPMOST);
    cActivate: array [boolean] of UINT    = (SWP_NOACTIVATE, 0);
  begin
    SetWindowPos(hwnd, cTopMost[OnTop], 0, 0, 0, 0,
      SWP_NOMOVE or SWP_NOSIZE or cActivate[Activate]);
  end; { DSiSetTopMost }

{ Aero }

  ///<summary>Disables Aero interface on Vista. Does nothing on older platforms.</summary>
  ///<returns>True if Aero was disabled or if Aero is not supported.</returns>
  ///<author>gabr</author>
  ///<remarks>Based on code by David J Taylor published in
  ///    news://borland.public.delphi.language.delphi.win32.</remarks>
  ///<since>2007-10-25</since>
  function DSiAeroDisable: boolean;
  begin
    Result := (DSiDwmEnableComposition(DWM_EC_DISABLECOMPOSITION) = S_OK);
    if not assigned(GDwmEnableComposition) then
      Result := true;
  end; { DSiAeroDisable }

  ///<summary>Enables Aero interface on Vista. Does nothing on older platforms.</summary>
  ///<returns>True if Aero was enabled. False if Aero is not supported.</returns>
  ///<author>gabr</author>
  ///<remarks>Based on code by David J Taylor published in
  ///    news://borland.public.delphi.language.delphi.win32.</remarks>
  ///<since>2007-10-25</since>
  function DSiAeroEnable: boolean;
  begin
    Result := (DSiDwmEnableComposition(DWM_EC_ENABLECOMPOSITION) = S_OK);
  end; { DSiAeroEnable }

  ///<summary>Checks if Aero interface is enabled.</summary>
  ///<returns>True if Aero is enabled. False if Aero is disabled or not supported.</returns>
  ///<author>gabr</author>
  ///<since>2007-10-25</since>
  function DSiAeroIsEnabled: boolean;
  var
    isEnabled: BOOL;
  begin
    Result := (DSiDwmIsCompositionEnabled(isEnabled) = S_OK);
    if Result then
      Result := isEnabled;
  end; { DSiAeroIsEnabled }

{ Taskbar }

  {ln}
  function DSiGetTaskBarPosition: integer;
  var
    pData: TAppBarData;
  begin
    Result := -1;
    pData.cbSize := SizeOf(TAppBarData);
    pData.hWnd := 0;
    if SHAppBarMessage(ABM_GETTASKBARPOS, pData) = 0 then
      Exit;
    Result := pData.uEdge;
  end; { DSiGetTaskBarPosition }

{ Menus }

  {gp}
  function DSiGetHotkey(const item: string): char;
  var
    item2: string;
    p    : integer;
  begin
    item2 := StringReplace(item, '&&', '&', [rfReplaceAll]);
    p := Pos('&', item2);
    if (p > 0) and (p < Length(item2)) then
      Result := UpCase(item2[p+1])
    else
      Result := #0;
  end; { DSiGetHotkey }

  {gp}
  function DSiGetMenuItem(menu: HMENU; item: integer): string;
  var
    res: integer;
    buf: array [0..1024] of char;
  begin
    res := GetMenuString(menu, item, buf, SizeOf(buf), MF_BYPOSITION);
    if res > 0 then
      Result := buf
    else
      Result := '';
  end; { DSiGetMenuItem }

{ Screen }

  {mr}
  procedure DSiDisableScreenSaver(out currentlyActive: boolean);
  var
    isActive: BOOL;
  begin
    SystemParametersInfo(SPI_GETSCREENSAVEACTIVE, 0, @isActive, 0);
    currentlyActive := isActive;
    if currentlyActive then
      SystemParametersInfo(SPI_SETSCREENSAVEACTIVE, UINT(false), nil, SPIF_SENDWININICHANGE);
  end; { DSiDisableScreenSaver }

  {mr}
  procedure DSiEnableScreenSaver;
  begin
    SystemParametersInfo(SPI_SETSCREENSAVEACTIVE, UINT(true), nil, SPIF_SENDWININICHANGE);
  end; { DSiEnableScreenSaver }

  {mr}
  function DSiGetBitsPerPixel: integer;
  var
    h: hDC;
  begin
    h := GetDC(0);
    try
      Result := GetDeviceCaps(h, BITSPIXEL);
    finally ReleaseDC(0, h); end;
  end; { DSiGetBitsPerPixel }

  {mr}
  function DSiGetBPP: integer;
  var
    DC: HDC;
  begin
    DC := GetDC(0);
    try
      Result := GetDeviceCaps(DC, BITSPIXEL) * GetDeviceCaps(DC, PLANES);
    finally ReleaseDC(0, DC); end;
  end; { DSiGetBPP }

  {mr}
  function DSiGetDesktopSize: TRect;
  begin
    SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0);
  end; { DSiGetDesktopSize }

  {ln}
  function DSiIsFullScreen: boolean;
  var
    desktopRect   : TRect;
    foregroundRect: TRect;
  begin
    Result :=
      GetWindowRect(GetForegroundWindow, foregroundRect)  and
      GetWindowRect(GetDesktopWindow, desktopRect)        and
      (not PtInRect(desktopRect, foregroundRect.TopLeft)) and
      (not PtInRect(desktopRect, foregroundRect.BottomRight));
  end; { DSiIsFullScreen }

  {ales}
  procedure DSiMonitorOff;
  begin
    SendMessage(GetForegroundWindow, WM_SYSCOMMAND, SC_MONITORPOWER, 2);
  end; { DSiMonitorOff }

  {ales}
  procedure DSiMonitorOn;
  begin
    SendMessage(GetForegroundWindow, WM_SYSCOMMAND, SC_MONITORPOWER, -1);
  end; { DSiMonitorOn }

  {gp}
  procedure DSiMonitorStandby;
  begin
    SendMessage(GetForegroundWindow, WM_SYSCOMMAND, SC_MONITORPOWER, 1);
  end; { DSiMonitorOff }

  {matijap}
  function DSiSetScreenResolution(width, height: integer): longint;
  var
    deviceMode: TDeviceMode;
  begin
    with deviceMode do begin
      dmSize := SizeOf(TDeviceMode);
      dmPelsWidth := width;
      dmPelsHeight := height;
      dmFields := DM_PELSWIDTH or DM_PELSHEIGHT;
    end;
    Result := ChangeDisplaySettings(DeviceMode, CDS_UPDATEREGISTRY);
  end; { DSiSetScreenResolution }

{ Rectangles }

  {:Centers rectangle over another rectangle.
    @since   2007-12-29
    @author  gabr
  }
  procedure DSiCenterRectInRect(const ownerRect: TRect; var clientRect: TRect);
  var
    clientCenter: TPoint;
    ownerCenter : TPoint;
  begin
    ownerCenter := CenterPoint(ownerRect);
    clientCenter := CenterPoint(clientRect);
    OffsetRect(clientRect, ownerCenter.X - clientCenter.X, ownerCenter.Y - clientCenter.Y);
  end; { DSiCenterRectInRect }

  {:Makes sure rectangle is fully placed inside another rectangle.
    @since   2007-12-29
    @author  gabr
  }
  procedure DSiMakeRectFullyVisibleOnRect(const ownerRect: TRect; var clientRect: TRect);
  begin
    if clientRect.Left < ownerRect.Left then 
      OffsetRect(clientRect, ownerRect.Left - clientRect.Left, 0)
    else if clientRect.Right > ownerRect.Right then
      OffsetRect(clientRect, ownerRect.Right - clientRect.Right, 0);
    if clientRect.Top < ownerRect.Top then
      OffsetRect(clientRect, 0, ownerRect.Top - clientRect.Top)
    else if clientRect.Bottom > ownerRect.Bottom then
      OffsetRect(clientRect, 0, ownerRect.Bottom - clientRect.Bottom);
  end; { DSiMakeRectFullyVisibleOnRect }

{ Clipboard }

var
  GCF_HTML: UINT;

  {:Checks if HTML format is stored on the clipboard.
    @since   2008-04-29
    @author  gabr
  }
  function DSiIsHtmlFormatOnClipboard: boolean;
  begin
    Result := IsClipboardFormatAvailable(GCF_HTML);
  end; { DSiIsHtmlFormatOnClipboard }

  {:Retrieves HTML format from the clipboard. If there is no HTML format on the clipboard,
    function returns empty string.
    @since   2008-04-29
    @author  MP002, gabr
  }
  function DSiGetHtmlFormatFromClipboard: string;
  var
    hClipData       : THandle;
    idxEndFragment  : integer;
    idxStartFragment: integer;
    pClipData       : PChar;
  begin
    Result := '';
    if DSiIsHtmlFormatOnClipboard then begin
      Win32Check(OpenClipboard(0));
      try
        hClipData := GetClipboardData(GCF_HTML);
        if hClipData <> 0 then begin
          pClipData := GlobalLock(hClipData);
          Win32Check(assigned(pClipData));
          try
            idxStartFragment := Pos('<!--StartFragment-->', pClipData); // len = 20
            idxEndFragment := Pos('<!--EndFragment-->', pClipData);
            if (idxStartFragment >= 0) and (idxEndFragment >= idxStartFragment) then
              Result := Copy(pClipData, idxStartFragment + 20, idxEndFragment - idxStartFragment - 20);
          finally GlobalUnlock(hClipData); end;
        end;
      finally Win32Check(CloseClipboard); end;
    end;
  end; { DSiGetHtmlFormatFromClipboard }

  {:Copies HTML (and, optionally, text) format to the clipboard.
    @since   2008-04-29
    @author  MP002, gabr
  }
  procedure DSiCopyHtmlFormatToClipboard(const sHtml, sText: string);

    function MakeFragment(const sHtml: string): string;
    const
      CVersion       = 'Version:1.0'#13#10;
      CStartHTML     = 'StartHTML:';
      CEndHTML       = 'EndHTML:';
      CStartFragment = 'StartFragment:';
      CEndFragment   = 'EndFragment:';
      CHTMLIntro     = '<sHtml><head><title>HTML clipboard</title></head><body><!--StartFragment-->';
      CHTMLExtro     = '<!--EndFragment--></body></sHtml>';
      CNumberLengthAndCR = 10;
      CDescriptionLength = // Let the compiler determine the description length.
        Length(CVersion) + Length(CStartHTML) + Length(CEndHTML) +
        Length(CStartFragment) + Length(CEndFragment) + 4*CNumberLengthAndCR;
    var
      description     : string;
      idxEndFragment  : integer;
      idxEndHtml      : integer;
      idxStartFragment: integer;
      idxStartHtml    : integer;
    begin
      // The sHtml clipboard format is defined by using byte positions in the entire block
      // where sHtml text and fragments start and end. These positions are written in a
      // description. Unfortunately the positions depend on the length of the description
      // but the description may change with varying positions. To solve this dilemma the
      // offsets are converted into fixed length strings which makes it possible to know
      // the description length in advance.
      idxStartHtml := CDescriptionLength;              // position 0 after the description
      idxStartFragment := idxStartHtml + Length(CHTMLIntro);
      idxEndFragment := idxStartFragment + Length(sHtml);
      idxEndHtml := idxEndFragment + Length(CHTMLExtro);
      description := CVersion +
        {$IFDEF ScopedUnitNames}System.{$ENDIF}SysUtils.Format('%s%.8d', [CStartHTML, idxStartHtml]) + #13#10 +
        {$IFDEF ScopedUnitNames}System.{$ENDIF}SysUtils.Format('%s%.8d', [CEndHTML, idxEndHtml]) + #13#10 +
        {$IFDEF ScopedUnitNames}System.{$ENDIF}SysUtils.Format('%s%.8d', [CStartFragment, idxStartFragment]) + #13#10 +
        {$IFDEF ScopedUnitNames}System.{$ENDIF}SysUtils.Format('%s%.8d', [CEndFragment, idxEndFragment]) + #13#10;
      Result := description + CHTMLIntro + sHtml + CHTMLExtro;
    end; { MakeFragment }

  var
    clipFormats: array[0..1] of UINT;
    clipStrings: array[0..1] of string;
    hClipData  : HGLOBAL;
    iFormats   : integer;
    pClipData  : PChar;

  begin { DSiCopyHtmlFormatToClipboard }
    Win32Check(OpenClipBoard(0));
    try
      //most descriptive first as per api docs
      clipStrings[0] := MakeFragment(sHtml);
      if sText = '' then
        clipStrings[1] := sHtml
      else
        clipStrings[1] := sText;
      clipFormats[0] := GCF_HTML;
      clipFormats[1] := CF_TEXT;
      Win32Check(EmptyClipBoard);
      for iFormats := 0 to High(clipStrings) do begin
        if clipStrings[iFormats] = '' then
          continue;
        hClipData := GlobalAlloc(GMEM_DDESHARE + GMEM_MOVEABLE, Length(clipStrings[iFormats]) + 1);
        Win32Check(hClipData <> 0);
        try
          pClipData := GlobalLock(hClipData);
          Win32Check(assigned(pClipData));
          try
            Move(PChar(clipStrings[iFormats])^, pClipData^, Length(clipStrings[iFormats]) + 1);
          finally GlobalUnlock(hClipData); end;
          Win32Check(SetClipboardData(clipFormats[iFormats], hClipData) <> 0);
          hClipData := 0;
        finally
          if hClipData <> 0 then
            GlobalFree(hClipData);
        end;
      end;
    finally Win32Check(CloseClipboard); end;
  end; { DSiCopyHtmlFormatToClipboard }

{ Information }

  {:Retrieves application compatibility flags. Works around WOW64 problems.
    @since   2007-02-11
    @author  Miha-R, gabr
  }
  function DSiGetAppCompatFlags(const exeName: string): string;
  var
    wow64key: longword;
  begin
    Result := DSiReadRegistry(
      'Software\Microsoft\Windows NT\CurrentVersion\AppCompatFlags\Layers',
      exeName, '', HKEY_CURRENT_USER);
    if DSiIsWow64 then
      wow64key := KEY_WOW64_64KEY
    else
      wow64key := 0;
    Result := Result + ' ' + DSiReadRegistry(
      'Software\Microsoft\Windows NT\CurrentVersion\AppCompatFlags\Layers',
      exeName, '', HKEY_LOCAL_MACHINE, KEY_QUERY_VALUE OR wow64key);
    Result := Trim(Result);
  end; { DSiGetAppCompatFlags }

  {:Returns type of a last boot.
    @author  ales, gabr
    @since   2003-09-02
  }
  function DSiGetBootType: TDSiBootType;
  var
    iBoot: integer;
  begin
    iBoot := GetSystemMetrics(SM_CLEANBOOT);
    if iBoot = 0 then
      Result := btNormal
    else if iBoot = 1 then
      Result := btFailSafe
    else if iBoot = 2 then
      Result := btFailSafeWithNetwork
    else
      Result := btUnknown;
  end; { DSiGetBootType }
  
  {:Returns name of the licensee organisation.
    @author  Lee_Nover
    @since   2002-11-25
  }
  function DSiGetCompanyName: string;
  begin
    Result := DSiReadRegistry(DSiWinVerKeys[DSiIsWinNT], 'RegisteredOrganization', '',
      HKEY_LOCAL_MACHINE);
  end; { DSiGetCompanyName }

  {:Returns computer name.
    @author  Miha-R
    @since   2002-11-25
  }
  function DSiGetComputerName: string;
  var
    buffer    : PChar;
    bufferSize: DWORD;
  begin
    bufferSize := MAX_COMPUTERNAME_LENGTH + 1;
    GetMem(buffer, bufferSize * SizeOf(char));
    try
      GetComputerName(buffer, bufferSize);
      SetLength(Result, StrLen(buffer));
      if Result <> '' then
        Move(buffer^, Result[1], Length(Result) * SizeOf(char));
    finally FreeMem(buffer); end;
  end; { DSiGetComputerName }

  {:Returns path to default web browser.
    @author  Lee_Nover
    @since   2002-11-25
  }
  function DSiGetDefaultBrowser: string;
  begin
    Result := DSiReadRegistry('http\shell\open\command', '', '', HKEY_CLASSES_ROOT);
  end; { DSiGetDefaultBrowser }

  {:Returns DirectX version.
    @author  Lee_Nover
    @since   2002-11-25
  }
  function DSiGetDirectXVer: string;
  begin
    Result := DSiReadRegistry('\Software\Microsoft\DirectX', 'Version', '',
      HKEY_LOCAL_MACHINE);
  end; { DSiGetDirectXVer }

  {:Returns name of the operating system licensee.
    @author  Lee_Nover
    @since   2002-11-25
  }
  function DSiGetRegisteredOwner: string;
  begin
    Result := DSiReadRegistry(DSiWinVerKeys[DSiIsWinNT], 'RegisteredOwner', '',
      HKEY_LOCAL_MACHINE);
  end; { DSiGetRegisteredOwner }

  {:Returns disk label of the specified drive.
    @author  Odisej
    @since   2003-10-09
  }
  function DSiGetDiskLabel(disk: char): string;
  var
    fileSysFlags: DWORD;
    maxCompLen  : DWORD;
    volName     : array [0..MAX_PATH] of char;
  begin
    if GetVolumeInformation(PChar(disk+':\'), volName, SizeOf(volName)-1,
         nil, maxCompLen, fileSysFlags, nil, 0)
    then
      Result := volName
    else
      Result := '';
  end; { DSiGetDiskLabel }

  {:Returns serial number of the specified drive.
    @author  ales
    @since   2002-11-25
  }
  function DSiGetDiskSerial(disk: char): string;
  var
    fileSysFlags: DWORD;
    maxCompLen  : DWORD;
    serNum      : DWORD;
  begin
    GetVolumeInformation(PChar(disk+':\'), nil, 0, @serNum,
      maxCompLen, fileSysFlags, nil, 0);
    Result := Format('%.4x-%.4x', [HiWord(serNum), LoWord(serNum)]);
  end; { DSiGetDiskSerial }

  {:Helper function returning current domain on an NT system.
    @author  Lee_Nover
    @since   2003-09-02
  }
  function GetDomainNT: string;
  var
    pwi: PWkstaInfo100;
  begin
    if DSiNetWkstaGetInfo(nil, 100, pointer(pwi)) = 0 then
      Result := string(pwi.wki100_langroup)
    else
      Result := '';
  end; { GetDomainNT }

  {:Returns the domain system is logged onto.
    @author  Lee_Nover
    @since   2003-09-02
  }        
  function DSiGetDomain: string;
  begin
    if DSiIsWinNT then
      Result := GetDomainNT
   else begin
     Result := DSiReadRegistry(
       '\System\CurrentControlSet\Services\MSNP32\NetworkProvider',
       'AuthenticatingAgent', '', HKEY_LOCAL_MACHINE);
     if Result = '' then // 9x
       Result := DSiReadRegistry(
         '\System\CurrentControlSet\Services\VXD\VNETSUP',
         'Workgroup', '', HKEY_LOCAL_MACHINE);
    end;
  end; { DSiGetDomain }

  {:Returns value of an environment variable.
    @author  gabr
    @since   2005-06-06
  }
  function DSiGetEnvironmentVariable(const envVarName: string): string;
  var
    bufSize: integer;
  begin
    Result := '';
    bufSize := GetEnvironmentVariable(PChar(envVarName), nil, 0);
    if bufSize <> 0 then begin
      SetLength(Result, bufSize-1);
      if GetEnvironmentVariable(PChar(envVarName), PChar(Result), bufSize) = 0 then
        Result := '';
    end;
  end; { DSiGetEnvironmentVariable }

  {:Returns location of a special folder.
    @author  Miha-R
    @since   2002-11-25
  }
  function DSiGetFolderLocation(const CSIDL: integer): string;
  var
    path : PChar;
    pPIDL: PItemIDList;
  begin
    GetMem(path, MAX_PATH * SizeOf(char));
    try
      if Succeeded(SHGetSpecialFolderLocation(0, CSIDL, pPIDL)) then begin
        SHGetPathFromIDList(pPIDL, path);
        DSiFreePidl(pPIDL);
      end
      else
        StrCopy(path, '');
      Result := string(path);
    finally FreeMem(path); end;
  end; { DSiGetFolderLocation }

  {:Returns list of available keyboard layouts. Objects[] property contains pointer to
    locale data, returned by the GetLocaleInfo. 
    @since   2005-02-13
  }        
  procedure DSiGetKeyboardLayouts(layouts: TStrings);
  var
    iLayout   : integer;
    keyLayList: array [0..9] of HKL;
    keyLayouts: array [0..255] of char;
  begin
    layouts.Clear;
    for iLayout := 0 to GetKeyboardLayoutList(SizeOf(keyLayList), keyLayList) - 1 do begin
      GetLocaleInfo(LoWord(keyLayList[iLayout]), LOCALE_SLANGUAGE, keyLayouts,
        SizeOf(keyLayouts));
      layouts.AddObject(keyLayouts, pointer(keyLayList[iLayout]));
    end;
  end; { DSiGetKeyboardLayouts }

  {:Returns My Documents folder.
    @author  xtreme
    @since   2003-10-09
  }
  function DSiGetMyDocumentsFolder: string;
  begin
    Result := DSiReadRegistry(
      '\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders',
      'Personal', '');
  end; { DSiGetMyDocumentsFolder }

  {:Returns program files folder (i.e. C:\Program Files or similar).
    @author  gabr
    @since   2002-11-25
  }
  function DSiGetProgramFilesFolder: string;
  begin
    Result := DSiReadRegistry('\Software\Microsoft\Windows\CurrentVersion',
      'ProgramFilesDir', '', HKEY_LOCAL_MACHINE);
  end; { DSiGetProgramFilesFolder }

  {:Returns system folder (i.e. C:\Windows\System32 or similar).
    @author  Miha-R
    @since   2002-11-25
  }
  function DSiGetSystemFolder: string;
  var
    path: array [1..MAX_PATH] of char;
  begin
    if GetSystemDirectory(@path, MAX_PATH) <> 0 then
      Result := StrPas(PChar(@path))
    else
      Result := '';
  end; { DSiGetSystemDirectory }

  {:Returns system default language formatted as a string.
    @author  xtreme
    @since   2005-02-13
  }
  function DSiGetSystemLanguage: string;
  var
    lngID  : LANGID;
    lngName: array [0..127] of char;
  begin
    lngID := GetSystemDefaultLangID;
    VerLanguageName(lngID, lngName, 127);
    Result := lngName;
  end; { DSiGetSystemLanguage }

  {:Returns extended information on operating system version (service pack level etc).
    @author  xtreme
    @since   2003-10-09
  }        
  function DSiGetSystemVersion: string;
  var
    versionInfo: TOSVersionInfo;
  begin
    try
      versionInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
      GetVersionEx(versionInfo);
    finally
      Result := versionInfo.szCSDVersion;
    end;
  end; { DSiGetSystemVersion }

  {:Tries to return a true OS version when application has compatibility flags set.
    @since   2007-02-11
    @author  Miha-R, gabr
  }
  function DSiGetTrueWindowsVersion: TDSiWindowsVersion;

    function ExportsAPI(module: HMODULE; const apiName: string): boolean;
    begin
      Result := GetProcAddress(module, PChar(apiName)) <> nil;
    end; { ExportsAPI }

  var
    hKernel32: HMODULE;

  begin { DSiGetTrueWindowsVersion }
    hKernel32 := GetModuleHandle('kernel32');
    Win32Check(hKernel32 <> 0);
    if ExportsAPI(hKernel32, 'CreateRemoteThreadEx') then
      Result := wvWin7OrServer2008R2
    else if ExportsAPI(hKernel32, 'AddSecureMemoryCacheCallback') then
      Result := wvWinServer2008OrVistaSP1
    else if ExportsAPI(hKernel32, 'GetLocaleInfoEx') then
      Result := wvWinVista
    else if ExportsAPI(hKernel32, 'GetLargePageMinimum') then
      Result := wvWinServer2003
//    else if ExportsAPI(hKernel32, 'GetDLLDirectory') then
//      Result := wvWinXPSP1
    else if ExportsAPI(hKernel32, 'GetNativeSystemInfo') then
      Result := wvWinXP
    else if ExportsAPI(hKernel32, 'ReplaceFile') then
      Result := wvWin2000
    else if ExportsAPI(hKernel32, 'OpenThread') then
      Result := wvWinME
    else if ExportsAPI(hKernel32, 'GetThreadPriorityBoost') then
      Result := wvWinNT4
    else if ExportsAPI(hKernel32, 'IsDebuggerPresent') then  // is also in NT4!
      Result := wvWin98
    else if ExportsAPI(hKernel32, 'GetDiskFreeSpaceEx') then  // is also in NT4!
      Result := wvWin95OSR2
    else if ExportsAPI(hKernel32, 'ConnectNamedPipe') then
      Result := wvWinNT3
    else if ExportsAPI(hKernel32, 'Beep') then
      Result := wvWin95
    else // we have no idea
      Result := DSiGetWindowsVersion;
  end; { DSiGetTrueWindowsVersion }

  {:Returns user name of the current thread.
    @author  Miha-R, Lee_Nover
    @since   2002-11-25
  }
  function DSiGetUserName: string;
  var
    buffer    : PChar;
    bufferSize: DWORD;
  begin
    bufferSize := 256; //UNLEN from lmcons.h
    buffer := AllocMem(bufferSize * SizeOf(char));
    try
      GetUserName(buffer, bufferSize);
      Result := string(buffer);
    finally FreeMem(buffer, bufferSize); end;
  end; { DSiGetUserName }

  {:Returns name of the user owning the desktop (currently logged user).
    @author  Lee_Nover
    @since   2003-09-03
  }        
  function DSiGetUserNameEx: string;
  var
    dwProcessId: DWORD;
    h          : HWND;
    hProcess   : THandle;
    hToken     : THandle;
  begin
    Result := '';
    h := FindWindow('Progman', nil);// maybe use GetDesktopWindow
    if h = 0 then
      Exit;
    if GetWindowThreadProcessId(h, @dwProcessId) = 0 then
      Exit;
    hProcess := OpenProcess(PROCESS_ALL_ACCESS, FALSE, dwProcessId);
    if hProcess = 0 then
      Exit;
    try
      if OpenProcessToken(hProcess, TOKEN_ALL_ACCESS, hToken) then
      try
        ImpersonateLoggedOnUser(hToken);
        try
          Result := DSiGetUserName;
        finally RevertToSelf; end;
      finally CloseHandle(hToken); end;
    finally CloseHandle(hProcess); end;
  end; { TDSiRegistry.DSiGetUserNameEx }

  {:Returns Windows folder (i.e. C:\Windows or similar).
    @author  Miha-R
    @since   2002-11-25
  }
  function DSiGetWindowsFolder: string;
  var
    path: PChar;
  begin
    GetMem(path, MAX_PATH * SizeOf(char));
    try
      if GetWindowsDirectory(path, MAX_PATH * SizeOf(char)) <> 0 then
        Result := StrPas(path)
      else
        Result := '';
    finally FreeMem(path); end;
  end; { DSiGetWindowsFolder }

  {:Returns detailed Windows version.
    @author  xtreme, ales
    @since   2003-10-09
  }
  function DSiGetWindowsVersion: TDSiWindowsVersion;
  var
    versionInfo      : TOSVersionInfo;
    versionInfoEx    : TOSVersionInfoEx;
    versionInfoExFake: TOSVersionInfo absolute versionInfoEx;
  begin
    versionInfo.dwOSVersionInfoSize := SizeOf(versionInfo);
    GetVersionEx(versionInfo);
    Result := wvUnknown;
    case versionInfo.dwPlatformID of
      VER_PLATFORM_WIN32s: Result := wvWin31;
      VER_PLATFORM_WIN32_WINDOWS:
        case versionInfo.dwMinorVersion of
          0:
            if Trim(versionInfo.szCSDVersion[1]) = 'B' then
              Result := wvWin95OSR2
            else
              Result := wvWin95;
          10:
            if Trim(versionInfo.szCSDVersion[1]) = 'A' then
              Result := wvWin98SE
            else
              Result := wvWin98;
          90:
            if (versionInfo.dwBuildNumber = 73010104) then
               Result := wvWinME;
             else
               Result := wvWin9x;
        end; //case versionInfo.dwMinorVersion
      VER_PLATFORM_WIN32_NT:
        case versionInfo.dwMajorVersion of
          3: Result := wvWinNT3;
          4: Result := wvWinNT4;
          5:
            case versionInfo.dwMinorVersion of
              0: Result := wvWin2000;
              1: Result := wvWinXP;
              2: Result := wvWinServer2003;
              else Result := wvWinNT
            end; //case versionInfo.dwMinorVersion
          6: begin
            versionInfoEx.dwOSVersionInfoSize := SizeOf(versionInfoEx);
            GetVersionEx(versionInfoExFake);
            if versionInfoEx.wProductType = VER_NT_WORKSTATION then
              if versionInfoEx.dwMinorVersion = 0 then
                Result := wvWinVista
              else
                Result := wvWin7
            else
              if versionInfoEx.dwMinorVersion = 0 then
                Result := wvWinServer2008
              else
                Result := wvWinServer2008R2;
          end;
        end; //case versionInfo.dwMajorVersion
      end; //versionInfo.dwPlatformID
  end; { DSiGetWindowsVersion }

  {:Initializes font to the metrics of a specific GUI element.
    @author  aoven
    @since   2007-11-13
  }
  function DSiInitFontToSystemDefault(aFont: TFont; aElement: TDSiUIElement): boolean;
  var
    NCM: TNonClientMetrics;
    PLF: PLogFont;
  begin
    Result := false;
    NCM.cbSize := SizeOf(TNonClientMetrics);
    if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NCM, 0) then begin
      case aElement of
        ueMenu:          PLF := @NCM.lfMenuFont;
        ueMessage:       PLF := @NCM.lfMessageFont;
        ueWindowCaption: PLF := @NCM.lfCaptionFont;
        ueStatus:        PLF := @NCM.lfStatusFont;
        else raise Exception.Create('Unexpected GUI element');
      end;
      aFont.Handle := CreateFontIndirect(PLF^);
      Result := true;
    end;
  end; { DSiInitFontToSystemDefault }

  {:Returns True if the application is running with admin privileges.
    Always returns True on Windows 95/98.
    Based on http://www.gumpi.com/Blog/2007/10/02/EKON11PromisedEntry3.aspx.
    @author  gabr
    @since   2002-11-25
  }
  function DSiIsAdmin: boolean;
  var
    accessToken   : THandle;
    administrators: PSID;
    groups        : PTokenGroups;
    iGroup        : integer;
    infoBufferSize: DWORD;
    success       : BOOL;
  begin
    if not DSiIsWinNT then
      Result := true
    else begin
      Result := false;
      success := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, accessToken);
      if not success then
        if GetLastError = ERROR_NO_TOKEN then
          success := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, accessToken);
      if success then begin
        if GetTokenInformation(accessToken, TokenGroups, nil, 0, infoBufferSize) or
           (GetLastError <> ERROR_INSUFFICIENT_BUFFER)
        then
          RaiseLastOSError;
        GetMem(groups, infoBufferSize);
        success := GetTokenInformation(accessToken, TokenGroups, groups, infoBufferSize, infoBufferSize);
        CloseHandle(accessToken);
        if success then begin
          AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, administrators);
          {$R-}
          for iGroup := 0 to groups.GroupCount - 1 do begin
            if EqualSid(administrators, groups.Groups[iGroup].Sid) then begin
              Result := true;
              break; //for iGroup
            end;
          end; //for iGroup
          {$IFDEF RestoreR}{$R+}{$ENDIF}
          FreeSid(administrators);
        end;
        FreeMem(groups);
      end;
    end;
  end; { DSiIsAdmin }

  {:Returns True if an administrator is logged onto the system. Always returns True on
    Windows 95/98.
    @author  Miha-R, gabr
    @since   2002-11-25
  }
  function DSiIsAdminLoggedOn: boolean;
  var
    hSC: SC_HANDLE;
  begin
    if not DSiIsWinNT then
      Result := true
    else begin
      // try an admin privileged API
      hSC := DSiOpenSCManager(nil, nil,
        GENERIC_READ or GENERIC_WRITE or GENERIC_EXECUTE);
      Result := (hSC <> 0);
      if Result then
        DSiCloseServiceHandle(hSC);
    end;
  end; { DSiIsAdminLoggedOn }

  {:Checks if disk is inserted in the specified drive.
    @author  Odisej
    @since   2003-10-09
  }        
  function DSiIsDiskInDrive(disk: char): boolean;
  var
    errorMode: word;
  begin
    errorMode := SetErrorMode(SEM_FailCriticalErrors);
    try
      Result := (DiskSize(Ord(disk) - Ord('A') + 1) >= 0);
    finally SetErrorMode(errorMode); end;
  end; { DSiIsDiskInDrive }

  {:Checks if program is running on an NT platform.
    @author  Lee_Nover
    @since   2002-11-25
  }        
  function DSiIsWinNT: boolean;
  begin
    Result := (Win32Platform = VER_PLATFORM_WIN32_NT);
  end; { DSiIsWinNT }

  {:WOW64 is the x86 emulator that allows 32-bit Windows-based applications to run on
    64-bit Windows. A 32-bit application can detect whether it is running under WOW64 by
    calling the IsWow64Process function.
    @author  Miha-R, gabr
    @since   2007-02-11
  }
  function DSiIsWow64: boolean;
  var
    isWow64: BOOL;
  begin
    Result := DSiIsWow64Process(GetCurrentProcess, isWow64);
    if Result then
      Result := isWow64;
  end; { DSiIsWow64 }

  {:Verifies local password.
    @author  gabr
    @since   2010-04-08
  }
  function DSiVerifyPassword(const username, password: string;
    const domain: string): boolean;
  var
    logonHandle: THandle;
  begin
    Result := DSiLogonAs(username, password, domain, logonHandle);
    if Result then
      CloseHandle(logonHandle);
  end; { DSiVerifyPassword }

{ Install }

  function UninstallRoot: HKEY;
  begin
    if DSiIsWinNT then
      Result := HKEY_CURRENT_USER
    else
      Result := HKEY_LOCAL_MACHINE;
  end; { UninstallRoot }

  {:On Vista+, calls DSiAddApplicationToFirewallExceptionListAdvanced.
    On XP, remaps parameters and calls DSiAddApplicationToFirewallExceptionListXP.
    On lesser Windows, simply returns False. 
    @author  gabr
    @since   2009-10-28
  }
  function DSiAddApplicationToFirewallExceptionList(const entryName,
    applicationFullPath: string; resolveConflict: TDSiFwResolveConflict;
    description: string; grouping: string; serviceName: string;
    protocols: TDSiFwIPProtocols; localPorts: string; profiles: TDSiFwIPProfiles): boolean;
  var
    profile    : TDSiFwIPProfile;
    versionInfo: TOSVersionInfo;
  begin
    Result := false;
    versionInfo.dwOSVersionInfoSize := SizeOf(versionInfo);
    GetVersionEx(versionInfo);
    if versionInfo.dwPlatformID = VER_PLATFORM_WIN32_NT then begin
      if (versionInfo.dwMajorVersion = 5) and (versionInfo.dwMinorVersion >= 1) then begin
        Result := true;
        for profile := Low(TDSiFwIPProfile) to High(TDSiFwIPProfile) do
          if (((fwProfileAll in profiles) and (profile <> fwProfileCurrent)) or (profile in profiles)) and
             (profile in [fwProfileCurrent, fwProfileDomain, fwProfilePrivate])
          then
            if not DSiAddApplicationToFirewallExceptionListXP(entryName,
                     applicationFullPath, resolveConflict, profile)
            then
              Result := false;
      end
      else if versionInfo.dwMajorVersion >= 6 then
        Result := DSiAddApplicationToFirewallExceptionListAdvanced(entryName,
          applicationFullPath, resolveConflict, description, grouping, serviceName,
          protocols, localPorts, profiles);
    end;
  end; { DSiAddApplicationToFirewallExceptionList }

  {:Adds application to the list of firewall exceptions, advanced style (Vista+).
    Based on MSDN example http://msdn.microsoft.com/en-us/library/aa364695(VS.85).aspx.
    For more information on parameters, check http://msdn.microsoft.com/en-us/library/aa365344(VS.85).aspx.
    CoInitialize must be called before using this function.
    @author  gabr
    @since   2009-10-28
  }
  function DSiAddApplicationToFirewallExceptionListAdvanced(const entryName,
    applicationFullPath: string; resolveConflict: TDSiFwResolveConflict;
    description: string; grouping: string; serviceName: string;
    protocols: TDSiFwIPProtocols; localPorts: string; profiles: TDSiFwIPProfiles): boolean;
  var
    fwPolicy2  : OleVariant;
    profileMask: integer;
    protocol   : TDSiFwIPProtocol;
    rule       : OleVariant;
  begin
    Result := false;
    try
      case resolveConflict of
        rcDuplicate:
          {nothing to do};
        rcOverwrite:
          DSiRemoveApplicationFromFirewallExceptionListAdvanced(entryName);
        rcSkip:
          if DSiFindApplicationInFirewallExceptionListAdvanced(entryName, rule) then begin
            Result := true;
            Exit;
          end;
        else
          raise Exception.CreateFmt('Unknown resolveConflict value %d', [Ord(resolveConflict)]);
      end;
      //http://msdn.microsoft.com/en-us/library/aa366458(VS.85).aspx
      //Windows Firewall with Advanced Security was first released with Windows Vista.
      fwPolicy2 := CreateOLEObject('HNetCfg.FwPolicy2');
      if fwProfileCurrent in profiles then
        profileMask := fwPolicy2.CurrentProfileTypes
      else if fwProfileAll in profiles then
        profileMask := NET_FW_PROFILE2_ALL
      else begin
        profileMask := 0;
        if fwProfileDomain  in profiles then profileMask := profileMask OR NET_FW_PROFILE2_DOMAIN;
        if fwProfilePrivate in profiles then profileMask := profileMask OR NET_FW_PROFILE2_PRIVATE;
        if fwProfilePublic  in profiles then profileMask := profileMask OR NET_FW_PROFILE2_PUBLIC;
      end;
      for protocol := Low(TDSiFwIPProtocol) to High(TDSiFwIPProtocol) do
        if protocol in protocols then begin
          rule := CreateOLEObject('HNetCfg.FWRule');
          rule.Name := entryName;
          rule.Description := description;
          rule.ApplicationName := applicationFullPath;
          if protocol = fwProtoTCP then
            rule.Protocol := NET_FW_IP_PROTOCOL_TCP
          else if protocol = fwProtoUDP then
            rule.Protocol := NET_FW_IP_PROTOCOL_UDP
          else
            raise Exception.Create('DSiAddApplicationToFirewallExceptionListAdvanced: ' +
                                   'Unexpected protocol!');
          rule.LocalPorts := localPorts;
          rule.Enabled := true;
          rule.Grouping := grouping;
          rule.Profiles := profileMask;
          rule.Action := NET_FW_ACTION_ALLOW;
          if serviceName <> '' then
            rule.ServiceName := serviceName;
          fwPolicy2.Rules.Add(rule);
        end;
      Result := true;
    except
      on E: EOleSysError do
        SetLastError(cardinal(E.ErrorCode));
      else
        SetLastError(ERROR_INVALID_FUNCTION);
    end;
  end; { DSiAddApplicationToFirewallExceptionListAdvanced }

  {:Adds application to the list of firewall exceptions, XP style. Based on the code at
    http://www.delphi3000.com/articles/article_5021.asp?SK=.
    MSDN: http://msdn.microsoft.com/en-us/library/aa366449(VS.85).aspx
    CoInitialize must be called before using this function.
    @author  gabr
    @since   2009-02-05
  }
  function DSiAddApplicationToFirewallExceptionListXP(const entryName,
    applicationFullPath: string; resolveConflict: TDSiFwResolveConflict;
    profile: TDSiFwIPProfile): boolean;
  var
    app      : OleVariant;
    fwMgr    : OleVariant;
    fwProfile: OleVariant;
  begin
    Result := false;
    try
      case resolveConflict of
        rcDuplicate:
          {nothing to do};
        rcOverwrite:
          DSiRemoveApplicationFromFirewallExceptionListXP(entryName, profile);
        rcSkip:
          if DSiFindApplicationInFirewallExceptionListXP(entryName, app, profile) then begin
            Result := true;
            Exit;
          end;
        else
          raise Exception.CreateFmt('Unknown resolveConflict value %d', [Ord(resolveConflict)]);
      end;
      fwMgr := CreateOLEObject('HNetCfg.FwMgr');
      if profile = fwProfileCurrent then
        fwProfile := fwMgr.LocalPolicy.CurrentProfile
      else if profile = fwProfileDomain then
        fwProfile := fwMgr.LocalPolicy.GetProfileByType(NET_FW_PROFILE_DOMAIN)
      else if profile = fwProfilePrivate {alias for 'standard'} then
        fwProfile := fwMgr.LocalPolicy.GetProfileByType(NET_FW_PROFILE_STANDARD);
      app := CreateOLEObject('HNetCfg.FwAuthorizedApplication');
      app.ProcessImageFileName := DSiGetSubstPath(applicationFullPath);
      app.Name := EntryName;
      app.Scope := NET_FW_SCOPE_ALL;
      app.IpVersion := NET_FW_IP_VERSION_ANY;
      app.Enabled :=true;
      fwProfile.AuthorizedApplications.Add(app);
      Result := true;
    except
      on E: EOleSysError do
        SetLastError(cardinal(E.ErrorCode));
      else
        SetLastError(ERROR_INVALID_FUNCTION);
    end;
  end; { DSiAddApplicationToFirewallExceptionListXP }

  {:Adds application to the list of firewall exceptions. Based on the code at
    http://www.delphi3000.com/articles/article_5021.asp?SK=.
    CoInitialize must be called before using this function.
    @author  gabr
    @since   2009-02-05
  }
  function DSiAddPortToFirewallExceptionList(const entryName: string;
    portNumber: cardinal): boolean;
  var
    fwMgr  : OleVariant;
    port   : OleVariant;
    profile: OleVariant;
  begin
    Result := false;
    try
      fwMgr := CreateOLEObject('HNetCfg.FwMgr');
      profile := fwMgr.LocalPolicy.CurrentProfile;
      port := CreateOLEObject('HNetCfg.FWOpenPort');
      port.Name := EntryName;
      port.Protocol := NET_FW_IP_PROTOCOL_TCP;
      port.Port := PortNumber;
      port.Scope := NET_FW_SCOPE_ALL;
      port.Enabled := true;
      profile.GloballyOpenPorts.Add(port);
      Result := true;
    except
      on E: EOleSysError do 
        SetLastError(cardinal(E.ErrorCode));
    end;
  end; { DSiAddPortToFirewallExceptionList }
  
  {gp}
  function DSiAddUninstallInfo(const displayName, uninstallCommand, publisher,
    URLInfoAbout, displayVersion, helpLink, URLUpdateInfo: string): boolean;
  begin
    Result := false;
    with TRegistry.Create do try
      RootKey := UninstallRoot;
      if OpenKey('\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\'+displayName, true) then
      try
        WriteString('DisplayName', displayName);
        WriteString('UninstallString', uninstallCommand);
        if publisher <> '' then
          WriteString('Publisher', publisher);
        if URLInfoAbout <> '' then
          WriteString('URLInfoAbout', URLInfoAbout);
        if displayVersion <> '' then
          WriteString('DisplayVersion', displayVersion);
        if helpLink <> '' then
          WriteString('HelpLink', helpLink);
        if URLUpdateInfo <> '' then
          WriteString('URLUpdateInfo', URLUpdateInfo);
        Result := true;
      finally CloseKey; end;
    finally {TRegistry.}Free; end;
  end; { DSiAddUninstallInfo }

  {ln}
  function DSiAutoRunApp(const applicationName, applicationPath: string;
    enabled: boolean): boolean;
  begin
    Result := false;
    with TRegistry.Create do try
      RootKey := HKEY_CURRENT_USER;
      if OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run', true) then try
        if enabled then
          WriteString(applicationName, applicationPath)
        else
          DeleteValue(applicationname);
        Result:=true;
      finally CloseKey; end;
    finally {TRegistry.}Free; end;
  end; { DSiAutoRunApp }

  {gp}
  // stolen from RXLib
  procedure DSiCreateShortcut(const fileName, displayName, parameters: string;
    folder: integer; const workDir: string);
  var
    fileDestPath: array [0..MAX_PATH] of char;
    itemIDList  : PItemIDList;
    persistFile : IPersistFile;
    shellLink   : IShellLink;
    {$IFNDEF Unicode}
    fileNameW   : array [0..MAX_PATH] of WideChar;
    {$ENDIF}
  begin
    CoInitialize(nil);
    try
      OleCheck(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_SERVER,
        IID_IShellLinkA, shellLink));
      OleCheck(shellLink.QueryInterface(IID_IPersistFile, persistFile));
      OleCheck(SHGetSpecialFolderLocation(0, folder, itemIDList));
      SHGetPathFromIDList(itemIDList, fileDestPath);
      StrCat(fileDestPath, PChar('\' + displayName + CLinkExt));
      shellLink.SetPath(PChar(fileName));
      shellLink.SetIconLocation(PChar(fileName), 0);
      shellLink.SetWorkingDirectory(PChar(workDir));
      shellLink.SetArguments(PChar(parameters));
      {$IFDEF Unicode}
        OleCheck(persistFile.Save(fileDestPath, true));
      {$ELSE}
        MultiByteToWideChar(CP_ACP, 0, fileDestPath, -1, fileNameW, MAX_PATH);
        OleCheck(persistFile.Save(fileNameW, true));
      {$ENDIF Unicode}
    finally CoUninitialize; end;
  end; { DSiCreateShortcut }

  {gp}
  function DSiDeleteShortcut(const displayName: string; folder: integer): boolean;
  var
    path: string;
  begin
    Result := false;
    path := DSiGetFolderLocation(folder);
    if path <> '' then begin
      path := path + '\' + displayName + CLinkExt;
      Result := DSiKillFile(path);
    end;
  end; { DSiDeleteShortcut }

  {:Edits shortcut info.
    @author  Fora
    @since   2006-06-20
  }
  procedure DSiEditShortcut(const lnkName, fileName, workDir, parameters: string);
  var
    persistFile: IPersistFile;
    shellLink  : IShellLink;
    {$IFNDEF Unicode}
    fileNameW  : array [0..MAX_PATH] of WideChar;
    {$ENDIF}
  begin
    CoInitialize(nil);
    try
      OleCheck(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_SERVER, IID_IShellLinkA, shellLink));
      OleCheck(shellLink.QueryInterface(IID_IPersistFile, persistFile));
      if (shellLink as IPersistFile).Load(PWideChar(WideString(lnkName)), 0) = 0 then
      begin
        shellLink.SetPath(PChar(fileName));
        shellLink.SetWorkingDirectory(PChar(workDir));
        shellLink.SetArguments(PChar(parameters));
        {$IFDEF Unicode}
          OleCheck(persistFile.Save(PChar(lnkName), true));
        {$ELSE}
          MultiByteToWideChar(CP_ACP, 0, PChar(lnkName), -1, fileNameW, MAX_PATH);
          OleCheck(persistFile.Save(fileNameW, true));
        {$ENDIF Unicode}
      end;
    finally CoUninitialize; end;
  end; { DSiEditShortcut }

  {:Finds rule in the firewall exception list, advanced style (Vista+).
    CoInitialize must be called before using this function.
    @author  gabr
    @since   2010-07-27
  }
  function DSiFindApplicationInFirewallExceptionListAdvanced(const entryName: string;
    var rule: OleVariant): boolean;
  var
    fwPolicy2: OleVariant;
    ruleEnum : IEnumVariant;
    value    : cardinal;
  begin
    Result := false;
    fwPolicy2 := CreateOLEObject('HNetCfg.FwPolicy2');
    if IUnknown(fwPolicy2.Rules._NewEnum).QueryInterface(IEnumVariant, ruleEnum) <> S_OK then
      Exit;
    while (ruleEnum.Next(1, rule, value) = S_OK) do begin
      if SameText(entryName, rule.Name) then begin
        Result := true;
        Exit;
      end;
    end;
    rule := Null;
  end; { DSiFindApplicationInFirewallExceptionListAdvanced }

  {:Finds rule in the firewall exception lis (XP).
    CoInitialize must be called before using this function.
    @author  gabr
    @since   2010-07-27
  }
  function  DSiFindApplicationInFirewallExceptionListXP(const entryName: string;
    var application: OleVariant; profile: TDSiFwIPProfile): boolean;
  var
    fwMgr      : OleVariant;
    fwProfile  : OleVariant;
    ruleEnum   : IEnumVariant;
    value      : cardinal;
  begin
    Result := false;
    fwMgr := CreateOLEObject('HNetCfg.FwMgr');
    if profile = fwProfileDomain then
      fwProfile := fwMgr.LocalPolicy.GetProfileByType(NET_FW_PROFILE_DOMAIN)
    else if profile = fwProfilePrivate {alias for 'standard'} then
      fwProfile := fwMgr.LocalPolicy.GetProfileByType(NET_FW_PROFILE_STANDARD)
    else
      fwProfile := fwMgr.LocalPolicy.CurrentProfile;
    if IUnknown(fwProfile.AuthorizedApplications._NewEnum).QueryInterface(IEnumVariant, ruleEnum) <> S_OK then
      Exit;
    while (ruleEnum.Next(1, application, value) = S_OK) do begin
      if SameText(entryName, application.Name) then begin
        Result := true;
        Exit;
      end;
    end;
    application := Null;
  end; { DSiFindApplicationInFirewallExceptionListXP }

  {$R-}
  function  DSiGetLogonSID(token: THandle; var logonSID: PSID): boolean;
  var
    dwLength: DWORD;
    iGroup  : integer;
    tkGroups: ^TOKEN_GROUPS;
  begin
    Result := false;
    dwLength := 0;
    tkGroups := nil;
    try
      if not GetTokenInformation(token, TokenGroups, tkGroups, 0, dwLength) then begin
        if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
          Exit;
        tkGroups := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, dwLength);
        if tkGroups = nil then
          Exit;
        if not GetTokenInformation(token, TokenGroups, tkGroups, dwLength, dwLength) then
          Exit;
        // Loop through the groups to find the logon SID.
        for iGroup := 0 to tkGroups.GroupCount - 1 do begin
          if (tkGroups.Groups[iGroup].Attributes AND SE_GROUP_LOGON_ID) = SE_GROUP_LOGON_ID then begin
            dwLength := GetLengthSid(tkGroups.Groups[iGroup].Sid);
            logonSID := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, dwLength);
            if logonSID = nil then
              Exit;
            if not CopySid(dwLength, logonSID, tkGroups.Groups[iGroup].Sid) then begin
              HeapFree(GetProcessHeap, 0, logonSID);
              Exit;
            end;
            break; //for iGroup
          end; //for iGroup
        end;
        Result := true;
      end;
    finally
      if assigned(tkGroups) then
        HeapFree(GetProcessHeap, 0, tkGroups);
    end;
  end; { DSiGetLogonSID }
  {$IFDEF RestoreR}{$R+}{$ENDIF}

  {:Extracts executable path and work dir from the LNK file.
    @author  Cavlji
    @since   2006-06-20
  }
  function  DSiGetShortcutInfo(const lnkName: string; var fileName, filePath, workDir,
    parameters: string): boolean;
  var
    buf      : array [0..MAX_PATH] of char;
    param    : array[0..MAX_PATH] of char;
    findData : TWin32FindData;
    shellLink: IShellLink;
  begin
    CoInitialize(nil);
    try
      Result := false;
      OleCheck(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_SERVER, IID_IShellLinkA,
        shellLink));
      if (shellLink as IPersistFile).Load(PWideChar(WideString(lnkName)), 0) = 0 then begin
        shellLink.GetPath(buf, MAX_PATH, findData, 0);
        fileName := findData.cFileName;
        filePath := buf;
        shellLink.GetWorkingDirectory(buf, MAX_PATH);
        shellLink.GetArguments(param, MAX_PATH);
        parameters := param;
        workDir := buf;
        Result := true;
      end;
    finally CoUninitialize; end;
  end; { DSiGetShortcutInfo }

  {gp}
  function DSiGetUninstallInfo(const displayName: string;
    out uninstallCommand: string): boolean;
  begin
    uninstallCommand :=
      DSiReadRegistry('\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\'+displayName,
        'UninstallString', '', UninstallRoot);
    Result := (uninstallCommand <> '');
  end; { DSiGetUninstallInfo }
  
  {ln}
  function DSiIsAutoRunApp(const applicationname: string): boolean;
  begin
    Result :=
      (DSiReadRegistry('\Software\Microsoft\Windows\CurrentVersion\Run', applicationname, '') <> '');
  end; { DSiIsAutoRunApp }

  {ln}
  function DSiRegisterActiveX(const fileName: string;
    registerDLL: boolean): HRESULT;
  type
    TDLLRegisterServer = function: HResult; stdcall;
  var
    _Register: TDllRegisterServer;
    DLLHandle: THandle;
    OldMode  : DWORD;
  begin
    Result := E_FAIL;
    OldMode := SetErrorMode (SEM_NOOPENFILEERRORBOX);
    DLLHandle := LoadLibrary(PChar(FileName));
    SetErrorMode (OldMode);
    if DLLHandle > 0 then
    try
      if RegisterDLL then
        _Register := GetProcAddress(DLLHandle, 'DllRegisterServer')
      else
        _Register := GetProcAddress(DLLHandle, 'DllUnregisterServer');
      if Assigned(_Register) then
        Result := _Register
      else
        Result := E_NOTIMPL;
    finally FreeLibrary(DLLHandle); end;
  end; { DSiRegisterActiveX }

  {gp}
  procedure DSiRegisterRunOnce(const applicationName, applicationPath: string);
  begin
    DSiWriteRegistry('SOFTWARE\Microsoft\Windows\CurrentVersion\RunOnce',
      applicationName, applicationPath);
  end; { DSiRegisterRunOnce }

  {:On Vista+, calls DSiRemoveApplicationFromFirewallExceptionListAdvanced.
    On XP, remaps parameters and calls DSiRemoveApplicationFromFirewallExceptionListXP.
    On lesser Windows, simply returns False.
    @author  gabr
    @since   2009-10-28
  }
  function DSiRemoveApplicationFromFirewallExceptionList(const entryName,
    applicationFullPath: string): boolean;
  var
    versionInfo: TOSVersionInfo;
  begin
    Result := false;
    versionInfo.dwOSVersionInfoSize := SizeOf(versionInfo);
    GetVersionEx(versionInfo);
    if versionInfo.dwPlatformID = VER_PLATFORM_WIN32_NT then begin
      if (versionInfo.dwMajorVersion = 5) and (versionInfo.dwMinorVersion >= 1) then
        Result := DSiRemoveApplicationFromFirewallExceptionListXP(applicationFullPath)
      else if versionInfo.dwMajorVersion >= 6 then
        Result := DSiRemoveApplicationFromFirewallExceptionListAdvanced(entryName);
    end;
  end; { DSiRemoveApplicationFromFirewallExceptionList }

  {:Removes application from the firewall exception list, advanced style (Vista+).
    @author  gabr
    @since   2009-10-28
  }
  function  DSiRemoveApplicationFromFirewallExceptionListAdvanced(const entryName: string): boolean;
  var
    fwPolicy2: OleVariant;
  begin
    Result := false;
    try
      fwPolicy2 := CreateOLEObject('HNetCfg.FwPolicy2');
      fwPolicy2.Rules.Remove(entryName);
      Result := true;
    except
      on E: EOleSysError do
        SetLastError(cardinal(E.ErrorCode));
      else
        SetLastError(ERROR_INVALID_FUNCTION);
    end;
  end; { DSiRemoveApplicationFromFirewallExceptionListAdvanced }

  {:Removes application from the firewall exception list, XP style.
    @author  gabr
    @since   2009-10-28
  }
  function  DSiRemoveApplicationFromFirewallExceptionListXP(const applicationFullPath: string): boolean;
  var
    profile     : TDSiFwIPProfile;
    removeResult: boolean;
  begin
    Result := false;
    for profile := fwProfileDomain to fwProfilePublic do begin
      removeResult := DSiRemoveApplicationFromFirewallExceptionListXP(applicationFullPath, profile);
      Result := Result or removeResult;
    end;
  end; { DSiRemoveApplicationFromFirewallExceptionListXP }

  {:Removes application from a specific profile in the firewall exception list, XP style.
    @author  gabr
    @since   2010-07-27
  }
  function  DSiRemoveApplicationFromFirewallExceptionListXP(const applicationFullPath: string;
    profile: TDSiFwIPProfile): boolean;
  var
    fwMgr    : OleVariant;
    fwProfile: OleVariant;
  begin
    Result := false;
    try
      fwMgr := CreateOLEObject('HNetCfg.FwMgr');
      if profile = fwProfileDomain then
        fwProfile := fwMgr.LocalPolicy.GetProfileByType(NET_FW_PROFILE_DOMAIN)
      else if profile = fwProfilePrivate {alias for 'standard'} then
        fwProfile := fwMgr.LocalPolicy.GetProfileByType(NET_FW_PROFILE_STANDARD)
      else
        fwProfile := fwMgr.LocalPolicy.CurrentProfile;
      Result := fwProfile.AuthorizedApplications.Remove(DSiGetSubstPath(applicationFullPath)) = S_OK;
    except
      on E: EOleSysError do
        SetLastError(cardinal(E.ErrorCode));
      else
        SetLastError(ERROR_INVALID_FUNCTION);
    end;
  end; { DSiRemoveApplicationFromFirewallExceptionListXP }

  {gp}
  procedure DSiRemoveRunOnce(const applicationName: string);
  begin
    with TRegistry.Create do try
      RootKey := HKEY_CURRENT_USER;
      if OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\RunOnce', true) then try
        DeleteValue(applicationName);
      finally CloseKey; end;
    finally {TRegistry.}Free; end;
  end; { DSiRemoveRunOnce }

  {gp}
  function DSiRemoveUninstallInfo(const displayName: string): boolean;
  begin
    Result :=
      DSiKillRegistry('\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\'+displayName,
        UninstallRoot);
  end; { DSiRemoveUninstallInfo }

  {gp}
  function DSiShortcutExists(const displayName: string; folder: integer): boolean;
  var
    path: string;
  begin
    path := DSiGetFolderLocation(folder);
    if path <> '' then begin
      path := path + '\' + displayName + CLinkExt;
      Result := FileExists(path);
    end
    else
      Result := false;
  end; { DSiShortcutExists }

{ Time }

threadvar
  GLastTimeGetTime: DWORD;
  GTimeGetTimeBase: int64;

var
  GPerformanceFrequency: int64;

  constructor TDSiTimer.Create(enabled: boolean; interval: cardinal; onTimer: TNotifyEvent;
    tag: longint);
  begin
    inherited Create;
    dtEnabled := enabled;
    dtInterval := interval;
    dtOnTimer := onTimer;
    dtTag := tag;
    dtWindowHandle := DSiAllocateHWnd(WndProc);
    UpdateTimer;
  end; { TDSiTimer.Create }

  destructor TDSiTimer.Destroy;
  begin
    dtEnabled := false;
    UpdateTimer;
    DSiDeallocateHWnd(dtWindowHandle);
    inherited;
  end; { TDSiTimer.Destroy }

  procedure TDSiTimer.SetEnabled(value: boolean);
  begin
    if value <> dtEnabled then begin
      dtEnabled := value;
      UpdateTimer;
    end;
  end; { TDSiTimer.SetEnabled }

  procedure TDSiTimer.SetInterval(value: cardinal);
  begin
    if value <> dtInterval then begin
      dtInterval := value;
      UpdateTimer;
    end;
  end; { TDSiTimer.SetInterval }

  procedure TDSiTimer.SetOnTimer(value: TNotifyEvent);
  begin
    dtOnTimer := value;
    UpdateTimer;
  end; { TDSiTimer.SetOnTimer }

  procedure TDSiTimer.UpdateTimer;
  begin
    KillTimer(dtWindowHandle, 1);
    if (Interval <> 0) and Enabled and Assigned(OnTimer) then
      if SetTimer(dtWindowHandle, 1, Interval, nil) = 0 then
        RaiseLastOSError;
  end; { TDSiTimer.UpdateTimer }

  procedure TDSiTimer.WndProc(var msgRec: TMessage);
  begin
    with msgRec do begin
      if Msg = WM_TIMER then begin
        if Assigned(OnTimer) then
          OnTimer(Self);
      end
      else
        Result := DefWindowProc(dtWindowHandle, Msg, wParam, lParam);
    end; //with msgRec
  end; { TDSiTimer.WndProc }

  {:Converts time from Delphi TDateTime format to Windows TFileTime format.
    @returns false if conversion failed.
    @author  gabr
    @since   2012-02-06
  }
  function DSiDateTimeToFileTime(dateTime: TDateTime; var fileTime: TFileTime): boolean;
   var
    sysTime: TSystemTime;
  begin
    DateTimeToSystemTime(dateTime, sysTime);
    Result := SystemTimeToFileTime(sysTime, fileTime);
  end; { DSiDateTimeToFileTime }

  {gp}
  function DSiElapsedSince(midTime, startTime: int64): int64;
  begin
    if midTime < startTime then
      midTime := midTime + $100000000;
    Result := midTime - startTime;
  end; { TDSiRegistry.DSiElapsedSince }

  {:Returns time elapsed since startTime, which must be a result of the GetTickCount.
  }
  function DSiElapsedTime(startTime: int64): int64;
  begin
    Result := DSiElapsedSince(GetTickCount, startTime);
  end; { DSiElapsedTime }

  {:Returns time elapsed since startTime, which must be a result of the DSiTimeGetTime64.
  }
  function DSiElapsedTime64(startTime: int64): int64;
  begin
    Result := DSiTimeGetTime64 - startTime;
  end; { DSiElapsedTime64 }

  {:Converts time from Windows TFileTime format to Delphi TDateTime format.
    @returns 0 if conversion failed.
    @author  gabr
    @since   2006-12-20
  }
  function DSiFileTimeToDateTime(fileTime: TFileTime): TDateTime;
  begin
    if not DSiFileTimeToDateTime(fileTime, Result) then
      Result := 0;
  end; { DSiFileTimeToDateTime }

  {:Converts time from Windows TFileTime format to Delphi TDateTime format.
    @returns false if conversion failed.
    @author  gabr
    @since   2006-12-20
  }
  function DSiFileTimeToDateTime(fileTime: TFileTime; var dateTime: TDateTime): boolean;
  var
    sysTime: TSystemTime;
  begin
    Result := FileTimeToSystemTime(fileTime, sysTime);
    if Result then
      dateTime := SystemTimeToDateTime(sysTime);
  end; { DSiFileTimeToDateTime }

  {:Converts TFileTime structure containing nanoseconds (user and kernel time returned
    from GetProcessTimes, for example) into int64 containing microseconds.
    @since   2006-12-20
  }
  function DSiFileTimeToMicroSeconds(fileTime: TFileTime): int64;
  begin
    Int64Rec(Result).Lo := fileTime.dwLowDateTime;
    Int64Rec(Result).Hi := fileTime.dwHighDateTime;
    Result := Result div 10;
  end; { DSiFileTimeToMicroSeconds }

  {:Checks whether the specified timeout_ms period has elapsed. Start time must be a value
    returned from the GetTickCount.
  }
  function DSiHasElapsed(startTime: int64; timeout_ms: DWORD): boolean;
  begin
    if timeout_ms = 0 then
      Result := true
    else if timeout_ms = INFINITE then
      Result := false
    else
      Result := (DSiElapsedTime(startTime) > timeout_ms);
  end; { DSiHasElapsed }

  {:Checks whether the specified timeout_ms period has elapsed. Start time must be a value
    returned from the DSiTimeGetTime64.
  }
  function DSiHasElapsed64(startTime: int64; timeout_ms: DWORD): boolean;
  begin
    if timeout_ms = 0 then
      Result := true
    else if timeout_ms = INFINITE then
      Result := false
    else
      Result := (DSiElapsedTime64(startTime) > timeout_ms);
  end; { DSiHasElapsed64 }

  {:Converts value returned from QueryPerformanceCounter to milliseconds.
    @author  gabr
    @since   2007-12-03
  }
  function DSiPerfCounterToMS(perfCounter: int64): int64;
  begin
    Result := 0;
    if GPerformanceFrequency > 0 then
      Result := Round(perfCounter / GPerformanceFrequency * 1000);
  end; { DSiPerfCounterToMS }

  {:Converts value returned from QueryPerformanceCounter to microseconds.
    @author  gabr
    @since   2007-12-03
  }
  function DSiPerfCounterToUS(perfCounter: int64): int64;
  begin
    Result := 0;
    if GPerformanceFrequency > 0 then
      Result := Round(perfCounter / GPerformanceFrequency * 1000000);
  end; { DSiPerfCounterToUS }

  {:Calls QueryPerformanceCounter and returns the result as microseconds.
    @author  gabr
    @since   2007-12-03
  }
  function DSiQueryPerfCounterAsUS: int64;
  begin
    if QueryPerformanceCounter(Result) then
      Result := DSiPerfCounterToUS(Result)
    else
      Result := 0;
  end; { DSiQueryPerfCounterAsUS }

  {:64-bit extension of MM timeGetTime. Time units are milliseconds.
    @author  gabr
    @since   2007-11-26
  }
  function DSiTimeGetTime64: int64;
  begin
    Result := timeGetTime;
    if Result < GLastTimeGetTime then
      GTimeGetTimeBase := GTimeGetTimeBase + $100000000;
    GLastTimeGetTime := Result;
    Result := Result + GTimeGetTimeBase;
  end; { DSiTimeGetTime64 }

  {ales, Brdaws}
  //'delay' is in microseconds
  procedure DSiuSecDelay(delay: int64);
  var
    dif    : int64;
    endTime: TLargeInteger;
    freq   : TLargeInteger;
    nowTime: TLargeInteger;
  begin
    QueryPerformanceFrequency(freq);
    dif := delay * freq div 1000000;
    QueryPerformanceCounter(endTime);
    endTime := endTime + dif;
    repeat
      QueryPerformanceCounter(nowTime);
    until nowTime >= endTime;
  end; { DSiuSecDelay }

{ Interlocked }

function DSiInterlockedDecrement64(var addend: int64): int64; register;
asm
{$IFDEF CPUX64}
  mov   rax, dword - 1
  lock xadd [addend], rax
  dec   rax
{$ELSE}
{     ->          EAX     addend }
{     <-          EDX:EAX Result }
          PUSH    EDI
          PUSH    EBX

          MOV     EDI, EAX

          MOV     EAX, [EDI]    // Fetch original int64 at memory location
          MOV     EDX, [EDI+4]
@@1:
          MOV     ECX, EDX
          MOV     EBX, EAX

          SUB     EBX, 1
          SBB     ECX, 0

LOCK      CMPXCHG8B [EDI]
          JNZ     @@1

          { Returns updated value of addend }
          MOV     EAX, EBX
          MOV     EDX, ECX

          POP     EBX
          POP     EDI
{$ENDIF ~CPUX64}
end; { DSiInterlockedDecrement64 }

function DSiInterlockedIncrement64(var addend: int64): int64;
asm
{$IFDEF CPUX64}
  mov   rax, 1
  lock xadd [addend], rax
  inc   rax
{$ELSE}
{     ->          EAX     addend }
{     <-          EDX:EAX Result }
          PUSH    EDI
          PUSH    EBX

          MOV     EDI, EAX

          MOV     EAX, [EDI]    // Fetch original int64 at memory location
          MOV     EDX, [EDI+4]
@@1:
          MOV     ECX, EDX
          MOV     EBX, EAX

          ADD     EBX, 1
          ADC     ECX, 0

LOCK      CMPXCHG8B [EDI]
          JNZ     @@1

          { Returns updated value of addend }
          MOV     EAX, EBX
          MOV     EDX, ECX

          POP     EBX
          POP     EDI
{$ENDIF ~CPUX64}
end; { DSiInterlockedIncrement64 }

function DSiInterlockedExchangeAdd64(var addend: int64; value: int64): int64;
asm
{$IFDEF CPUX64}
  mov   rax, value
  lock  xadd [addend], value
  mov   rax, value
{$ELSE}
{     ->          EAX     addend }
{                 ESP+4   value  }
{     <-          EDX:EAX Result }

          PUSH    EDI
          PUSH    ESI
          PUSH    EBP
          PUSH    EBX

          MOV     ESI, DWORD PTR [value]    // EDI:ESI = value
          MOV     EDI, DWORD PTR [value+4]
          MOV     EBP, EAX

          MOV     EAX, [EBP]    // EDX:EAX = addend (fetch original int64 value)
          MOV     EDX, [EBP+4]
@@1:
          MOV     ECX, EDX      // ECX:EBX = addend
          MOV     EBX, EAX

          ADD     EBX, ESI
          ADC     ECX, EDI

LOCK      CMPXCHG8B [EBP]
          JNZ     @@1
          // Returns initial value in addend

          POP     EBX
          POP     EBP
          POP     ESI
          POP     EDI
{$ENDIF ~CPUX64}
end; { DSiInterlockedExchangeAdd64 }

function DSiInterlockedExchange64(var target: int64; value: int64): int64;
asm
{$IFDEF CPUX64}
  lock  xchg    [target], value
  mov   rax, value
{$ELSE}
{     ->          EAX     target }
{                 ESP+4   value  }
{     <-          EDX:EAX Result }
          PUSH    EDI
          PUSH    EBX

          MOV     EDI, EAX

          MOV     EAX, [EDI]
          MOV     EDX, [EDI+4]

          MOV     EBX, DWORD PTR [value]
          MOV     ECX, DWORD PTR [value+4]
@@1:
LOCK      CMPXCHG8B [EDI]
          JNZ     @@1
          // Returns initial value in target

          POP     EBX
          POP     EDI
{$ENDIF ~CPUX64}
end; { DSiInterlockedExchange64 }

function DSiInterlockedCompareExchange64(var destination: int64; exchange, comparand: int64): int64;
asm
{$IFDEF CPUX64}
  mov   rax, comparand
  lock cmpxchg [destination], exchange
{$ELSE}
{     ->          EAX     destination }
{                 ESP+4   exchange    }
{                 ESP+12  comparand   }
{     <-          EDX:EAX Result      }
          PUSH    EBX
          PUSH    EDI

          MOV     EDI, EAX

          MOV     EAX, DWORD PTR [comparand]
          MOV     EDX, DWORD PTR [comparand+4]

          MOV     EBX, DWORD PTR [exchange]
          MOV     ECX, DWORD PTR [exchange+4]

LOCK      CMPXCHG8B [EDI]

          POP     EDI
          POP     EBX
{$ENDIF ~CPUX64}
end; { DSiInterlockedCompareExchange64 }

function DSiInterlockedCompareExchange64(destination: PInt64; exchange, comparand: int64): int64;
asm
{$IFDEF CPUX64}
  mov   rax, comparand
  lock cmpxchg [destination], exchange
{$ELSE}
{     ->          EAX     destination }
{                 ESP+4   exchange    }
{                 ESP+12  comparand   }
{     <-          EDX:EAX Result      }
          PUSH    EBX
          PUSH    EDI

          MOV     EDI, EAX

          MOV     EAX, DWORD PTR [comparand]
          MOV     EDX, DWORD PTR [comparand+4]

          MOV     EBX, DWORD PTR [exchange]
          MOV     ECX, DWORD PTR [exchange+4]

LOCK      CMPXCHG8B [EDI]

          POP     EDI
          POP     EBX
{$ENDIF ~CPUX64}
end; { DSiInterlockedCompareExchange64 }

{ DynaLoad }

var
  _GLibraryList: TStringList = nil;

  function GLibraryList: TStringList;
  begin
    if not assigned(_GLibraryList) then
      _GLibraryList := TStringList.Create;
    Result := _GLibraryList;
  end; { GLibraryList }

  {:Loads library and adds library handle to the list of handles that must be
    unloaded at process termination. Caches library handle for future reference.
    @since   2003-09-02
  }
  function DSiLoadLibrary(const libFileName: string): HMODULE;
  var
    hLib   : HMODULE;
    idxLib : integer;
    OldMode: DWORD;
  begin
    idxLib := GLibraryList.IndexOf(libFileName);
    if idxLib < 0 then begin
      OldMode := SetErrorMode (SEM_NOOPENFILEERRORBOX);
      hLib := LoadLibrary(PChar(libFileName));
      SetErrorMode (OldMode);
      if hLib <> 0 then
        idxLib := GLibraryList.AddObject(libFileName, TObject(hLib));
    end;
    if idxLib >= 0 then
      Result := HMODULE(GLibraryList.Objects[idxLib])
    else
      Result := 0;
  end; { DSiLoadLibrary }

  {:Loads the library if it was not loaded yet (and adds it to the list of
    libraries that must be unloaded at process termination), then calls
    GetProcAddress.
    @since   2003-09-02
  }        
  function DSiGetProcAddress(const libFileName, procName: string): FARPROC;
  begin
    Result := GetProcAddress(DSiLoadLibrary(libFileName), PChar(procName));
  end; { DSiGetProcAddress }

  {:Unloads all loaded libraries.
    @since   2003-09-02
  }
  procedure DSiUnloadLibrary;
  var
    iLib: integer;
  begin
    for iLib := 0 to GLibraryList.Count-1 do
      if HMODULE(GLibraryList.Objects[iLib]) <> 0 then begin
        FreeLibrary(HMODULE(GLibraryList.Objects[iLib]));
        GLibraryList.Objects[iLib] := TObject(0);
      end;
  end; { TDSiRegistry.DSiUnloadLibrary }

  function DSi9xNetShareAdd(serverName: PChar; shareLevel: smallint;
    buffer: pointer; size: word): integer;
  begin
    if not assigned(G9xNetShareAdd) then
      G9xNetShareAdd := DSiGetProcAddress('svrapi.dll', 'NetShareAdd');
    if assigned(G9xNetShareAdd) then
      Result := G9xNetShareAdd(serverName, shareLevel, buffer, size)
    else
      Result := ERROR_NOT_SUPPORTED;
  end; { DSi9xNetShareAdd }

  function DSi9xNetShareDel(serverName: PChar; netName: PChar;
    reserved: word): integer;
  begin
    if not assigned(G9xNetShareDel) then
      G9xNetShareDel := DSiGetProcAddress('svrapi.dll', 'NetShareDel');
    if assigned(G9xNetShareDel) then
      Result := G9xNetShareDel(serverName, netName, reserved)
    else
      Result := ERROR_NOT_SUPPORTED;
  end; { DSi9xNetShareDel }
  
  function DSiCloseServiceHandle(hSCObject: SC_HANDLE): BOOL;
  begin
    if not assigned(GCloseServiceHandle) then
      GCloseServiceHandle := DSiGetProcAddress('advapi32.dll',
        'CloseServiceHandle');
    if assigned(GCloseServiceHandle) then
      Result := GCloseServiceHandle(hSCObject)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiCloseServiceHandle }

  function DSiCreateProcessAsUser(hToken: THandle;
    lpApplicationName, lpCommandLine: PChar; lpProcessAttributes,
    lpThreadAttributes: PSecurityAttributes; bInheritHandles: BOOL;
    dwCreationFlags: DWORD; lpEnvironment: pointer;
    lpCurrentDirectory: PChar; const lpStartupInfo: TStartupInfo;
    var lpProcessInformation: TProcessInformation): BOOL;
  {$IFDEF Unicode}
  var
    commandLine: string;
  {$ENDIF Unicode}
  begin
    {$IFDEF Unicode}
      commandLine := lpCommandLine;
      {$IFDEF Unicode}UniqueString(commandLine);{$ENDIF Unicode}
      Result := CreateProcessAsUser(hToken, lpApplicationName, PChar(commandLine),
        lpProcessAttributes, lpThreadAttributes, bInheritHandles,
        dwCreationFlags, lpEnvironment, lpCurrentDirectory, lpStartupInfo,
        lpProcessInformation);
    {$ELSE}
      if not assigned(GCreateProcessAsUser) then
        GCreateProcessAsUser := DSiGetProcAddress('advapi32.dll', 'CreateProcessAsUserA');
      if assigned(GCreateProcessAsUser) then
        Result := GCreateProcessAsUser(hToken, lpApplicationName, lpCommandLine,
          lpProcessAttributes, lpThreadAttributes, bInheritHandles,
          dwCreationFlags, lpEnvironment, lpCurrentDirectory, lpStartupInfo,
          lpProcessInformation)
      else begin
        SetLastError(ERROR_NOT_SUPPORTED);
        Result := false;
      end;
    {$ENDIF Unicode}
  end; { DSiCreateProcessAsUser }

  function DSiCreateProcessWithLogonW(lpUsername, lpDomain, lpPassword: PWideChar;
    dwLogonFlags: DWORD; lpApplicationName, lpCommandLine: PWideChar;
    dwCreationFlags: DWORD; lpEnvironment: pointer; lpCurrentDirectory: PWideChar;
    const lpStartupInfo: TStartupInfoW; var lpProcessInformation: TProcessInformation): BOOL;
  var
    restoreLastError: IRestoreLastError;
  var
    commandLine: WideString;
  begin
    if not assigned(GCreateProcessWithLogonW) then
      GCreateProcessWithLogonW := DSiGetProcAddress('advapi32.dll', 'CreateProcessWithLogonW');
    if assigned(GCreateProcessWithLogonW) then begin
      commandLine := lpCommandLine;
      {$IFDEF Unicode}UniqueString(commandLine);{$ENDIF Unicode}
      Result := GCreateProcessWithLogonW(lpUsername, lpDomain, lpPassword, dwLogonFlags,
        lpApplicationName, PWideChar(commandLine), dwCreationFlags, lpEnvironment,
        lpCurrentDirectory, lpStartupInfo, lpProcessInformation)
    end
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
    restoreLastError := TRestoreLastError.Create;
  end; { DSiCreateProcessWithLogonW }

  function DSiCreateEnvironmentBlock(var lpEnvironment: pointer; hToken: THandle;
    bInherit: BOOL): BOOL;
  begin
    if not assigned(GCreateEnvironmentBlock) then
      GCreateEnvironmentBlock := DSiGetProcAddress('userenv.dll', 'CreateEnvironmentBlock');
    if assigned(GCreateEnvironmentBlock) then
      Result := GCreateEnvironmentBlock(lpEnvironment, hToken, bInherit)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiCreateEnvironmentBlock }

  function DSiDestroyEnvironmentBlock(lpEnvironment: pointer): BOOL;
  begin
    if not assigned(GDestroyEnvironmentBlock) then
      GDestroyEnvironmentBlock := DSiGetProcAddress('userenv.dll', 'DestroyEnvironmentBlock');
    if assigned(GDestroyEnvironmentBlock) then
      Result := GDestroyEnvironmentBlock(lpEnvironment)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiDestroyEnvironmentBlock }

  //http://msdn2.microsoft.com/en-us/library/aa969510.aspx
  function DSiDwmEnableComposition(uCompositionAction: UINT): HRESULT;
  begin
    if not assigned(GDwmEnableComposition) then
      GDwmEnableComposition := DSiGetProcAddress('DWMAPI.dll', 'DwmEnableComposition');
    if assigned(GDwmEnableComposition) then
      Result := GDwmEnableComposition(uCompositionAction)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := S_FALSE;
    end;
  end; { DSiDwmEnableComposition }

  //http://msdn2.microsoft.com/en-us/library/aa969518.aspx
  function DSiDwmIsCompositionEnabled(var pfEnabled: BOOL): HRESULT; stdcall;
  begin
    if not assigned(GDwmIsCompositionEnabled) then
      GDwmIsCompositionEnabled := DSiGetProcAddress('DWMAPI.dll', 'DwmIsCompositionEnabled');
    if assigned(GDwmIsCompositionEnabled) then
      Result := GDwmIsCompositionEnabled(pfEnabled)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := S_FALSE;
    end;
  end; { DSiDwmIsCompositionEnabled }

  function DSiEnumProcessModules(hProcess: THandle; lphModule: PModule; cb: DWORD;
    var lpcbNeeded: DWORD): BOOL; stdcall;
  begin
    if not assigned(GEnumProcessModules) then
      GEnumProcessModules := DSiGetProcAddress('psapi.dll', 'EnumProcessModules');
    if assigned(GEnumProcessModules) then
      Result := GEnumProcessModules(hProcess, lphModule, cb, lpcbNeeded)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiEnumProcessModules }

  function DSiGetModuleFileNameEx(hProcess: THandle; hModule: HMODULE; lpFilename: PChar;
    nSize: DWORD): DWORD; stdcall;
  begin
    if not assigned(GGetModuleFileNameEx) then
      GGetModuleFileNameEx := DSiGetProcAddress('psapi.dll', 'GetModuleFileNameEx' + CAPISuffix);
    if assigned(GGetModuleFileNameEx) then
      Result := GGetModuleFileNameEx(hProcess, hModule, lpFilename, nSize)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := 0;
    end;
  end; { DSiGetModuleFileNameEx }

  function DSiGetProcessImageFileName(hProcess: THandle; lpImageFileName: PChar;
    nSize: DWORD): DWORD; stdcall;
  begin
    if not assigned(GGetProcessImageFileName) then
      GGetProcessImageFileName := DSiGetProcAddress('psapi.dll', 'GetProcessImageFileName' + CAPISuffix);
    if assigned(GGetProcessImageFileName) then
      Result := GGetProcessImageFileName(hProcess, lpImageFileName, nSize)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := 0;
    end;
  end; { DSiGetProcessImageFileName }

  function DSiGetProcessMemoryInfo(process: THandle; memCounters: PProcessMemoryCounters;
    cb: DWORD): boolean; stdcall;
  begin
    if not assigned(GGetProcessMemoryInfo) then
      GGetProcessMemoryInfo := DSiGetProcAddress('psapi.dll', 'GetProcessMemoryInfo');
    if assigned(GGetProcessMemoryInfo) then
      Result := GGetProcessMemoryInfo(process, memCounters, cb)
    else begin        
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiGetProcessMemoryInfo }

  function DSiGetTickCount64: int64; stdcall;
  begin
    if not assigned(GGetTickCount64) then
      GGetTickCount64 := DSiGetProcAddress('kernel32.dll', 'GetTickCount64');
    if assigned(GGetTickCount64) then
      Result := GGetTickCount64()
    else
      raise Exception.Create('Unsupported API: GetTickCount64');
  end; { DSiGetTickCount64 }

  function DSiGetUserProfileDirectoryW(hToken: THandle; lpProfileDir: PWideChar;
    var lpcchSize: DWORD): BOOL;
  begin
    if not assigned(GGetUserProfileDirectoryW) then
      GGetUserProfileDirectoryW := DSiGetProcAddress('userenv.dll', 'GetUserProfileDirectoryW');
    if assigned(GGetUserProfileDirectoryW) then
      Result := GGetUserProfileDirectoryW(hToken, lpProfileDir, lpcchSize)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiGetUserProfileDirectoryW }

  function DSiGlobalMemoryStatusEx(memStatus: PMemoryStatusEx): boolean; stdcall;
  begin
    if not assigned(GGlobalMemoryStatusEx) then
      GGlobalMemoryStatusEx := DSiGetProcAddress('kernel32.dll', 'GlobalMemoryStatusEx');
    if assigned(GGlobalMemoryStatusEx) then
      Result := GGlobalMemoryStatusEx(memStatus)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiGlobalMemoryStatusEx }

  function DSiImpersonateLoggedOnUser(hToken: THandle): BOOL; stdcall;
  begin
    if not assigned(GImpersonateLoggedOnUser) then
      GImpersonateLoggedOnUser := DSiGetProcAddress('advapi32.dll',
        'ImpersonateLoggedOnUser');
    if assigned(GImpersonateLoggedOnUser) then
      Result := GImpersonateLoggedOnUser(hToken)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiImpersonateLoggedOnUser }

  function DSiIsWow64Process(hProcess: THandle; var wow64Process: BOOL): BOOL; stdcall;
  begin
    if not assigned(GIsWow64Process) then
      GIsWow64Process := DSiGetProcAddress('kernel32.dll', 'IsWow64Process');
    if assigned(GIsWow64Process) then
      Result := GIsWow64Process(hProcess, wow64Process)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiIsWow64Process }

  function DSiLogonUser(lpszUsername, lpszDomain, lpszPassword: PChar;
    dwLogonType, dwLogonProvider: DWORD; var phToken: THandle): BOOL;
  begin
    {$IFDEF Unicode}
      Result := LogonUser(lpszUsername, lpszDomain, lpszPassword, dwLogonType,
        dwLogonProvider, phToken);
    {$ELSE}
      Result := false;
      if not assigned(GLogonUser) then
        GLogonUser := DSiGetProcAddress('advapi32.dll', 'LogonUserA');
      if assigned(GLogonUser) then begin
        if not DSiEnablePrivilege('SeTcbName') and DSiEnablePrivilege('SeChangeNotifyName')
        then
          Exit;
        Result := GLogonUser(lpszUsername, lpszDomain, lpszPassword, dwLogonType,
          dwLogonProvider, phToken)
      end
      else
        SetLastError(ERROR_NOT_SUPPORTED);
    {$ENDIF Unicode}
  end; { DSiLogonUser }

  function DSiNetApiBufferFree(buffer: pointer): cardinal; stdcall;
  begin
    if not assigned(GNetApiBufferFree) then
      GNetApiBufferFree := DSiGetProcAddress('netapi32.dll', 'NetWkstaGetInfo');
    if assigned(GNetApiBufferFree) then
      Result := GNetApiBufferFree(buffer)
    else
      Result := ERROR_NOT_SUPPORTED;
  end; { DSiNetApiBufferFree }

  function DSiNetWkstaGetInfo(servername: PChar; level: cardinal;
    out bufptr: pointer): cardinal; stdcall;
  begin
    if not assigned(GNetWkstaGetInfo) then
      GNetWkstaGetInfo := DSiGetProcAddress('netapi32.dll', 'NetWkstaGetInfo');
    if assigned(GNetWkstaGetInfo) then
      Result := GNetWkstaGetInfo(servername, level, bufptr)
    else
      Result := ERROR_NOT_SUPPORTED;
  end; { DSiNetWkstaGetInfo }

  function DSiNTNetShareAdd(serverName: PChar; level: integer; buf: PChar;
    var parm_err: integer): DWord;
  begin
    if not assigned(GNTNetShareAdd) then
      GNTNetShareAdd := DSiGetProcAddress('netapi32.dll', 'NetShareAdd');
    if assigned(GNTNetShareAdd) then
      Result := GNTNetShareAdd(serverName, level, buf, parm_err)
    else
      Result := ERROR_NOT_SUPPORTED;
  end; { DSiNTNetShareAdd }

  function DSiNTNetShareDel(serverName: PChar; netName: PWideChar;
    reserved: integer): DWord;
  begin
    if not assigned(GNTNetShareDel) then
      GNTNetShareDel := DSiGetProcAddress('netapi32.dll', 'NetShareDel');
    if assigned(GNTNetShareDel) then
      Result := GNTNetShareDel(serverName, netName, reserved)
    else
      Result := ERROR_NOT_SUPPORTED;
  end; { DSiNTNetShareDel }

  function DSiOpenSCManager(lpMachineName, lpDatabaseName: PChar;
    dwDesiredAccess: DWORD): SC_HANDLE; stdcall;
  begin
    if not assigned(GOpenSCManager) then
      GOpenSCManager := DSiGetProcAddress('advapi32.dll', 'OpenSCManager' + CAPISuffix);
    if assigned(GOpenSCManager) then
      Result := GOpenSCManager(lpMachineName, lpDatabaseName, dwDesiredAccess)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := 0;
    end;
  end; { DSiOpenSCManager }

  function DSiRevertToSelf: BOOL; stdcall;
  begin
    if not assigned(GRevertToSelf) then
      GRevertToSelf := DSiGetProcAddress('advapi32.dll', 'RevertToSelf');
    if assigned(GRevertToSelf) then
      Result := GRevertToSelf
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiRevertToSelf }

  function DSiSetDllDirectory(path: PChar): boolean; stdcall;
  begin
    if not assigned(GSetDllDirectory) then
      GSetDllDirectory := DSiGetProcAddress('kernel32.dll', 'SetDllDirectory' + CAPISuffix);
    if assigned(GSetDllDirectory) then
      Result := GSetDllDirectory(path)
    else
      Result := false;
  end; { DSiSetDllDirectory }

  function DSiSetSuspendState(hibernate, forceCritical, disableWakeEvent: BOOL): BOOL; stdcall;
  begin
    if not assigned(GSetSuspendState) then
      GSetSuspendState := DSiGetProcAddress('PowrProf.dll', 'SetSuspendState');
    if assigned(GSetSuspendState) then
      Result := GSetSuspendState(hibernate, forceCritical, disableWakeEvent)
    else
      Result := false;
  end; { DSiSetSuspendState }

  function DSiSHEmptyRecycleBin(Wnd: HWND; pszRootPath: PChar;
    dwFlags: DWORD): HRESULT; stdcall;
  begin
    if not assigned(GSHEmptyRecycleBin) then
      GSHEmptyRecycleBin := DSiGetProcAddress('shell32.dll', 'SHEmptyRecycleBin' + CAPISuffix);
    if assigned(GSHEmptyRecycleBin) then
      Result := GSHEmptyRecycleBin(Wnd, pszRootPath, dwFlags)
    else
      Result := S_FALSE;
  end; { DSiSHEmptyRecycleBin }

  function DSiWow64DisableWow64FsRedirection(var oldStatus: pointer): BOOL; stdcall;
  begin
    if not assigned(GWow64DisableWow64FsRedirection) then
      GWow64DisableWow64FsRedirection := DSiGetProcAddress('kernel32.dll', 'Wow64DisableWow64FsRedirection');
    if assigned(GWow64DisableWow64FsRedirection) then
      Result := GWow64DisableWow64FsRedirection(oldStatus)
    else
      Result := false;
  end; { DSiWow64DisableWow64FsRedirection }

  function DSiWow64RevertWow64FsRedirection(const oldStatus: pointer): BOOL; stdcall;
  begin
    if not assigned(GWow64RevertWow64FsRedirection) then
      GWow64RevertWow64FsRedirection := DSiGetProcAddress('kernel32.dll', 'Wow64RevertWow64FsRedirection');
    if assigned(GWow64RevertWow64FsRedirection) then
      Result := GWow64RevertWow64FsRedirection(oldStatus)
    else
      Result := false;
  end; { DSiWow64RevertWow64FsRedirection }

{ TRestoreLastError }

constructor TRestoreLastError.Create;
begin
  inherited Create;
  rleLastError := GetLastError;
end; { TRestoreLastError.Create }

destructor TRestoreLastError.Destroy;
begin
  SetLastError(rleLastError);
  inherited;
end; { TRestoreLastError.Destroy }

{ TBackgroundTask }

constructor TBackgroundTask.Create(waitObject: THandle);
begin
  inherited Create;
  btWaitObject := waitObject;
end; { TBackgroundTask.Create }

{ TCleanupAces }

procedure TCleanupAces.Awaited;
begin
  if assigned(caSid) then begin
    if caWindowStation <> 0 then
      DSiRemoveAceFromWindowStation(caWindowStation, caSid);
    if caDesktop <> 0 then
      DSiRemoveAceFromDesktop(caDesktop, caSid);
  end;
end; { TCleanupAces.Awaited }

constructor TCleanupAces.Create(waitObject: THandle; windowStation: HWINSTA;
  desktop: HDESK; sid: PSID);
var
  waitHandle: THandle;
begin
  Win32Check(DuplicateHandle(GetCurrentProcess, waitObject, GetCurrentProcess,
      @waitHandle, 0, false, DUPLICATE_SAME_ACCESS));
  inherited Create(waitHandle);
  caWindowStation := windowStation;
  caDesktop := desktop;
  caSid := sid;
end; { TCleanupAces.Create }

destructor TCleanupAces.Destroy;
begin
  if assigned(caSid) then 
    HeapFree(GetProcessHeap, 0, caSid);
  if caWindowStation <> 0 then
    CloseWindowStation(caWindowStation);
  if caDesktop <> 0 then
    CloseDesktop(caDesktop);
  inherited;
end; { TCleanupAces.Destroy }

{ TBackgroundThread }

constructor TBackgroundThread.Create(task: TBackgroundTask);
begin
  btTask := task;
  FreeOnTerminate := true;
  inherited Create(false);
end; { TBackgroundThread.Create }

procedure TBackgroundThread.Execute;
begin
  if DSiWaitForTwoObjects(GTerminateBackgroundTasks, btTask.WaitObject, false, INFINITE) = WAIT_OBJECT_1 then
    btTask.Awaited;
  FreeAndNil(btTask);
end; { TBackgroundThread.Execute }

{ initialization }

procedure DynaLoadAPIs;
begin
  GInterlockedCompareExchange64 := DSiGetProcAddress('kernel.dll', 'InterlockedCompareExchange64');
end; { DynaLoadAPIs }

initialization
  InitializeCriticalSection(GDSiWndHandlerCritSect);
  GTerminateBackgroundTasks := CreateEvent(nil, false, false, nil);
  GDSiWndHandlerCount := 0;
  GTimeGetTimeBase := 0;
  GLastTimeGetTime := 0;
  if not QueryPerformanceFrequency(GPerformanceFrequency) then
    GPerformanceFrequency := 0;
  GCF_HTML := RegisterClipboardFormat('HTML Format');
  DynaLoadAPIs;
  timeBeginPeriod(1);
finalization
  timeEndPeriod(1);
  DSiCloseHandleAndNull(GTerminateBackgroundTasks);
  DeleteCriticalSection(GDSiWndHandlerCritSect);
  DSiUnloadLibrary;
  FreeAndNil(_GLibraryList);
end.
