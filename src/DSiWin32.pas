(*:Collection of Win32/Win64 wrappers and helper functions.
   @desc <pre>
   Free for personal and commercial use. No rights reserved.

   Maintainer        : gabr
   Contributors      : ales, aoven, gabr, Lee_Nover, _MeSSiah_, Miha-R, Odisej, xtreme,
                       Brdaws, Gre-Gor, krho, Cavlji, radicalb, fora, M.C, MP002, Mitja,
                       Christian Wimmer, Tommi Prami, Miha, Craig Peterson, Tommaso Ercole,
                       bero.
   Creation date     : 2002-10-09
   Last modification : 2020-04-03
   Version           : 2.0
</pre>*)(*
   History:
     2.0: 2020-04-03
       - Extracted all code depending on Vcl.Graphics into unit DSiWin32.VCL.
     1.108: 2020-04-03
       - Compiles without referencing Vcl.Graphics if NoVCL symbol is defined.
     1.107: 2020-01-21
       - Better process termination of timed-out processes in DSiExecuteAndCapture.
     1.106: 2019-12-13
       - DSiEnumFiles* family filters out non-directories when only directories are
         requested (attr = faDirectory).
     1.105b: 2019-05-08
       - Compiles with Delphi 2007.
     1.105a: 2019-05-06
       - DSiDeallocateHWnd prevents WndProc from being called after the window was destroyed.
     1.105: 2019-03-01
       - TDSiRegistry.WriteVariant handles more integer types.
     1.104: 2019-02-04
       - Added functions DSiGetSystemTimePreciseAsFileTime.
     1.103a: 2018-08-09
       - [MkhPavel] fixed declarations for _PROCESS_MEMORY_COUNTERS.
       - Compiles again with Delphi 2007 and 2009.
     1.103: 2018-05-12
       - DSiExecuteAndCapture can accept a nil output in which case the buffer will be reset
          on each ProcessPartialLine.
       - Added an optional abortHandle parameter to DSiExecuteAndCapture to make it cancellable.
     1.102: 2018-05-11
       - Fixed DSiExecuteInSession. In Unicode, `cmdLine` was not copied to local buffer.
         In Ansi, `cmdLine` was of wrong string type.
     1.101: 2018-04-19
       - DSiExecuteAndCapture supports CR-delimited output.
     1.100b: 2017-09-05
       - Fixed WideCharBufToUTF8Buf and UTF8BufToWideCharBuf which were casting pointers
         to integer instead of NativeUInt.
     1.100a: 2017-07-31
       - DSiTimeGetTime64 was not thread-safe.
     1.100: 2017-07-26
       - Added functions DSiGetProcessSID and DSiHasRoamingProfile.
     1.99: 2017-06-29
       - Addded dynamically loaded API DSiWTSQueryUserToken.
       - Added DSiExecuteInSession which can start interactive process from session 0.
     1.97: 2017-05-25
       - Added parameters `parameters`, `directory`, and `wait` to DSiExecuteAsAdmin.
       - Fixed: various functions declared file handle _not_ as THandle and would fail
         in Win64 mode.
     1.96: 2017-04-06
       - Added DSiFileExtensionIs overload accepting TArray<string>.
     1.95: 2017-03-28
       - Changed DSiTimeGetTime64, DSiElapsedTime64, DSiHasElapsed64 to use locking instead
         of a thread-local variable. This way timestamps can be safely used across the
         thread boundary.
     1.94: 2016-10-19
       - Added DSiGetLogicalProcessorInformationEx.
       - Added some Winapi stuff needed in older Delphis.
     1.93: 2016-10-05
       - DSiGetWindowsVersion didn't call GetVersionEx() for MajorVersion 10 (Windows 10 and Server 2016).
     1.92: 2016-09-23
       - Added optional 'window class' parameter to DSiFindWindow.
     1.91: 2016-07-19
       - Implemented DSiFindWindow.
     1.90: 2016-07-18
       - Implemented dynamically loaded forwarders DSiGetSystemFirmwareTable,
         DSiGetNumaProximityNodeEx, and DSiGetNumaHighestNodeNumber.
     1.89: 2016-07-01
       - Implemented dynamically loaded forwarders DSiGetThreadGroupAffinity and
         DSiSetThreadGroupAffinity.
     1.88: 2016-06-15
       - [shaun07776] C++Builder compatible.
     1.87: 2016-05-18
       - Added function DSiExecuteAsAdmin.
     1.86: 2016-03-05
       - [bero] Added 'const' to various 'string' parameters.
     1.85: 2016-01-16
       - DSiExecuteAndCapture supports #10-delimited program output in
         combination with the `onNewLine` handler.
     1.84a: 2015-11-20
       - Removed creationFlags parameter from the simpler DSiExecute overload as it was
         not used in the implementation.
     1.84: 2015-10-20
       - Updated DSiGetWindowsVersion for Windows 8.1, Windows 10, Windows Server 2012,
         and Windows Server 2016.
     1.83: 2015-09-22
       - Added function DSiFileSizeAndTime.
     1.82b: 2015-09-22
       - File handle is open with least access in DSiGetFileTimes.
     1.82a: 2015-07-29
       - Fixed DSiGetClassName for Unicode.
     1.82: 2015-04-24
       - Affinity functions support up to 64 cores in 64-bit mode
         (previously they were limited to 32 cores).
     1.81: 2015-04-17
       - Fixed DSiClassWndProc parameter types for Win64.
       - DSiClassWndProc catches exceptions in instance's WndProc.
     1.80: 2015-03-06
       - Added function DSiGetFocusedWindow.
     1.79d: 2015-01-06
       - Compiles with D2007 and D2009 again.
     1.79c: 2015-01-05
       - Attribute is no longer checked in _DSiEnumFilesEx and returns all from FindFirst/Next.
     1.79b: 2014-12-08
       - Fixed attribute checking in _DSiEnumFilesEx.
     1.79a: 2014-11-11
       - DSiFileSize opens file with FILE_SHARE_READ OR FILE_SHARE_WRITE OR FILE_SHARE_DELETE.
     1.79: 2014-10-15
       - Creation flags can be passed to DSiExecute and DSiExecuteAndCapture.
     1.78: 2014-10-09
       - Added allowRoot parameter to DSiDeleteTree (default False).
       - Added safety checks to DSiDeleteTree.
     1.77: 2014-10-06
       - Added another DSiExecute overload which returns TProcessInformation with active
         process and thread handles (caller should CloseHandle them).
     1.76: 2014-07-24
       - Defined DSiRegisterUserFileAssoc and DSiUnregisterUserFileAssoc.
     1.75: 2014-06-02
       - Defined LCID constants.
     1.74: 2014-05-12
       - Defined CSIDL_PERSONAL constant.
       - DSiGetFolderLocation uses SHGetFolderLocation instead of deprecated
         SHGetSpecialFolderPath.
     1.73: 2014-04-21
       - DSiEnumFiles family has new parameter - ignoreDottedFolders. When True (default
         is False), folders that start in a dot (for example, .UnitTests) will be skipped
         during enumeration.
     1.72e: 2013-10-27
       - Fixed invalid control flow in DSiFileExtensionIs.
     1.72d: 2013-10-14
       - Removed compiler warnings in XE5.
     1.72c: 2013-10-07
       - Compiles with D2009 again.
     1.72b: 2013-10-06
       - Fixed enumeration routines to not return folders that don't match the mask.
     1.72a: 2013-06-03
       - [Tommaso Ercole] DSiAllocateHWnd accepts 'style' and 'parentWindow' parameter
         which are by default set to (WS_EX_TOOLWINDOW OR WS_EX_NOACTIVATE) and
         HWND_MESSAGE, respectively. These are suggested values for message-only windows
         (http://msdn.microsoft.com/en-us/library/windows/desktop/ms632680(v=vs.85).aspx).
         Previously, these values were WS_EX_TOOLWINDOW and 0, respectively.
     1.72: 2013-02-07
       - Added function DSiIsCodeSigned.
     1.71b: 2012-12-12
       - Fixed parameter types in DSiClassWndProc.
     1.71a: 2012-11-04
       - TNonClientMetrics.cbSize is correctly initialized.
     1.71: 2012-10-19
       - Defined type DSiNativeInt and DSiNativeUInt which map to integer/cardinal on
         Delphi XE and older.
       - Fixed invalid casting in DSiAllocateHWnd.
       - All internal DEFINEs were renamed with 'DSi' prefix.
     1.70b: 2012-06-12
       - DSiEnumFiles* will also enumerate hidden folders if 'attr' contains faHidden flag.
     1.70a: 2012-05-22
       - [Tommi Prami, MihaR] Defined ULONG_PTR and ULONGLONG for Delphis up to D2006.
     1.70: 2012-05-21
       - TDSiRegistry.ReadBool accepts 't' and 'true' as True.
     1.69: 2012-05-18
       - Added dynamically loaded API forwarder DSiGetLogicalProcessorInformation.
       - Added function DSiGetLogicalProcessorInfo.
       - Fixed a bug in DSiEnumFilesToOL.
     1.68: 2012-05-16
       - New function: DSiEnumFilesToOL.
     1.67: 2012-05-15
       - Callback functions are declared as anonymous delegates in D2009 and newer.
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

{$DEFINE DSiNeedUTF}{$UNDEF DSiNeedVariants}{$DEFINE DSiNeedStartupInfo}
{$DEFINE DSiNeedFileCtrl}
{$DEFINE DSiNeedRawByteString}
{$UNDEF DSiHasGroupAffinity}{$UNDEF DSiNeedUSHORT}
{$IFDEF ConditionalExpressions}
  {$UNDEF DSiNeedUTF}{$DEFINE DSiNeedVariants}{$UNDEF DSiNeedStartupInfo}{$UNDEF DSiHasSafeNativeInt}{$UNDEF UseAnsiStrings}
  {$IF CompilerVersion >= 25}{$LEGACYIFEND ON}{$IFEND}
  {$IF RTLVersion >= 18}{$UNDEF DSiNeedFileCtrl}{$IFEND}
  {$IF CompilerVersion >= 25}{$DEFINE DSiUseAnsiStrings}{$IFEND}
  {$IF CompilerVersion >= 23}{$DEFINE DSiScopedUnitNames}{$DEFINE DSiHasSafeNativeInt}{$DEFINE DSiHasTPath}{$DEFINE DSiHasGroupAffinity}{$DEFINE DSiHasSizeT}{$IFEND}
  {$IF CompilerVersion >= 22}{$DEFINE DSiHasAnonymousFunctions}{$DEFINE DSiHasGenerics}{$IFEND} // only XE+ has 'good enough' generics
  {$IF CompilerVersion > 19}{$DEFINE DSiHasGetFolderLocation}{$IFEND}
  {$IF CompilerVersion >= 19}{$DEFINE DSiHasUInt64}{$IFEND}
  {$IF CompilerVersion < 21}{$DEFINE DSiNeedUSHORT}{$IFEND}
  {$IF CompilerVersion < 18.5}{$DEFINE DSiNeedULONGEtc}{$IFEND}
{$ENDIF}
{$IFDEF Unicode}{$UNDEF DSiNeedRawByteString}{$ENDIF}

{$ALIGN ON}
{$MINENUMSIZE 4}

uses
  {$IFDEF DSiScopedUnitNames}Winapi.Windows{$ELSE}Windows{$ENDIF},
  {$IFDEF DSiScopedUnitNames}Winapi.Messages{$ELSE}Messages{$ENDIF},
  {$IFDEF DSiNeedVariants}
  {$IFDEF DSiScopedUnitNames}System.Variants{$ELSE}Variants{$ENDIF},
  {$ENDIF}
  {$IFDEF DSiNeedFileCtrl}
  FileCtrl, // use before SysUtils so deprecated functions from FileCtrl can be reintroduced
  {$ENDIF DSiNeedFileCtrl}
  {$IFDEF DSiScopedUnitNames}
  System.UITypes,
  {$ENDIF}
  {$IFDEF DSiScopedUnitNames}System.SysUtils{$ELSE}SysUtils{$ENDIF},
  {$IFDEF DSiScopedUnitNames}System.IOUtils,{$ENDIF}
  {$IFDEF DSiScopedUnitNames}System.StrUtils,{$ENDIF}
  {$IFDEF DSiScopedUnitNames}Winapi.ShellAPI{$ELSE}ShellAPI{$ENDIF},
  {$IFDEF DSiScopedUnitNames}Winapi.ShlObj{$ELSE}ShlObj{$ENDIF},
  {$IFDEF DSiScopedUnitNames}System.Classes{$ELSE}Classes{$ENDIF},
  {$IFDEF DSiScopedUnitNames}System.Contnrs{$ELSE}Contnrs{$ENDIF},
  {$IFDEF DSiScopedUnitNames}System.Win.Registry{$ELSE}Registry{$ENDIF}
  {$IFDEF DSiUseAnsiStrings}, System.AnsiStrings{$ENDIF}
  {$IFDEF DSiHasGenerics}, {$IFDEF DSiScopedUnitNames}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF}{$ENDIF}
  ;

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
  {$EXTERNALSYM CSIDL_ADMINTOOLS}
  CSIDL_ADMINTOOLS              = $0030; //v5.0; <user name>\Start Menu\Programs\Administrative Tools
  {$EXTERNALSYM CSIDL_ALTSTARTUP}
  CSIDL_ALTSTARTUP              = $001D; //The file system directory that corresponds to the user's nonlocalized Startup program group.
  {$EXTERNALSYM CSIDL_APPDATA}
  CSIDL_APPDATA                 = $001A; //v4.71; Application Data, new for NT4
  {$EXTERNALSYM CSIDL_CDBURN_AREA}
  CSIDL_CDBURN_AREA             = $003B; //v6.0; The file system directory acting as a staging area for files waiting to be written to CD.
  {$EXTERNALSYM CSIDL_COMMON_ADMINTOOLS}
  CSIDL_COMMON_ADMINTOOLS       = $002F; //v5.0; All Users\Start Menu\Programs\Administrative Tools
  {$EXTERNALSYM CSIDL_COMMON_ALTSTARTUP}
  CSIDL_COMMON_ALTSTARTUP       = $001E; //The file system directory that corresponds to the nonlocalized Startup program group for all users.
  {$EXTERNALSYM CSIDL_COMMON_APPDATA}
  CSIDL_COMMON_APPDATA          = $0023; //v5.0; All Users\Application Data
  {$EXTERNALSYM CSIDL_COMMON_DESKTOPDIRECTORY}
  CSIDL_COMMON_DESKTOPDIRECTORY = $0019; //The file system directory that contains files and folders that appear on the desktop for all users.
  {$EXTERNALSYM CSIDL_COMMON_DOCUMENTS}
  CSIDL_COMMON_DOCUMENTS        = $002E; //All Users\Documents
  {$EXTERNALSYM CSIDL_COMMON_FAVORITES}
  CSIDL_COMMON_FAVORITES        = $001F; //The file system directory that serves as a common repository for favorite items common to all users.
  {$EXTERNALSYM CSIDL_COMMON_MUSIC}
  CSIDL_COMMON_MUSIC            = $0035; //v6.0; The file system directory that serves as a repository for music files common to all users.
  {$EXTERNALSYM CSIDL_COMMON_PICTURES}
  CSIDL_COMMON_PICTURES         = $0036; //v6.0; The file system directory that serves as a repository for image files common to all users.
  {$EXTERNALSYM CSIDL_COMMON_PROGRAMS}
  CSIDL_COMMON_PROGRAMS         = $0017; //The file system directory that contains the directories for the common program groups that appear on the Start menu for all users.
  {$EXTERNALSYM CSIDL_COMMON_STARTMENU}
  CSIDL_COMMON_STARTMENU        = $0016; //The file system directory that contains the programs and folders that appear on the Start menu for all users.
  {$EXTERNALSYM CSIDL_COMMON_STARTUP}
  CSIDL_COMMON_STARTUP          = $0018; //The file system directory that contains the programs that appear in the Startup folder for all users.
  {$EXTERNALSYM CSIDL_COMMON_TEMPLATES}
  CSIDL_COMMON_TEMPLATES        = $002D; //The file system directory that contains the templates that are available to all users.
  {$EXTERNALSYM CSIDL_COMMON_VIDEO}
  CSIDL_COMMON_VIDEO            = $0037; //v6.0; The file system directory that serves as a repository for video files common to all users.
  {$EXTERNALSYM CSIDL_COMPUTERSNEARME}
  CSIDL_COMPUTERSNEARME         = $003D; //The folder representing other machines in your workgroup.
  {$EXTERNALSYM CSIDL_CONNECTIONS}
  CSIDL_CONNECTIONS             = $0031; //The virtual folder representing Network Connections, containing network and dial-up connections.
  {$EXTERNALSYM CSIDL_COOKIES}
  CSIDL_COOKIES                 = $0021; //The file system directory that serves as a common repository for Internet cookies.
  {$EXTERNALSYM CSIDL_HISTORY}
  CSIDL_HISTORY                 = $0022; //The file system directory that serves as a common repository for Internet history items.
  {$EXTERNALSYM CSIDL_INTERNET}
  CSIDL_INTERNET                = $0001; //A viritual folder for Internet Explorer (icon on desktop).
  {$EXTERNALSYM CSIDL_INTERNET_CACHE}
  CSIDL_INTERNET_CACHE          = $0020; //v4.72; The file system directory that serves as a common repository for temporary Internet files.
  {$EXTERNALSYM CSIDL_LOCAL_APPDATA}
  CSIDL_LOCAL_APPDATA           = $001C; //v5.0; non roaming, user\Local Settings\Application Data
  {$EXTERNALSYM CSIDL_MYDOCUMENTS}
  CSIDL_MYDOCUMENTS             = $000C; //v6.0; The virtual folder representing the My Documents desktop item.
  {$EXTERNALSYM CSIDL_MYMUSIC}
  CSIDL_MYMUSIC                 = $000D; //The file system directory that serves as a common repository for music files.
  {$EXTERNALSYM CSIDL_MYPICTURES}
  CSIDL_MYPICTURES              = $0027; //v5.0; My Pictures, new for Win2K
  {$EXTERNALSYM CSIDL_MYVIDEO}
  CSIDL_MYVIDEO                 = $000E; //v6.0; The file system directory that serves as a common repository for video files.
  {$EXTERNALSYM CSIDL_PERSONAL}
  CSIDL_PERSONAL                = $0005; //v6.0; Equal to CSIDL_MYDOCUMENTS; previous; The file system directory used to physically store a user's common repository of documents.

  CSIDL_PHOTOALBUMS             = $0045; //Vista; The virtual folder used to store photo albums.
  CSIDL_PLAYLISTS               = $003F; //Vista; The virtual folder used to store play albums.

  {$EXTERNALSYM CSIDL_PRINTHOOD}
  CSIDL_PRINTHOOD               = $001B; //The file system directory that contains the link objects that can exist in the Printers virtual folder.
  {$EXTERNALSYM CSIDL_PROFILE}
  CSIDL_PROFILE                 = $0028; //v5.0; The user's profile folder.
  {$EXTERNALSYM CSIDL_PROGRAM_FILES}
  CSIDL_PROGRAM_FILES           = $0026; //v5.0; C:\Program Files
  {$EXTERNALSYM CSIDL_PROGRAM_FILES_COMMON}
  CSIDL_PROGRAM_FILES_COMMON    = $002B; //v5.0; C:\Program Files\Common
  {$EXTERNALSYM CSIDL_RESOURCES}
  CSIDL_RESOURCES               = $0038; //Vista; The file system directory that contains resource data.

  CSIDL_SAMPLE_MUSIC            = $0040; //Vista; The file system directory that contains sample music.
  CSIDL_SAMPLE_PICTURES         = $0042; //Vista; The file system directory that contains sample pictures.
  CSIDL_SAMPLE_PLAYLISTS        = $0041; //Vista; The file system directory that contains sample playlists.
  CSIDL_SAMPLE_VIDEOS           = $0043; //Vista; The file system directory that contains sample videos.

  {$EXTERNALSYM CSIDL_RECENT}
  CSIDL_RECENT                  = $0008; //Recent files

  {$EXTERNALSYM CSIDL_SYSTEM}
  CSIDL_SYSTEM                  = $0025; //v5.0; GetSystemDirectory()
  {$EXTERNALSYM CSIDL_WINDOWS}
  CSIDL_WINDOWS                 = $0024; //GetWindowsDirectory()
  {$EXTERNALSYM CSIDL_FLAG_DONT_UNEXPAND}
  CSIDL_FLAG_DONT_UNEXPAND = $2000; //Combine with another CSIDL constant to ensure expanding of environment variables.
  {$EXTERNALSYM CSIDL_FLAG_DONT_VERIFY}
  CSIDL_FLAG_DONT_VERIFY   = $4000; //Combine with another CSIDL constant, except for CSIDL_FLAG_CREATE, to return an unverified folder path-with no attempt to create or initialize the folder.
  {$EXTERNALSYM CSIDL_FLAG_CREATE}
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

  {$EXTERNALSYM COMPRESSION_FORMAT_NONE}
  COMPRESSION_FORMAT_NONE    = 0;
  {$EXTERNALSYM COMPRESSION_FORMAT_DEFAULT}
  COMPRESSION_FORMAT_DEFAULT = 1;

  {$EXTERNALSYM SPI_GETFOREGROUNDLOCKTIMEOUT}
  SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
  {$EXTERNALSYM SPI_SETFOREGROUNDLOCKTIMEOUT}
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
  {$EXTERNALSYM SHERB_NOCONFIRMATION}
  SHERB_NOCONFIRMATION = $00000001;
  {$EXTERNALSYM SHERB_NOPROGRESSUI}
  SHERB_NOPROGRESSUI   = $00000002;
  {$EXTERNALSYM SHERB_NOSOUND}
  SHERB_NOSOUND        = $00000004;

  // CurrentVersion registry key
  DSiWinVerKey9x = '\Software\Microsoft\Windows\CurrentVersion';
  DSiWinVerKeyNT = '\Software\Microsoft\Windows NT\CurrentVersion';
  DSiWinVerKeys: array [boolean] of string = (DSiWinVerKey9x, DSiWinVerKeyNT);

  // CPU IDs for the Affinity familiy of functions
  DSiCPUIDs = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz@$';

const
  // security constants needed in DSiIsAdmin
  {$EXTERNALSYM SECURITY_NT_AUTHORITY}
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  {$EXTERNALSYM SECURITY_BUILTIN_DOMAIN_RID}
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  {$EXTERNALSYM DOMAIN_ALIAS_RID_ADMINS}
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
  {$EXTERNALSYM DOMAIN_ALIAS_RID_USERS}
  DOMAIN_ALIAS_RID_USERS : DWORD = $00000221;
  {$EXTERNALSYM DOMAIN_ALIAS_RID_GUESTS}
  DOMAIN_ALIAS_RID_GUESTS: DWORD = $00000222;
  DOMAIN_ALIAS_RID_POWER_: DWORD = $00000223;
  {$EXTERNALSYM SE_GROUP_ENABLED}
  SE_GROUP_ENABLED = $00000004;

  //LCID values, http://msdn.microsoft.com/nb-no/goglobal/bb964664.aspx
  LCID_Afrikaans_SouthAfrica         = $0436;
  LCID_Albanian_Albania              = $041c;
  LCID_Alsatian                      = $0484;
  LCID_Amharic_Ethiopia              = $045e;
  LCID_Arabic_SaudiArabia            = $0401;
  LCID_Arabic_Algeria                = $1401;
  LCID_Arabic_Bahrain                = $3c01;
  LCID_Arabic_Egypt                  = $0c01;
  LCID_Arabic_Iraq                   = $0801;
  LCID_Arabic_Jordan                 = $2c01;
  LCID_Arabic_Kuwait                 = $3401;
  LCID_Arabic_Lebanon                = $3001;
  LCID_Arabic_Libya                  = $1001;
  LCID_Arabic_Morocco                = $1801;
  LCID_Arabic_Oman                   = $2001;
  LCID_Arabic_Qatar                  = $4001;
  LCID_Arabic_Syria                  = $2801;
  LCID_Arabic_Tunisia                = $1c01;
  LCID_Arabic_UAE                    = $3801;
  LCID_Arabic_Yemen                  = $2401;
  LCID_Armenian_Armenia              = $042b;
  LCID_Assamese                      = $044d;
  LCID_Azeri_Cyrillic                = $082c;
  LCID_Azeri_Latin                   = $042c;
  LCID_Bashkir                       = $046d;
  LCID_Basque                        = $042d;
  LCID_Belarusian                    = $0423;
  LCID_Bengali_India                 = $0445;
  LCID_Bengali_Bangladesh            = $0845;
  LCID_Bosnian_BosniaHerzegovina     = $141A;
  LCID_Breton                        = $047e;
  LCID_Bulgarian                     = $0402;
  LCID_Burmese                       = $0455;
  LCID_Catalan                       = $0403;
  LCID_Cherokee_UnitedStates         = $045c;
  LCID_Chinese_PRC                   = $0804;
  LCID_Chinese_Singapore             = $1004;
  LCID_Chinese_Taiwan                = $0404;
  LCID_Chinese_HongKongSAR           = $0c04;
  LCID_Chinese_MacaoSAR              = $1404;
  LCID_Corsican                      = $0483;
  LCID_Croatian                      = $041a;
  LCID_Croatian_BosniaHerzegovina    = $101a;
  LCID_Czech                         = $0405;
  LCID_Danish                        = $0406;
  LCID_Dari                          = $048c;
  LCID_Divehi                        = $0465;
  LCID_Dutch_Netherlands             = $0413;
  LCID_Dutch_Belgium                 = $0813;
  LCID_Edo                           = $0466;
  LCID_English_UnitedStates          = $0409;
  LCID_English_UnitedKingdom         = $0809;
  LCID_English_Australia             = $0c09;
  LCID_English_Belize                = $2809;
  LCID_English_Canada                = $1009;
  LCID_English_Caribbean             = $2409;
  LCID_English_HongKongSAR           = $3c09;
  LCID_English_India                 = $4009;
  LCID_English_Indonesia             = $3809;
  LCID_English_Ireland               = $1809;
  LCID_English_Jamaica               = $2009;
  LCID_English_Malaysia              = $4409;
  LCID_English_NewZealand            = $1409;
  LCID_English_Philippines           = $3409;
  LCID_English_Singapore             = $4809;
  LCID_English_SouthAfrica           = $1c09;
  LCID_English_Trinidad              = $2c09;
  LCID_English_Zimbabwe              = $3009;
  LCID_Estonian                      = $0425;
  LCID_Faroese                       = $0438;
  LCID_Farsi                         = $0429;
  LCID_Filipino                      = $0464;
  LCID_Finnish                       = $040b;
  LCID_French_France                 = $040c;
  LCID_French_Belgium                = $080c;
  LCID_French_Cameroon               = $2c0c;
  LCID_French_Canada                 = $0c0c;
  LCID_French_DRCongo                = $240c;
  LCID_French_CotedIvoire            = $300c;
  LCID_French_Haiti                  = $3c0c;
  LCID_French_Luxembourg             = $140c;
  LCID_French_Mali                   = $340c;
  LCID_French_Monaco                 = $180c;
  LCID_French_Morocco                = $380c;
  LCID_French_NorthAfrica            = $e40c;
  LCID_French_Reunion                = $200c;
  LCID_French_Senegal                = $280c;
  LCID_French_Switzerland            = $100c;
  LCID_French_WestIndies             = $1c0c;
  LCID_Frisian_Netherlands           = $0462;
  LCID_Fulfulde_Nigeria              = $0467;
  LCID_FYRO_Macedonian               = $042f;
  LCID_Galician                      = $0456;
  LCID_Georgian                      = $0437;
  LCID_German_Germany                = $0407;
  LCID_German_Austria                = $0c07;
  LCID_German_Liechtenstein          = $1407;
  LCID_German_Luxembourg             = $1007;
  LCID_German_Switzerland            = $0807;
  LCID_Greek                         = $0408;
  LCID_Greenlandic                   = $046f;
  LCID_Guarani_Paraguay              = $0474;
  LCID_Gujarati                      = $0447;
  LCID_Hausa_Nigeria                 = $0468;
  LCID_Hawaiian_UnitedStates         = $0475;
  LCID_Hebrew                        = $040d;
  LCID_Hindi                         = $0439;
  LCID_Hungarian                     = $040e;
  LCID_Ibibio_Nigeria                = $0469;
  LCID_Icelandic                     = $040f;
  LCID_Igbo_Nigeria                  = $0470;
  LCID_Indonesian                    = $0421;
  LCID_Inuktitut                     = $045d;
  LCID_Irish                         = $083c;
  LCID_Italian_Italy                 = $0410;
  LCID_Italian_Switzerland           = $0810;
  LCID_Japanese                      = $0411;
  LCID_Kiche                         = $0486;
  LCID_Kannada                       = $044b;
  LCID_Kanuri_Nigeria                = $0471;
  LCID_Kashmiri                      = $0860;
  LCID_Kashmiri_Arabic               = $0460;
  LCID_Kazakh                        = $043f;
  LCID_Khmer                         = $0453;
  LCID_Kinyarwanda                   = $0487;
  LCID_Konkani                       = $0457;
  LCID_Korean                        = $0412;
  LCID_Kyrgyz_Cyrillic               = $0440;
  LCID_Lao                           = $0454;
  LCID_Latin                         = $0476;
  LCID_Latvian                       = $0426;
  LCID_Lithuanian                    = $0427;
  LCID_Luxembourgish                 = $046e;
  LCID_Malay_Malaysia                = $043e;
  LCID_Malay_BruneiDarussalam        = $083e;
  LCID_Malayalam                     = $044c;
  LCID_Maltese                       = $043a;
  LCID_Manipuri                      = $0458;
  LCID_Maori_NewZealand              = $0481;
  LCID_Mapudungun                    = $0471;
  LCID_Marathi                       = $044e;
  LCID_Mohawk                        = $047c;
  LCID_Mongolian_Cyrillic            = $0450;
  LCID_Mongolian_Mongolian           = $0850;
  LCID_Nepali                        = $0461;
  LCID_Nepali_India                  = $0861;
  LCID_Norwegian_Bokmal              = $0414;
  LCID_Norwegian_Nynorsk             = $0814;
  LCID_Occitan                       = $0482;
  LCID_Oriya                         = $0448;
  LCID_Oromo                         = $0472;
  LCID_Papiamentu                    = $0479;
  LCID_Pashto                        = $0463;
  LCID_Polish                        = $0415;
  LCID_Portuguese_Brazil             = $0416;
  LCID_Portuguese_Portugal           = $0816;
  LCID_Punjabi                       = $0446;
  LCID_Punjabi_Pakistan              = $0846;
  LCID_Quecha_Bolivia                = $046B;
  LCID_Quecha_Ecuador                = $086B;
  LCID_Quecha_Peru                   = $0C6B;
  LCID_RhaetoRomanic                 = $0417;
  LCID_Romanian                      = $0418;
  LCID_Romanian_Moldava              = $0818;
  LCID_Russian                       = $0419;
  LCID_Russian_Moldava               = $0819;
  LCID_Sami_Lappish                  = $043b;
  LCID_Sanskrit                      = $044f;
  LCID_Scottish_Gaelic               = $043c;
  LCID_Sepedi                        = $046c;
  LCID_Serbian_Cyrillic              = $0c1a;
  LCID_Serbian_Latin                 = $081a;
  LCID_Sindhi_India                  = $0459;
  LCID_Sindhi_Pakistan               = $0859;
  LCID_Sinhalese_SriLanka            = $045b;
  LCID_Slovak                        = $041b;
  LCID_Slovenian                     = $0424;
  LCID_Somali                        = $0477;
  LCID_Sorbian                       = $042e;
  LCID_Spanish_Spain_ModernSort      = $0c0a;
  LCID_Spanish_Spain_TraditionalSort = $040a;
  LCID_Spanish_Argentina             = $2c0a;
  LCID_Spanish_Bolivia               = $400a;
  LCID_Spanish_Chile                 = $340a;
  LCID_Spanish_Colombia              = $240a;
  LCID_Spanish_CostaRica             = $140a;
  LCID_Spanish_DominicanRepublic     = $1c0a;
  LCID_Spanish_Ecuador               = $300a;
  LCID_Spanish_ElSalvador            = $440a;
  LCID_Spanish_Guatemala             = $100a;
  LCID_Spanish_Honduras              = $480a;
  LCID_Spanish_LatinAmerica          = $580a;
  LCID_Spanish_Mexico                = $080a;
  LCID_Spanish_Nicaragua             = $4c0a;
  LCID_Spanish_Panama                = $180a;
  LCID_Spanish_Paraguay              = $3c0a;
  LCID_Spanish_Peru                  = $280a;
  LCID_Spanish_PuertoRico            = $500a;
  LCID_Spanish_UnitedStates          = $540a;
  LCID_Spanish_Uruguay               = $380a;
  LCID_Spanish_Venezuela             = $200a;
  LCID_Sutu                          = $0430;
  LCID_Swahili                       = $0441;
  LCID_Swedish                       = $041d;
  LCID_Swedish_Finland               = $081d;
  LCID_Syriac                        = $045a;
  LCID_Tajik                         = $0428;
  LCID_Tamazight_Arabic              = $045f;
  LCID_Tamazight_Latin               = $085f;
  LCID_Tamil                         = $0449;
  LCID_Tatar                         = $0444;
  LCID_Telugu                        = $044a;
  LCID_Thai                          = $041e;
  LCID_Tibetan_Bhutan                = $0851;
  LCID_Tibetan_PRC                   = $0451;
  LCID_Tigrigna_Eritrea              = $0873;
  LCID_Tigrigna_Ethiopia             = $0473;
  LCID_Tsonga                        = $0431;
  LCID_Tswana                        = $0432;
  LCID_Turkish                       = $041f;
  LCID_Turkmen                       = $0442;
  LCID_Uighur_China                  = $0480;
  LCID_Ukrainian                     = $0422;
  LCID_Urdu                          = $0420;
  LCID_Urdu_India                    = $0820;
  LCID_Uzbek_Cyrillic                = $0843;
  LCID_Uzbek_Latin                   = $0443;
  LCID_Venda                         = $0433;
  LCID_Vietnamese                    = $042a;
  LCID_Welsh                         = $0452;
  LCID_Wolof                         = $0488;
  LCID_Xhosa                         = $0434;
  LCID_Yakut                         = $0485;
  LCID_Yi                            = $0478;
  LCID_Yiddish                       = $043d;
  LCID_Yoruba                        = $046a;
  LCID_Zulu                          = $0435;
  LCID_HID                           = $04ff;

  // Undocumented WM_SYSCOMMAND WPARAM.
  // http://users.atw.hu/delphicikk/listaz.php?id=353&oldal=15
  SC_RESIZELEFT        = $F001; // Resize from left
  SC_RESIZERIGHT       = $F002; // Resize from right
  SC_RESIZETOP         = $F003; // Resize from up
  SC_RESIZETOPLEFT     = $F004; // Lock the bottom right corner of the form, the up left corner move for resize
  SC_RESIZETOPRIGHT    = $F005; // Same from bottom left corner
  SC_RESIZEBOTTOM      = $F006; // Lock up right and left border, resize other
  SC_RESIZEBOTTOMLEFT  = $F007; // Lock up and right border, resize other border
  SC_RESIZEBOTTOMRIGHT = $F008; // Lock left and up border and resize other
  SC_DRAGMOVE          = $F009; // Drag from anywhere
  SC_MINIMIZE          = $F020; // Auto-Minimize Form
  SC_MAXIMIZE          = $F030; // Auto-Maximize Form
  SC_SCREENSAVER       = $F148; // Activate ScreenSaver
  SC_STARTBUTTON       = $F13E; // Activate StartButton

type
  {$IFDEF DSiNeedULONGEtc}
  ULONG_PTR = Cardinal;
  {$EXTERNALSYM ULONG_PTR}
  ULONGLONG = UInt64;
  {$EXTERNALSYM ULONGLONG}
  {$ENDIF}
  {$IFDEF DSiNeedUSHORT}
  USHORT = Word;
  {$EXTERNALSYM USHORT}
  {$ENDIF}
  {$IFNDEF DSiHasSizeT}
  SIZE_T = ULONG_PTR;
  {$ENDIF DSiHasSizeT}

  {$IFDEF DSiHasSafeNativeInt}
  DSiNativeInt = NativeInt;
  DSiNativeUInt = NativeUInt;
  {$ELSE}
  {$IFDEF CPUX64}! Implementation error{$ENDIF}
  DSiNativeInt = integer;
  DSiNativeUInt = cardinal;
  {$ENDIF}

  // API types not defined in Delphi 5
  {$EXTERNALSYM PWkstaInfo100}
  PWkstaInfo100 = ^TWkstaInfo100;
  _WKSTA_INFO_100 = record
    wki100_platform_id: DWORD;
    wki100_computername: LPWSTR;
    wki100_langroup: LPWSTR;
    wki100_ver_major: DWORD;
    wki100_ver_minor: DWORD;
  end;
  {$EXTERNALSYM _WKSTA_INFO_100}
  {$EXTERNALSYM TWkstaInfo100}
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
    PeakWorkingSetSize: SIZE_T;
    WorkingSetSize: SIZE_T;
    QuotaPeakPagedPoolUsage: SIZE_T;
    QuotaPagedPoolUsage: SIZE_T;
    QuotaPeakNonPagedPoolUsage: SIZE_T;
    QuotaNonPagedPoolUsage: SIZE_T;
    PagefileUsage: SIZE_T;
    PeakPagefileUsage: SIZE_T;
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
  TDSiEnumFilesCallback = {$IFDEF DSiHasAnonymousFunctions}reference to{$ENDIF}
    procedure(const longFileName: string)
    {$IFNDEF DSiHasAnonymousFunctions}of object{$ENDIF};
  // DSiEnumFilesEx callback
  TDSiEnumFilesExCallback = {$IFDEF DSiHasAnonymousFunctions}reference to{$ENDIF}
    procedure(const folder: string; S: TSearchRec;
    isAFolder: boolean; var stopEnum: boolean)
    {$IFNDEF DSiHasAnonymousFunctions}of object{$ENDIF};

  // DSiExecuteAndCapture callback
  TDSiOnNewLineCallback = {$IFDEF DSiHasAnonymousFunctions}reference to{$ENDIF}
    procedure(const line: string; var runningTimeLeft_sec: integer)
    {$IFNDEF DSiHasAnonymousFunctions}of object{$ENDIF};

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
  {$EXTERNALSYM KEY_WOW64_64KEY}
  KEY_WOW64_64KEY = $0100;

type
  {$IFDEF DSiNeedRawByteString}
  RawByteString = AnsiString;
  {$ENDIF DSiNeedRawByteString}

  TDSiRegistry = class(TRegistry)
  public
    function  ReadBinary(const name: string; const defval: RawByteString): RawByteString; overload;
    function  ReadBinary(const name: string; dataStream: TStream): boolean; overload;
    function  ReadBool(const name: string; defval: boolean): boolean;
    function  ReadDate(const name: string; defval: TDateTime): TDateTime;
    function  ReadInt64(const name: string; defval: int64): int64;
    function  ReadInteger(const name: string; defval: integer): integer;
    function  ReadString(const name, defval: string): string;
    procedure ReadStrings(const name: string; strings: TStrings);
    function  ReadVariant(const name: string; defval: variant): variant;
    procedure WriteBinary(const name: string; data: RawByteString); overload;
    procedure WriteBinary(const name: string; data: TStream); overload;
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

  procedure DSiRegisterUserFileAssoc(const extension, progID, description, defaultIcon,
    openCommand: string);
  procedure DSiUnregisterUserFileAssoc(const progID: string);

{ Files }

type
  TShFileOpFlag = (fofAllowUndo, fofFilesOnly, fofMultiDestFiles, fofNoConfirmation,
    fofNoConfirmMkDir, fofNoConnectedElements, fofNoErrorUI, fofNoRecursion,
    fofNoRecurseReparse, fofRenameOnCollision, fofSilent, fofSimpleProgress,
    fofWantMappingHandle, fofWantNukeWarning, fofNoUI);
  TShFileOpFlags = set of TShFileOpFlag;

const
  {$EXTERNALSYM FILE_LIST_DIRECTORY}
  FILE_LIST_DIRECTORY = $0001;
  FILE_SHARE_FULL     = FILE_SHARE_DELETE OR FILE_SHARE_READ OR FILE_SHARE_WRITE;

  {$EXTERNALSYM FILE_ACTION_ADDED}
  FILE_ACTION_ADDED            = $00000001;
  {$EXTERNALSYM FILE_ACTION_REMOVED}
  FILE_ACTION_REMOVED          = $00000002;
  {$EXTERNALSYM FILE_ACTION_MODIFIED}
  FILE_ACTION_MODIFIED         = $00000003;
  {$EXTERNALSYM FILE_ACTION_RENAMED_OLD_NAME}
  FILE_ACTION_RENAMED_OLD_NAME = $00000004;
  {$EXTERNALSYM FILE_ACTION_RENAMED_NEW_NAME}
  FILE_ACTION_RENAMED_NEW_NAME = $00000005;

  FOF_NOCONNECTEDELEMENTS = $2000;
  {$EXTERNALSYM FOF_NORECURSION}
  FOF_NORECURSION         = $1000;
  {$EXTERNALSYM FOF_NORECURSEREPARSE}
  FOF_NORECURSEREPARSE    = $8000;
  {$EXTERNALSYM FOF_WANTNUKEWARNING}
  FOF_WANTNUKEWARNING     = $4000;
  {$EXTERNALSYM FOF_NO_UI}
  FOF_NO_UI               =  FOF_SILENT OR FOF_NOCONFIRMATION OR FOF_NOERRORUI OR FOF_NOCONFIRMMKDIR;

  CShFileOpFlagMappings: array [TShFileOpFlag] of FILEOP_FLAGS = (FOF_ALLOWUNDO,
    FOF_FILESONLY, FOF_MULTIDESTFILES, FOF_NOCONFIRMATION, FOF_NOCONFIRMMKDIR,
    FOF_NOCONNECTEDELEMENTS, FOF_NOERRORUI, FOF_NORECURSION, FOF_NORECURSEREPARSE,
    FOF_RENAMEONCOLLISION, FOF_SILENT, FOF_SIMPLEPROGRESS, FOF_WANTMAPPINGHANDLE,
    FOF_WANTNUKEWARNING, FOF_NO_UI);

type
  TDSiFileInfo = class
  private
    FDepth    : integer;
    FFolder   : string;
    FSearchRec: TSearchRec;
  protected
    function GetFullName: string;
  public
    constructor Create(const folder: string; searchRec: TSearchRec; depth: integer);
    property Depth: integer read FDepth;
    property Folder: string read FFolder;
    property FullName: string read GetFullName;
    property SearchRec: TSearchRec read FSearchRec;
  end; { TDSiFileInfo }

  function  DSiCanWriteToFolder(const folderName: string): boolean;
  function  DSiCompressFile(fileHandle: THandle): boolean;
  function  DSiConnectToNetworkResource(const networkResource: string; const mappedLetter:
    string = ''; const username: string = ''; const password: string = ''): boolean;
  function  DSiCopyFileAnimated(ownerWindowHandle: THandle; sourceFile, destinationFile:
    string; var aborted: boolean; flags: TShFileOpFlags = [fofNoConfirmMkDir]): boolean;
  function  DSiCreateTempFolder: string;
  procedure DSiDeleteFiles(const folder, fileMask: string);
  function  DSiDeleteOnReboot(const fileName: string): boolean;
  procedure DSiDeleteTree(const folder: string; removeSubdirsOnly: boolean; allowRoot: boolean = false);
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
    maxEnumDepth: integer = 0;
    ignoreDottedFolders: boolean = false): integer;
  procedure DSiEnumFilesToSL(const fileMask: string; attr: integer; fileList: TStrings;
    storeFullPath: boolean = false; enumSubfolders: boolean = false;
    maxEnumDepth: integer = 0;
    ignoreDottedFolders: boolean = false);
  procedure DSiEnumFilesToOL(const fileMask: string; attr: integer;
    fileList: TObjectList {of TDSiFileInfo};
    enumSubfolders: boolean = false; maxEnumDepth: integer = 0;
    ignoreDottedFolders: boolean = false);
  function  DSiFileExistsW(const fileName: WideString): boolean;
  function  DSiFileExtensionIs(const fileName, extension: string): boolean; overload;
  function  DSiFileExtensionIs(const fileName: string; const extension: array of string): boolean; overload;
  {$IFDEF DSiHasGenerics}
  function  DSiFileExtensionIs(const fileName: string; const extension: TArray<string>): boolean; overload;
  {$ENDIF DSiHasGenerics}
  function  DSiFileSize(const fileName: string): int64;
  function  DSiFileSizeAndTime(const fileName: string; var dtModified_UTC: TDateTime; var size: int64): boolean;
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
  function  DSiAffinityMaskToString(affinityMask: DSiNativeUInt): string;
  function  DSiGetCurrentProcessHandle: THandle;
  function  DSiGetCurrentThreadHandle: THandle;
  function  DSiEnablePrivilege(const privilegeName: string): boolean;
  function  DSiExecute(const commandLine: string;
    visibility: integer = SW_SHOWDEFAULT; const workDir: string = '';
    wait: boolean = false): cardinal; overload;
  function  DSiExecute(const commandLine: string; var processInfo: TProcessInformation;
    visibility: integer = SW_SHOWDEFAULT; const workDir: string = '';
    creationFlags: DWORD = CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS): cardinal; overload;
  function  DSiExecuteAndCapture(const app: string; output: TStrings; const workDir: string;
    var exitCode: longword; waitTimeout_sec: integer = 15;
    onNewLine: TDSiOnNewLineCallback = nil;
    creationFlags: DWORD = CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS;
    const abortHandle: THandle = 0): cardinal;
  function  DSiExecuteAsAdmin(const path: string; const parameters: string = '';
    const directory: string = ''; parentWindow: THandle = 0;
    showWindow: integer = SW_NORMAL; wait: boolean = false): boolean;
  function  DSiExecuteAsUser(const commandLine, username, password: string;
    var winErrorCode: cardinal; const domain: string = '.';
    visibility: integer = SW_SHOWDEFAULT; const workDir: string = '';
    wait: boolean = false): cardinal; overload;
  function  DSiExecuteAsUser(const commandLine, username, password: string;
    var winErrorCode: cardinal; var processInfo: TProcessInformation;
    const domain: string = '.'; visibility: integer = SW_SHOWDEFAULT;
    const workDir: string = ''; wait: boolean = false;
    startInfo: PStartupInfo = nil): cardinal; overload;
  function  DSiExecuteInSession(sessionID: DWORD; const commandLine: string;
    var processInfo: TProcessInformation; workDir: string = ''): boolean;
  function  DSiGetProcessAffinity: string;
  function  DSiGetProcessAffinityMask: DSiNativeUInt;
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
  function  DSiGetSystemAffinityMask: DSiNativeUInt;
  function  DSiGetThreadAffinity: string;
  function  DSiGetThreadAffinityMask: DSiNativeUInt;
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
  function  DSiSetProcessAffinity(const affinity: string): string;
  function  DSiSetProcessPriorityClass(const processName: string;
    priority: DWORD): boolean;
  function  DSiSetThreadAffinity(const affinity: string): string;
  procedure DSiStopImpersonatingUser;
  function  DSiStringToAffinityMask(const affinity: string): DSiNativeUInt;
  function  DSiTerminateProcessById(processID: DWORD; closeWindowsFirst: boolean = true;
    maxWait_sec: integer = 10): boolean;
  procedure DSiTrimWorkingSet;
  function  DSiValidateProcessAffinity(const affinity: string): string;
  function  DSiValidateProcessAffinityMask(affinityMask: DSiNativeUInt): DSiNativeUInt;
  function  DSiValidateThreadAffinity(const affinity: string): string;
  function  DSiValidateThreadAffinityMask(affinityMask: DSiNativeUInt): DSiNativeUInt;
  procedure DSiYield;

{ Memory }

  procedure DSiFreePidl(pidl: PItemIDList);
  procedure DSiFreeMemAndNil(var mem: pointer);
  function  DSiGetGlobalMemoryStatus(var memoryStatus: TMemoryStatusEx): boolean;

{ Windows }

type
  TDSiExitWindows = (ewLogOff, ewForcedLogOff, ewPowerOff, ewForcedPowerOff, ewReboot,
    ewForcedReboot, ewShutdown, ewForcedShutdown);

  function  DSiAllocateHWnd(wndProcMethod: TWndMethod;
    style: cardinal = WS_EX_TOOLWINDOW OR WS_EX_NOACTIVATE;
    parentWindow: HWND = HWND_MESSAGE): HWND;
  procedure DSiDeallocateHWnd(wnd: HWND);
  function  DSiDisableStandby: boolean;
  procedure DSiDisableX(hwnd: THandle);
  procedure DSiEnableX(hwnd: THandle);
  function  DSiExitWindows(exitType: TDSiExitWindows): boolean;
  function  DSiFindWindow(const caption: string; const wndClass: string = ''): HWND;
  function  DSiForceForegroundWindow(hwnd: THandle;
    restoreFirst: boolean = true): boolean;
  function  DSiGetClassName(hwnd: THandle): string;
  function  DSiGetFocusedWindow: THandle;
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
    wvWin7, wvWinServer2008R2, wvWin7OrServer2008R2, wvWin8, wvWin81,
    wvWinServer2012, wvWinServer2012R2, wvWin10, wvWinServer2016);

  TDSiUIElement = (ueMenu, ueMessage, ueWindowCaption, ueStatus);

const
  CDSiWindowsVersionStr: array [TDSiWindowsVersion] of string = ('Unknown',
    'Windows 3.1', 'Windows 95', 'Windows 95 OSR 2', 'Windows 98',
    'Windows 98 SE', 'Windows Me', 'Windows 9x', 'Windows NT 3.5',
    'Windows NT 4', 'Windows 2000', 'Windows XP', 'Windows NT', 'Windows Server 2003',
    'Windows Vista', 'Windows Server 2008', 'Windows Server 2008 or Windows Vista SP1',
    'Windows 7', 'Windows Server 2008 R2', 'Windows 7 or Windows Server 2008 R2',
    'Windows 8', 'Windows 8.1', 'Windows Server 2012', 'Windows Server 2012 R2', 'Windows 10',
    'Windows Server 2016');

  {$EXTERNALSYM VER_SUITE_BACKOFFICE}
  VER_SUITE_BACKOFFICE     = $00000004; // Microsoft BackOffice components are installed.
  {$EXTERNALSYM VER_SUITE_BLADE}
  VER_SUITE_BLADE          = $00000400; // Windows Server 2003, Web Edition is installed.
  {$EXTERNALSYM VER_SUITE_COMPUTE_SERVER}
  VER_SUITE_COMPUTE_SERVER = $00004000; // Windows Server 2003, Compute Cluster Edition is installed.
  {$EXTERNALSYM VER_SUITE_DATACENTER}
  VER_SUITE_DATACENTER     = $00000080; // Windows Server 2008 Datacenter, Windows Server 2003, Datacenter Edition, or Windows 2000 Datacenter Server is installed.
  {$EXTERNALSYM VER_SUITE_ENTERPRISE}
  VER_SUITE_ENTERPRISE     = $00000002; // Windows Server 2008 Enterprise, Windows Server 2003, Enterprise Edition, or Windows 2000 Advanced Server is installed. Refer to the Remarks section for more information about this bit flag.
  {$EXTERNALSYM VER_SUITE_EMBEDDEDNT}
  VER_SUITE_EMBEDDEDNT     = $00000040; // Windows XP Embedded is installed.
  {$EXTERNALSYM VER_SUITE_PERSONAL}
  VER_SUITE_PERSONAL       = $00000200; // Windows Vista Home Premium, Windows Vista Home Basic, or Windows XP Home Edition is installed.
  {$EXTERNALSYM VER_SUITE_SINGLEUSERTS}
  VER_SUITE_SINGLEUSERTS   = $00000100; // Remote Desktop is supported, but only one interactive session is supported. This value is set unless the system is running in application server mode.
  {$EXTERNALSYM VER_SUITE_SMALLBUSINESS}
  VER_SUITE_SMALLBUSINESS  = $00000001; // Microsoft Small Business Server was once installed on the system, but may have been upgraded to another version of Windows. Refer to the Remarks section for more information about this bit flag.
  {$EXTERNALSYM VER_SUITE_SMALLBUSINESS_RESTRICTED}
  VER_SUITE_SMALLBUSINESS_RESTRICTED = $00000020; // Microsoft Small Business Server is installed with the restrictive client license in force. Refer to the Remarks section for more information about this bit flag.
  {$EXTERNALSYM VER_SUITE_STORAGE_SERVER}
  VER_SUITE_STORAGE_SERVER = $00002000; // Windows Storage Server 2003 R2 or Windows Storage Server 2003is installed.
  {$EXTERNALSYM VER_SUITE_TERMINAL}
  VER_SUITE_TERMINAL       = $00000010; // Terminal Services is installed. This value is always set.
  {$EXTERNALSYM VER_SUITE_WH_SERVER}
  VER_SUITE_WH_SERVER      = $00008000; // Windows Home Server is installed.

  {$EXTERNALSYM VER_NT_DOMAIN_CONTROLLER}
  VER_NT_DOMAIN_CONTROLLER = $0000002; // The system is a domain controller and the operating system is Windows Server 2008, Windows Server 2003, or Windows 2000 Server.
  {$EXTERNALSYM VER_NT_SERVER}
  VER_NT_SERVER            = $0000003; // The operating system is Windows Server 2008, Windows Server 2003, or Windows 2000 Server.
                                       // Note that a server that is also a domain controller is reported as VER_NT_DOMAIN_CONTROLLER, not VER_NT_SERVER.
  {$EXTERNALSYM VER_NT_WORKSTATION}
  VER_NT_WORKSTATION       = $0000001; // The operating system is Windows Vista, Windows XP Professional, Windows XP Home Edition, or Windows 2000 Professional.

type
  {$IFNDEF Unicode}
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
  {$ENDIF}

  {$IFNDEF DSiNeedStartupInfo}
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
  {$ENDIF DSiNeedStartupInfo}

  _LOGICAL_PROCESSOR_RELATIONSHIP = (RelationProcessorCore{ = 0}, RelationNumaNode{ = 1}, RelationCache{ = 2}, RelationProcessorPackage{ = 3}, RelationGroup{ = 4}, RelationAll = $FFFF);
  {$EXTERNALSYM _LOGICAL_PROCESSOR_RELATIONSHIP}
  LOGICAL_PROCESSOR_RELATIONSHIP = _LOGICAL_PROCESSOR_RELATIONSHIP;
  {$EXTERNALSYM LOGICAL_PROCESSOR_RELATIONSHIP}
  TLogicalProcessorRelationship = LOGICAL_PROCESSOR_RELATIONSHIP;

  _PROCESSOR_CACHE_TYPE = (CacheUnified{ = 0}, CacheInstruction{ = 1}, CacheData{ = 2}, CacheTrace{ = 3});
  {$EXTERNALSYM _PROCESSOR_CACHE_TYPE}
  PROCESSOR_CACHE_TYPE = _PROCESSOR_CACHE_TYPE;
  {$EXTERNALSYM PROCESSOR_CACHE_TYPE}
  TProcessorCacheType = PROCESSOR_CACHE_TYPE;

  _CACHE_DESCRIPTOR = record
    Level: BYTE;
    Associativity: BYTE;
    LineSize: WORD;
    Size: DWORD;
    _Type: PROCESSOR_CACHE_TYPE;
  end;
  {$EXTERNALSYM _CACHE_DESCRIPTOR}
  CACHE_DESCRIPTOR = _CACHE_DESCRIPTOR;
  {$EXTERNALSYM CACHE_DESCRIPTOR}
  PCACHE_DESCRIPTOR = ^_CACHE_DESCRIPTOR;
  {$EXTERNALSYM PCACHE_DESCRIPTOR}
  TCacheDescriptor = _CACHE_DESCRIPTOR;
  PCacheDescriptor = PCACHE_DESCRIPTOR;

  _SYSTEM_LOGICAL_PROCESSOR_INFORMATION = record
    ProcessorMask: ULONG_PTR;
    Relationship: LOGICAL_PROCESSOR_RELATIONSHIP;
    case Integer of
      0: (Flags: BYTE); // ProcessorCore
      1: (NodeNumber: DWORD); // NumaNode
      2: (Cache: CACHE_DESCRIPTOR); //Cache
      3: (Reserved: array [0..1] of ULONGLONG);
  end;
  {$EXTERNALSYM _SYSTEM_LOGICAL_PROCESSOR_INFORMATION}
  SYSTEM_LOGICAL_PROCESSOR_INFORMATION = _SYSTEM_LOGICAL_PROCESSOR_INFORMATION;
  {$EXTERNALSYM SYSTEM_LOGICAL_PROCESSOR_INFORMATION}
  PSYSTEM_LOGICAL_PROCESSOR_INFORMATION = ^SYSTEM_LOGICAL_PROCESSOR_INFORMATION;
  {$EXTERNALSYM PSYSTEM_LOGICAL_PROCESSOR_INFORMATION}
  TSystemLogicalProcessorInformation = SYSTEM_LOGICAL_PROCESSOR_INFORMATION;
  PSystemLogicalProcessorInformation = PSYSTEM_LOGICAL_PROCESSOR_INFORMATION;
  TSystemLogicalProcessorInformationArr = array of TSystemLogicalProcessorInformation;

{$IFNDEF DSiHasGroupAffinity}
  KAFFINITY = ULONG_PTR;

  _GROUP_AFFINITY = record
      Mask: KAFFINITY;
      Group: WORD;
      Reserved: array[0..2] of WORD;
  end;
  {$EXTERNALSYM _GROUP_AFFINITY}
  GROUP_AFFINITY = _GROUP_AFFINITY;
  {$EXTERNALSYM GROUP_AFFINITY}
  PGROUP_AFFINITY = ^_GROUP_AFFINITY;
  {$EXTERNALSYM PGROUP_AFFINITY}
  TGroupAffinity = _GROUP_AFFINITY;
  PGroupAffinity = PGROUP_AFFINITY;

  _PROCESSOR_GROUP_INFO = record
    MaximumProcessorCount: BYTE;
    ActiveProcessorCount: BYTE;
    Reserved: array[0..37] of BYTE;
    ActiveProcessorMask: KAFFINITY;
  end;
  {$EXTERNALSYM _PROCESSOR_GROUP_INFO}
  PROCESSOR_GROUP_INFO = _PROCESSOR_GROUP_INFO;
  {$EXTERNALSYM PROCESSOR_GROUP_INFO}
  PPROCESSOR_GROUP_INFO = ^_PROCESSOR_GROUP_INFO;
  {$EXTERNALSYM PPROCESSOR_GROUP_INFO}
  TProcessorGroupInfo = _PROCESSOR_GROUP_INFO;
  PProcessorGroupInfo = PPROCESSOR_GROUP_INFO;

  _PROCESSOR_RELATIONSHIP = record
    Flags: BYTE;
    Reserved: array[0..20] of BYTE;
    GroupCount: WORD;
    GroupMask: array[0..0] of GROUP_AFFINITY;
  end;
  {$EXTERNALSYM _PROCESSOR_RELATIONSHIP}
  PROCESSOR_RELATIONSHIP = _PROCESSOR_RELATIONSHIP;
  {$EXTERNALSYM PROCESSOR_RELATIONSHIP}
  PPROCESSOR_RELATIONSHIP = ^_PROCESSOR_RELATIONSHIP;
  {$EXTERNALSYM PPROCESSOR_RELATIONSHIP}
  TProcessorRelationship = _PROCESSOR_RELATIONSHIP;
  PProcessorRelationship = PPROCESSOR_RELATIONSHIP;

  _NUMA_NODE_RELATIONSHIP = record
    NodeNumber: DWORD;
    Reserved: array[0..19] of BYTE;
    GroupMask: GROUP_AFFINITY;
  end;
  {$EXTERNALSYM _NUMA_NODE_RELATIONSHIP}
  NUMA_NODE_RELATIONSHIP = _NUMA_NODE_RELATIONSHIP;
  {$EXTERNALSYM NUMA_NODE_RELATIONSHIP}
  PNUMA_NODE_RELATIONSHIP = ^_NUMA_NODE_RELATIONSHIP;
  {$EXTERNALSYM PNUMA_NODE_RELATIONSHIP}
  TNumaNodeRelationship = _NUMA_NODE_RELATIONSHIP;
  PNumaNodeRelationship = PNUMA_NODE_RELATIONSHIP;

  _CACHE_RELATIONSHIP = record
    Level: BYTE;
    Associativity: BYTE;
    LineSize: WORD;
    CacheSize: DWORD;
    _Type: PROCESSOR_CACHE_TYPE;
    Reserved: array[0..19] of BYTE;
    GroupMask: GROUP_AFFINITY;
  end;
  {$EXTERNALSYM _CACHE_RELATIONSHIP}
  CACHE_RELATIONSHIP = _CACHE_RELATIONSHIP;
  {$EXTERNALSYM CACHE_RELATIONSHIP}
  PCACHE_RELATIONSHIP = ^_CACHE_RELATIONSHIP;
  {$EXTERNALSYM PCACHE_RELATIONSHIP}
  TCacheRelationship = _CACHE_RELATIONSHIP;
  PCacheRelationship = PCACHE_RELATIONSHIP;

  _GROUP_RELATIONSHIP = record
    MaximumGroupCount: WORD;
    ActiveGroupCount: WORD;
    Reserved: array[0..19] of BYTE;
    GroupInfo: array[0..0] of PROCESSOR_GROUP_INFO;
  end;
  {$EXTERNALSYM _GROUP_RELATIONSHIP}
  GROUP_RELATIONSHIP = _GROUP_RELATIONSHIP;
  {$EXTERNALSYM GROUP_RELATIONSHIP}
  PGROUP_RELATIONSHIP = ^_GROUP_RELATIONSHIP;
  {$EXTERNALSYM PGROUP_RELATIONSHIP}
  TGroupRelationship = _GROUP_RELATIONSHIP;
  PGroupRelationship = PGROUP_RELATIONSHIP;

  _SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX = record
    Relationship: LOGICAL_PROCESSOR_RELATIONSHIP;
    Size: DWORD;
    case Integer of
      0: (Processor: PROCESSOR_RELATIONSHIP);
      1: (NumaNode: NUMA_NODE_RELATIONSHIP);
      2: (Cache: CACHE_RELATIONSHIP);
      3: (Group: GROUP_RELATIONSHIP);
  end;
  {$EXTERNALSYM _SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX}
  SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX = _SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX;
  {$EXTERNALSYM SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX}
  PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX = ^_SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX;
  {$EXTERNALSYM PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX}
  TSystemLogicalProcessorInformationEx = _SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX;
  PSystemLogicalProcessorInformationEx = PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX;

{$ENDIF ~DSiHasGroupAffinity}

// Imagehlp.dll
const
  CERT_SECTION_TYPE_ANY = $FF;      // Any Certificate type

// Crypt32.dll
const
  {$EXTERNALSYM CERT_NAME_SIMPLE_DISPLAY_TYPE}
  CERT_NAME_SIMPLE_DISPLAY_TYPE = 4;
  {$EXTERNALSYM PKCS_7_ASN_ENCODING}
  PKCS_7_ASN_ENCODING = $00010000;
  {$EXTERNALSYM X509_ASN_ENCODING}
  X509_ASN_ENCODING = $00000001;

type
  PCCERT_CONTEXT = type Pointer;
  HCRYPTPROV_LEGACY = type Pointer;
  PFN_CRYPT_GET_SIGNER_CERTIFICATE = type Pointer;

  CRYPT_VERIFY_MESSAGE_PARA = record
    cbSize: DWORD;
    dwMsgAndCertEncodingType: DWORD;
    hCryptProv: HCRYPTPROV_LEGACY;
    pfnGetSignerCertificate: PFN_CRYPT_GET_SIGNER_CERTIFICATE;
    pvGetArg: Pointer;
  end;

// WinTrust.dll
const
  WINTRUST_ACTION_GENERIC_VERIFY_V2: TGUID = '{00AAC56B-CD44-11d0-8CC2-00C04FC295EE}';
  {$EXTERNALSYM WTD_CHOICE_FILE}
  WTD_CHOICE_FILE = 1;
  {$EXTERNALSYM WTD_REVOKE_NONE}
  WTD_REVOKE_NONE = 0;
  {$EXTERNALSYM WTD_UI_NONE}
  WTD_UI_NONE = 2;

type
  PWinTrustFileInfo = ^TWinTrustFileInfo;
  TWinTrustFileInfo = record
    cbStruct: DWORD;                    // = sizeof(WINTRUST_FILE_INFO)
    pcwszFilePath: PWideChar;           // required, file name to be verified
    hFile: THandle;                     // optional, open handle to pcwszFilePath
    pgKnownSubject: PGUID;              // optional: fill if the subject type is known
  end;

  PWinTrustData = ^TWinTrustData;
  TWinTrustData = record
    cbStruct: DWORD;
    pPolicyCallbackData: Pointer;
    pSIPClientData: Pointer;
    dwUIChoice: DWORD;
    fdwRevocationChecks: DWORD;
    dwUnionChoice: DWORD;
    pFile: PWinTrustFileInfo;
    dwStateAction: DWORD;
    hWVTStateData: THandle;
    pwszURLReference: PWideChar;
    dwProvFlags: DWORD;
    dwUIContext: DWORD;
  end;

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
  function  DSiGetLogicalProcessorInfo(var info: TSystemLogicalProcessorInformationArr): boolean;
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
  function  DSiHasRoamingProfile(var userHasRoamingProfile: boolean): boolean;
  function  DSiIsAdmin: boolean;
  function  DSiIsAdminLoggedOn: boolean;
  function  DSiIsCodeSigned(const exeFileName: string; var certName: AnsiString): boolean;
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
    const description: string = ''; const grouping: string = '';
    const serviceName: string = ''; protocols: TDSiFwIPProtocols = [fwProtoTCP];
    const localPorts: string = '*'; profiles: TDSiFwIPProfiles = [fwProfileAll]): boolean;
  function  DSiAddApplicationToFirewallExceptionListAdvanced(const entryName,
    applicationFullPath: string; resolveConflict: TDSiFwResolveConflict = rcDuplicate;
    const description: string = ''; const grouping: string = '';
    const serviceName: string = ''; protocols: TDSiFwIPProtocols = [fwProtoTCP];
    const localPorts: string = '*'; profiles: TDSiFwIPProfiles = [fwProfileAll]): boolean;
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
  function  DSiGetProcessSID(var sid: string): boolean;
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

  //Following three functions are based on GetTickCount
  function  DSiElapsedSince(midTime, startTime: int64): int64;
  function  DSiElapsedTime(startTime: int64): int64;
  function  DSiHasElapsed(startTime: int64; timeout_ms: DWORD): boolean;

  function  DSiTimeGetTime64: int64;
  function  DSiElapsedTime64(startTime: int64): int64;
  function  DSiHasElapsed64(startTime: int64; timeout_ms: DWORD): boolean;

  function  DSiDateTimeToFileTime(dateTime: TDateTime; var fileTime: TFileTime): boolean;
  function  DSiFileTimeToDateTime(fileTime: TFileTime): TDateTime; overload;
  function  DSiFileTimeToDateTime(fileTime: TFileTime; var dateTime: TDateTime): boolean; overload;
  function  DSiFileTimeToMicroSeconds(fileTime: TFileTime): int64;
  function  DSiPerfCounterToMS(perfCounter: int64): int64;
  function  DSiPerfCounterToUS(perfCounter: int64): int64;
  function  DSiQueryPerfCounterAsUS: int64;
  procedure DSiuSecDelay(delay: int64);

  function  DSiGetSystemTimePreciseAsFileTime(var fileTime: TFileTime): boolean;

{ Interlocked }

function  DSiInterlockedDecrement64(var addend: int64): int64; register;
function  DSiInterlockedIncrement64(var addend: int64): int64; register;
function  DSiInterlockedExchangeAdd64(var addend: int64; value: int64): int64; register;
function  DSiInterlockedExchange64(var target: int64; value: int64): int64; register;
function  DSiInterlockedCompareExchange64(var destination: int64; exchange, comparand: int64): int64; register; overload;
function  DSiInterlockedCompareExchange64(destination: PInt64; exchange, comparand: int64): int64; register; overload;

{ DynaLoad }

const // composition action values for DSiDwmEnableComposition
  {$EXTERNALSYM DWM_EC_DISABLECOMPOSITION}
  DWM_EC_DISABLECOMPOSITION = 0;
  {$EXTERNALSYM DWM_EC_ENABLECOMPOSITION}
  DWM_EC_ENABLECOMPOSITION = 1;

type
  PModule = ^HMODULE;

  function  DSi9xNetShareAdd(serverName: PChar; shareLevel: smallint;
    buffer: pointer; size: word): integer; stdcall;
  function  DSi9xNetShareDel(serverName: PChar; netName: PChar;
    reserved: word): integer; stdcall;
  function  DSiCertCreateCertificateContext(dwCertEncodingType: DWORD;
    pbCertEncoded: PBYTE; cbCertEncoded: DWORD): PCCERT_CONTEXT; stdcall;
  function  DSiCertFreeCertificateContext(pCertContext: PCCERT_CONTEXT): BOOL; stdcall;
  function  DSiCertGetNameStringA(pCertContext: PCCERT_CONTEXT; dwType: DWORD; dwFlags: DWORD;
    pvTypePara: Pointer; pszNameString: PAnsiChar; cchNameString: DWORD): DWORD; stdcall;
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
  function  DSiCryptVerifyMessageSignature(const pVerifyPara: CRYPT_VERIFY_MESSAGE_PARA;
    dwSignerIndex: DWORD; pbSignedBlob: PByte; cbSignedBlob: DWORD; pbDecoded: PBYTE;
    pcbDecoded: PDWORD; ppSignerCert: PCCERT_CONTEXT): BOOL; stdcall; //external 'Crypt32.dll';
  function  DSiDestroyEnvironmentBlock(lpEnvironment: pointer): BOOL;
  function  DSiDwmEnableComposition(uCompositionAction: UINT): HRESULT; stdcall;
  function  DSiDwmIsCompositionEnabled(var pfEnabled: BOOL): HRESULT; stdcall;
  function  DSiEnumProcessModules(hProcess: THandle; lphModule: PModule; cb: DWORD;
    var lpcbNeeded: DWORD): BOOL; stdcall;
  function  DSiGetLogicalProcessorInformation(
    pBuffer: PSYSTEM_LOGICAL_PROCESSOR_INFORMATION;
    var ReturnLength: DWORD): BOOL; stdcall;
  function  DSiGetLogicalProcessorInformationEx(
    RelationshipType: LOGICAL_PROCESSOR_RELATIONSHIP;
    Buffer: PSYSTEM_LOGICAL_PROCESSOR_INFORMATION;
    var ReturnedLength: DWORD): BOOL; stdcall;
  function  DSiGetModuleFileNameEx(hProcess: THandle; hModule: HMODULE; lpFilename: PChar;
    nSize: DWORD): DWORD; stdcall;
  function  DSiGetProcAddress(const libFileName, procName: string): FARPROC;
  function  DSiGetProcessImageFileName(hProcess: THandle; lpImageFileName: PChar;
    nSize: DWORD): DWORD; stdcall;
  function  DSiGetProcessMemoryInfo(process: THandle; memCounters: PProcessMemoryCounters;
    cb: DWORD): boolean; stdcall;
  function  DSiGetSystemFirmwareTable(FirmwareTableProviderSignature: DWORD;
    FirmwareTableID: DWORD; pFirmwareTableBuffer: pointer; BufferSize: DWORD): UInt; stdcall;
  function  DSiGetThreadGroupAffinity(hThread: THandle; var GroupAffinity: TGroupAffinity): BOOL; stdcall;
  function  DSiGetTickCount64: int64; stdcall;
  function  DSiGetUserProfileDirectoryW(hToken: THandle; lpProfileDir: PWideChar;
    var lpcchSize: DWORD): BOOL; stdcall;
  function  DSiGlobalMemoryStatusEx(memStatus: PMemoryStatusEx): boolean; stdcall;
  function  DSiImageEnumerateCertificates(FileHandle: THandle; TypeFilter: WORD;
    out CertificateCount: DWORD; Indices: PDWORD; IndexCount: Integer): BOOL; stdcall;
  function  DSiImageGetCertificateData(FileHandle: THandle; CertificateIndex: Integer;
    Certificate: PWinCertificate; var RequiredLength: DWORD): BOOL; stdcall;
  function  DSiImageGetCertificateHeader(FileHandle: THandle; CertificateIndex: Integer;
    var CertificateHeader: TWinCertificate): BOOL; stdcall;
  function  DSiImpersonateLoggedOnUser(hToken: THandle): BOOL; stdcall;
  function  DSiIsWow64Process(hProcess: THandle; var wow64Process: BOOL): BOOL; stdcall;
  function  DSiLogonUser(lpszUsername, lpszDomain, lpszPassword: PChar;
    dwLogonType, dwLogonProvider: DWORD; var phToken: THandle): BOOL; stdcall;
  function  DSiNetApiBufferFree(buffer: pointer): cardinal; stdcall;
  function  DSiNetWkstaGetInfo(servername: PChar; level: cardinal;
    out bufptr: pointer): cardinal; stdcall;
  function  DSiGetNumaHighestNodeNumber(var HighestNodeNunber: ULONG): BOOL; stdcall;
  function  DSiGetNumaProximityNodeEx(ProximityId: ULONG;
    var NodeNumber: USHORT): BOOL; stdcall;
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
  function  DSiSetThreadGroupAffinity(hThread: THandle; const GroupAffinity: TGroupAffinity;
    PreviousGroupAffinity: PGroupAffinity): BOOL; stdcall;
  function  DSiSHEmptyRecycleBin(Wnd: HWND; pszRootPath: PChar;
    dwFlags: DWORD): HRESULT; stdcall;
  function  DSiWinVerifyTrust(hwnd: HWND; const ActionID: TGUID;
    ActionData: Pointer): Longint; stdcall;
  function  DSiWow64DisableWow64FsRedirection(var oldStatus: pointer): BOOL; stdcall;
  function  DSiWow64RevertWow64FsRedirection(const oldStatus: pointer): BOOL; stdcall;
  function  DSiWTSQueryUserToken(sessionId: ULONG; var phToken: THandle): BOOL; stdcall;

{ Helpers }

{$IFDEF DSiNeedUTF}
// UTF <-> 16-bit conversion. Same signature as D7 functions but custom implementation
// (taken from http://gp.17slon.com/gp/gptextstream.htm with permission).

type
  UTF8String = type string;
  PUTF8String = ^UTF8String;

function UTF8Encode(const ws: WideString): UTF8String;
function UTF8Decode(const sUtf: UTF8String): WideString;
{$ENDIF DSiNeedUTF}

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
  TCertCreateCertificateContext = function(dwCertEncodingType: DWORD;
    pbCertEncoded: PBYTE; cbCertEncoded: DWORD): PCCERT_CONTEXT; stdcall;
  TCertFreeCertificateContext = function(pCertContext: PCCERT_CONTEXT): BOOL; stdcall;
  TCertGetNameStringA = function(pCertContext: PCCERT_CONTEXT; dwType: DWORD; dwFlags: DWORD;
    pvTypePara: Pointer; pszNameString: PAnsiChar; cchNameString: DWORD): DWORD; stdcall;
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
  TCreateEnvironmentBlock = function(var lpEnvironment: pointer; hToken: THandle;
    bInherit: BOOL): BOOL; stdcall;
  TCryptVerifyMessageSignature = function(const pVerifyPara: CRYPT_VERIFY_MESSAGE_PARA;
    dwSignerIndex: DWORD; pbSignedBlob: PByte; cbSignedBlob: DWORD; pbDecoded: PBYTE;
    pcbDecoded: PDWORD; ppSignerCert: PCCERT_CONTEXT): BOOL; stdcall;
  TDestroyEnvironmentBlock = function(lpEnvironment: pointer): BOOL; stdcall;
  TDwmEnableComposition = function(uCompositionAction: UINT): HRESULT; stdcall;
  TDwmIsCompositionEnabled = function(var pfEnabled: BOOL): HRESULT; stdcall;
  TEnumProcessModules = function(hProcess: THandle; lphModule: PModule; cb: DWORD;
    var lpcbNeeded: DWORD): BOOL; stdcall;
  TGetLongPathName = function(lpszShortPath, lpszLongPath: PChar;
    cchBuffer: DWORD): DWORD; stdcall;
  TGetLogicalProcessorInformation = function(
    pBuffer: PSYSTEM_LOGICAL_PROCESSOR_INFORMATION;
    var ReturnLength: DWORD): BOOL; stdcall;
  TGetLogicalProcessorInformationEx = function(
    RelationshipType: LOGICAL_PROCESSOR_RELATIONSHIP;
    Buffer: PSYSTEM_LOGICAL_PROCESSOR_INFORMATION;
    var ReturnedLength: DWORD): BOOL; stdcall;
  TGetModuleFileNameEx = function(hProcess: THandle; hModule: HMODULE; lpFilename: PChar;
    nSize: DWORD): DWORD; stdcall;
  TGetProcessImageFileName = function(hProcess: THandle; lpImageFileName: PChar;
    nSize: DWORD): DWORD; stdcall;
  TGetProcessMemoryInfo = function(process: THandle; memCounters: PProcessMemoryCounters;
    cb: DWORD): boolean; stdcall;
  TGetSystemFirmwareTable = function (FirmwareTableProviderSignature: DWORD;
    FirmwareTableID: DWORD; pFirmwareTableBuffer: pointer; BufferSize: DWORD): UInt; stdcall;
  TGetThreadGroupAffinity = function(hThread: THandle; var GroupAffinity: TGroupAffinity): BOOL; stdcall;
  TGetTickCount64 = function: int64; stdcall;
  TGetUserProfileDirectoryW = function(hToken: THandle; lpProfileDir: PWideChar;
    var lpcchSize: DWORD): BOOL; stdcall;
  TGlobalMemoryStatusEx = function(memStatus: PMemoryStatusEx): boolean; stdcall;
  TImageEnumerateCertificates = function(FileHandle: THandle; TypeFilter: WORD;
    out CertificateCount: DWORD; Indices: PDWORD; IndexCount: Integer): BOOL; stdcall;
  TImageGetCertificateData = function(FileHandle: THandle; CertificateIndex: Integer;
    Certificate: PWinCertificate; var RequiredLength: DWORD): BOOL; stdcall;
  TImageGetCertificateHeader = function(FileHandle: THandle; CertificateIndex: Integer;
    var CertificateHeader: TWinCertificate): BOOL; stdcall;
  TImpersonateLoggedOnUser = function(hToken: THandle): BOOL; stdcall;
  TIsWow64Process = function(hProcess: THandle; var wow64Process: BOOL): BOOL; stdcall;
  TLogonUser = function(lpszUsername, lpszDomain, lpszPassword: LPCSTR;
    dwLogonType, dwLogonProvider: DWORD; var phToken: THandle): BOOL; stdcall;
  TNetApiBufferFree = function(buffer: pointer): cardinal; stdcall;
  TNetWkstaGetInfo = function(servername: PChar; level: cardinal;
    out bufptr: pointer): cardinal; stdcall;
  TGetNumaHighestNodeNumber = function(var HighestNodeNunber: ULONG): BOOL; stdcall;
  TGetNumaProximityNodeEx = function (ProximityId: ULONG;
    var NodeNumber: USHORT): BOOL; stdcall;
  TGetSystemTimePreciseAsFileTime = procedure (var fileTime: TFileTime); stdcall;
  TNTNetShareAdd = function(serverName: PChar; level: integer; buf: PChar;
    var parm_err: integer): DWord; stdcall;
  TNTNetShareDel = function(serverName: PChar; netName: PWideChar;
    reserved: integer): DWord; stdcall;
  TOpenSCManager = function(lpMachineName, lpDatabaseName: PChar;
    dwDesiredAccess: DWORD): SC_HANDLE; stdcall;
  TRevertToSelf = function: BOOL; stdcall;
  TSetDllDirectory = function(path: PChar): boolean; stdcall;
  TSetSuspendState = function(hibernate, forceCritical, disableWakeEvent: BOOL): BOOL; stdcall;
  TSetThreadGroupAffinity = function(hThread: THandle; const GroupAffinity: TGroupAffinity;
    PreviousGroupAffinity: PGroupAffinity): BOOL; stdcall;
  TSHEmptyRecycleBin = function(wnd: HWND; pszRootPath: PChar;
    dwFlags: DWORD): HRESULT; stdcall;
  TWinVerifyTrust = function(hwnd: HWND; const ActionID: TGUID;
    ActionData: Pointer): Longint; stdcall;
  TWow64DisableWow64FsRedirection = function(var oldStatus: pointer): BOOL; stdcall;
  TWow64RevertWow64FsRedirection = function(const oldStatus: pointer): BOOL; stdcall;
  TWTSQueryUserToken = function(sessionId: ULONG; var phToken: THandle): BOOL; stdcall;

const
  G9xNetShareAdd: T9xNetShareAdd = nil;
  G9xNetShareDel: T9xNetShareDel = nil;
  GCertCreateCertificateContext: TCertCreateCertificateContext = nil;
  GCertFreeCertificateContext: TCertFreeCertificateContext = nil;
  GCertGetNameStringA: TCertGetNameStringA = nil;
  GCloseServiceHandle: TCloseServiceHandle = nil;
  GCreateProcessAsUser: TCreateProcessAsUser = nil;
  GCreateProcessWithLogonW: TCreateProcessWithLogonW = nil;
  GCreateEnvironmentBlock: TCreateEnvironmentBlock = nil;
  GCryptVerifyMessageSignature: TCryptVerifyMessageSignature = nil;
  GDestroyEnvironmentBlock: TDestroyEnvironmentBlock = nil;
  GDwmEnableComposition: TDwmEnableComposition = nil;
  GDwmIsCompositionEnabled: TDwmIsCompositionEnabled = nil;
  GEnumProcessModules: TEnumProcessModules = nil;
  GGetLogicalProcessorInformation: TGetLogicalProcessorInformation = nil;
  GGetLogicalProcessorInformationEx: TGetLogicalProcessorInformationEx = nil;
  GGetModuleFileNameEx: TGetModuleFileNameEx = nil;
  GGetLongPathName: TGetLongPathName = nil;
  GGetProcessImageFileName: TGetProcessImageFileName = nil;
  GGetProcessMemoryInfo: TGetProcessMemoryInfo = nil;
  GGetSystemFirmwareTable: TGetSystemFirmwareTable = nil;
  GGetThreadGroupAffinity: TGetThreadGroupAffinity = nil;
  GGetTickCount64: TGetTickCount64 = nil;
  GGetUserProfileDirectoryW: TGetUserProfileDirectoryW = nil;
  GGlobalMemoryStatusEx: TGlobalMemoryStatusEx = nil;
  GImageEnumerateCertificates: TImageEnumerateCertificates = nil;
  GImageGetCertificateData: TImageGetCertificateData = nil;
  GImageGetCertificateHeader: TImageGetCertificateHeader = nil;
  GImpersonateLoggedOnUser: TImpersonateLoggedOnUser = nil;
  GIsWow64Process: TIsWow64Process = nil;
  GLogonUser: TLogonUser = nil;
  GNetApiBufferFree: TNetApiBufferFree = nil;
  GNetWkstaGetInfo: TNetWkstaGetInfo = nil;
  GGetNumaHighestNodeNumber: TGetNumaHighestNodeNumber = nil;
  GGetNumaProximityNodeEx: TGetNumaProximityNodeEx = nil;
  GGetSystemTimePreciseAsFileTime: TGetSystemTimePreciseAsFileTime = nil;
  GNTNetShareAdd: TNTNetShareAdd = nil;
  GNTNetShareDel: TNTNetShareDel = nil;
  GOpenSCManager: TOpenSCManager = nil;
  GRevertToSelf: TRevertToSelf = nil;
  GSetDllDirectory: TSetDllDirectory = nil;
  GSetSuspendState: TSetSuspendState = nil;
  GSetThreadGroupAffinity: TSetThreadGroupAffinity = nil;
  GSHEmptyRecycleBin: TSHEmptyRecycleBin = nil;
  GWinVerifyTrust: TWinVerifyTrust = nil;
  GWow64DisableWow64FsRedirection: TWow64DisableWow64FsRedirection = nil;
  GWow64RevertWow64FsRedirection: TWow64RevertWow64FsRedirection = nil;
  GWTSQueryUserToken: TWTSQueryUserToken = nil;

{$IFOPT R+} {$DEFINE RestoreR} {$ELSE} {$UNDEF RestoreR} {$ENDIF}

{ Missing imports }

{$IF not Defined(ConvertSidToStringSid)}
function ConvertSidToStringSid(Sid: PSID; var StringSid: LPWSTR): BOOL; stdcall; external 'advapi32.dll' name 'ConvertSidToStringSidW';
{$IFEND}

{$IF not Defined(TOKEN_USER)}
type
  TOKEN_USER = record
    User : TSIDAndAttributes;
  end;
  PTokenUser = ^TOKEN_USER;
{$IFEND}

{ Helpers }

  function StrPasA(const Str: PAnsiChar): AnsiString;
  begin
    Result := {$IFDEF DSiUseAnsiStrings}System.AnsiStrings.{$ENDIF}StrPas(Str);
  end;

  procedure CreateProcessWatchdog(task: TBackgroundTask);
  begin
    TBackgroundThread.Create(task);
  end; { CreateProcessWatchdog }

  function FileOpenSafe(const fileName: string; var fileHandle: textfile;
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

{$IFDEF DSiNeedUTF}
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
    Result := DSiNativeUInt(pch)-DSiNativeUInt(@utf8Buf);
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
    Result := DSiNativeUInt(pwc)-DSiNativeUInt(@unicodeBuf);
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
{$ENDIF DSiNeedUTF}

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
  var
    sBool: string;
  begin
    try
      if GetDataSize(name) < 0 then
        Abort; // D4 does not generate an exception!
      case GetDataType(name) of
        rdString:
          begin
            sBool := ReadString(name, '');
            if sBool = '' then
              Result := defval
            else
              Result := SameText(sBool, 'true') or SameText(sBool, 't');
          end //rdString
        else Result := inherited ReadBool(name);
      end;
    except Result := defval; end;
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
      Result := inherited ReadDate(name);
    except Result := defval; end;
  end; { TDSiRegistry.ReadDate }

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
      varSmallInt,
      varShortInt,
      varWord,
      varLongWord,
      varInteger,
      {$IFDEF DSiHasUInt64}varUInt64,{$ENDIF}
      varInt64  : WriteInteger(name,value);
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

  {:Writes per-user file association info to the registry.
    See: http://stackoverflow.com/questions/6285791/how-to-associate-a-delphi-program-with-a-file-type-but-only-for-the-current-use
     and http://msdn.microsoft.com/en-us/library/windows/desktop/hh127451(v=vs.85).aspx
    @author  gabr
    @since   2014-07-24
  }
  procedure DSiRegisterUserFileAssoc(const extension, progID, description, defaultIcon,
    openCommand: string);
  var
    sExt: string;
  begin
    if Copy(extension, 1, 1) <> '.' then
      sExt := '.' + extension
    else
      sExt := extension;
    DSiWriteRegistry('\Software\Classes\'+progID, '', description);
    DSiWriteRegistry('\Software\Classes\'+progID+'\DefaultIcon', '', defaultIcon);
    DSiWriteRegistry('\Software\Classes\'+progID+'\shell\open\command', '', openCommand);
    DSiWriteRegistry('\Software\Classes\'+sExt, '', progID);
  end; { DSiRegisterUserFileAssoc }

  {:Removes per-user file association info from the registry.
    See DSiRegisterUserFileASsoc.
    @author  gabr
    @since   2014-07-24
  }
  procedure DSiUnregisterUserFileAssoc(const progID: string);
  begin
    DSiKillRegistry('\Software\Classes\' + progID, HKEY_CURRENT_USER);
  end; { DSiUnregisterUserFileAssoc }

{ Files }

  constructor TDSiFileInfo.Create(const folder: string; searchRec: TSearchRec; depth:
    integer);
  begin
    inherited Create;
    FFolder := folder;
  FSearchRec := searchRec;
  FDepth := depth;
  end; { TDSiFileInfo.Create }

  function TDSiFileInfo.GetFullName: string;
  begin
    Result := FFolder + FSearchRec.Name;
  end; { TDSiFileInfo.GetFullName }

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
  procedure DSiDeleteTree(const folder: string; removeSubdirsOnly, allowRoot: boolean);

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

    // Check that the code is not doing something extremely stupid
    procedure Validate(folder: string);
    {$IFDEF DSiHasTPath}
    var
      fullPath : string;
      p        : integer;
      pathOnly : string;
      pathRoot : string;
      sysRoot  : string;
      winFolder: string;
    {$ENDIF}
    begin
      folder := Trim(folder);

      if folder = '' then
        raise Exception.Create('DSiDeleteTree: Path is empty');

      {$IFDEF DSiHasTPath}//rest of tests depend on a TPath implementation
      if TPath.GetExtendedPrefix(folder) <> TPathPrefixType.pptNoPrefix then
        raise Exception.Create('DSiDeleteTree: Path starts with extended prefix');

      // split to drive/server + folder
      if TPath.IsUNCRooted(folder) then begin
        p := PosEx(TPath.DirectorySeparatorChar, folder, 3);
        if p = 0 then begin
          pathRoot := folder;
          folder := folder + TPath.DirectorySeparatorChar;
        end
        else
          pathRoot := Copy(folder, 1, p-1);
      end
      else begin
        pathRoot := TPath.GetPathRoot(folder);
        if EndsText(TPath.DirectorySeparatorChar, pathRoot) then
          Delete(pathRoot, Length(pathRoot), 1);
      end;
      pathOnly := folder;
      Delete(pathOnly, 1, Length(pathRoot));
      pathOnly := Trim(pathOnly);

      if not StartsText(TPath.DirectorySeparatorChar, pathOnly) then
        raise Exception.Create('DSiDeleteTree: Path is not in absolute format');

      winFolder := ExcludeTrailingPathDelimiter(DSiGetWindowsFolder);
      sysRoot := TPath.GetPathRoot(winFolder);
      if EndsText(TPath.DirectorySeparatorChar, sysRoot) then
        Delete(sysRoot, Length(sysRoot), 1);

      if (pathOnly = '') or (pathOnly = TPath.DirectorySeparatorChar) then begin // root only
        if SameText(sysRoot, pathRoot) then
          raise Exception.Create('DSiDeleteTree: System root path')
        else if not allowRoot then
          raise Exception.Create('DSiDeleteTree: Root path');
        Exit;
      end;

      fullPath := TPath.GetFullPath(folder);

      if StartsText(DSiGetTempPath, fullPath) then
        Exit; // OK if TEMP is inside Windows folder (old system)

      if StartsText(winFolder, fullPath) then
        raise Exception.Create('DSiDeleteTree: System path');
      {$ENDIF DSiHasTPath}
    end; { Validate }

  begin { DSiDeleteTree }
    Validate(folder);
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
    boolean; fileList: TStrings; fileObjectList: TObjectList; storeFullPath: boolean;
    currentDepth, maxDepth: integer; ignoreDottedFolders: boolean);
  var
    err: integer;
    s  : TSearchRec;
  begin
    if enumSubfolders and ((maxDepth <= 0) or (currentDepth < maxDepth)) then begin
      err := FindFirst(folder+'*.*', faDirectory or (attr and faHidden), S);
      if err = 0 then try
        repeat
          if (S.Attr and faDirectory) <> 0 then
            if (S.Name <> '.') and (S.Name <> '..') and
               ((S.Name[1] <> '.') or (not ignoreDottedFolders)) then
            begin
              if assigned(enumCallback) then begin
                enumCallback(folder, S, true, stopEnum);
                if stopEnum then
                  Exit;
              end;
              {$IFDEF DSiScopedUnitNames}
              if (fileMask = '') or TPath.MatchesPattern(S.Name, fileMask, false) then begin
              {$ENDIF DSiScopedUnitNames}
                if assigned(fileList) then
                  if storeFullPath then
                    fileList.Add(folder + S.Name + '\')
                  else
                    fileList.Add(S.Name + '\');
                if assigned(fileObjectList) then
                  fileObjectList.Add(TDSiFileInfo.Create(folder, S, currentDepth));
              {$IFDEF DSiScopedUnitNames}end;{$ENDIF}
              _DSiEnumFilesEx(folder+S.Name+'\', fileMask, attr, enumSubfolders,
                enumCallback, totalFiles, stopEnum, fileList, fileObjectList,
                storeFullPath, currentDepth + 1, maxDepth, ignoreDottedFolders);
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
        // don't filter anything
        //if (S.Attr AND attr <> 0) or (S.Attr AND attr = attr) then begin
        if (attr <> faDirectory)
           or ((attr = faDirectory) and (S.Attr AND attr = attr))
        then begin
          if assigned(enumCallback) then
            enumCallback(folder, S, false, stopEnum);
          if assigned(fileList) then
            if storeFullPath then
              fileList.Add(folder + S.Name)
            else
              fileList.Add(S.Name);
          if assigned(fileObjectList) then
            fileObjectList.Add(TDSiFileInfo.Create(folder, S, currentDepth));
          Inc(totalFiles);
        end;
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
    enumCallback: TDSiEnumFilesExCallback; maxEnumDepth: integer;
    ignoreDottedFolders: boolean): integer;
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
      nil, nil, false, 1, maxEnumDepth, ignoreDottedFolders);
  end; { DSiEnumFilesEx }

  {:Enumerates files (optionally in subfolders) and stores results into caller-provided
    TStrings object.
    @since   2006-05-14
  }
  procedure DSiEnumFilesToSL(const fileMask: string; attr: integer; fileList: TStrings;
    storeFullPath: boolean; enumSubfolders: boolean; maxEnumDepth: integer;
    ignoreDottedFolders: boolean);
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
      fileList, nil, storeFullPath, 1, maxEnumDepth, ignoreDottedFolders);
  end; { DSiEnumFilesToSL }

  {:Enumerates files (optionally in subfolders) and stores TDSiFileInfo objects into
    caller-provided TObjectList.
    @since   2012-05-16
  }
  procedure DSiEnumFilesToOL(const fileMask: string; attr: integer;
    fileList: TObjectList {of TDSiFileInfo};
    enumSubfolders: boolean; maxEnumDepth: integer;
    ignoreDottedFolders: boolean);
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
      nil, fileList, true{ignored}, 1, maxEnumDepth, ignoreDottedFolders);
  end; { DSiEnumFilesToOL }

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
  function DSiFileExtensionIs(const fileName: string; const extension: array of string):
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
          Exit;
      end
      else begin
        if SameText(fExtDot, testExt) then
          Exit;
      end;
    end;
    Result := false;
  end; { DSiFileExtensionIs }

{$IFDEF DSiHasGenerics}
  function  DSiFileExtensionIs(const fileName: string; const extension: TArray<string>): boolean; overload;
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
          Exit;
      end
      else begin
        if SameText(fExtDot, testExt) then
          Exit;
      end;
    end;
    Result := false;
  end; { DSiFileExtensionIs }
{$ENDIF DSiHasGenerics}

  {:Retrieves file size.
    @returns -1 for unexisting/unaccessible file or file size.
    @author  gabr
    @since   2003-06-17
  }
  function DSiFileSize(const fileName: string): int64;
  var
    fHandle: THandle;
  begin
    fHandle := CreateFile(PChar(fileName), 0,
      FILE_SHARE_READ OR FILE_SHARE_WRITE OR FILE_SHARE_DELETE, nil, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL, 0);
    if fHandle = INVALID_HANDLE_VALUE then
      Result := -1
    else try
      Int64Rec(Result).Lo := GetFileSize(fHandle, @Int64Rec(Result).Hi);
    finally CloseHandle(fHandle); end;
  end; { DSiFileSize }

  {:Retrieves file size and file time (last modification time).
    @returns -1 for unexisting/unaccessible file or file size.
    @author  gabr
    @since   2015-09-22
  }
  function DSiFileSizeAndTime(const fileName: string; var dtModified_UTC: TDateTime; var size: int64): boolean;
  var
    fileHandle            : THandle;
    fsCreationTime        : TFileTime;
    fsLastAccessTime      : TFileTime;
    fsLastModificationTime: TFileTime;
  begin
    Result := false;
    fileHandle := CreateFile(PChar(fileName), 0, FILE_SHARE_READ OR FILE_SHARE_WRITE OR FILE_SHARE_DELETE, nil, OPEN_EXISTING, 0, 0);
    if fileHandle <> INVALID_HANDLE_VALUE then
    try
      Int64Rec(size).Lo := GetFileSize(fileHandle, @Int64Rec(size).Hi);
      Result := GetFileTime(fileHandle, @fsCreationTime, @fsLastAccessTime, @fsLastModificationTime) and
        DSiFileTimeToDateTime(fsLastModificationTime, dtModified_UTC);
    finally CloseHandle(fileHandle); end;
  end; { DSiFileSizeAndTime }

  {:Wide version of DSiFileSize.
    @returns -1 for unexisting/unaccessible file or file size.
    @author  gabr
    @since   2006-08-14
  }
  function DSiFileSizeW(const fileName: WideString): int64;
  var
    fHandle: THandle;
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
    fileHandle            : THandle;
    fsCreationTime        : TFileTime;
    fsLastAccessTime      : TFileTime;
    fsLastModificationTime: TFileTime;
  begin
    Result := false;
    fileHandle := CreateFile(PChar(fileName), 0,
      FILE_SHARE_READ OR FILE_SHARE_WRITE OR FILE_SHARE_DELETE, nil,
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
    fileHandle            : THandle;
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

  {:Convert affinity mask into a string representation (0..9, A..Z, a..z, @, $).
    @author  gabr
    @since   2003-11-14
  }
  function DSiAffinityMaskToString(affinityMask: DSiNativeUInt): string;
  var
    idxID: integer;
  begin
    Result := '';
    for idxID := 1 to Length(DSiCPUIDs) do begin
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
  begin
    Result := DSiExecute(commandLine, processInfo, visibility, workDir);
    if Result = 0 then begin
      if wait then begin
        WaitForSingleObject(processInfo.hProcess, INFINITE);
        GetExitCodeProcess(processInfo.hProcess, Result);
      end;
      CloseHandle(processInfo.hProcess);
      CloseHandle(processInfo.hThread);
    end;
  end; { DSiExecute }

  function DSiExecute(const commandLine: string; var processInfo: TProcessInformation;
    visibility: integer; const workDir: string; creationFlags: DWORD): cardinal;
  var
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
             creationFlags, nil, PChar(useWorkDir), startupInfo, processInfo)
    then
      Result := MaxInt
    else
      Result := 0;
  end; { DSiExecute }

  {:Executes application with elevated privileges.
    @author  gabr
    @returns True if application was allowed to start.
    @since   2016-05-18
  }
  function DSiExecuteAsAdmin(const path, parameters, directory: string;
    parentWindow: THandle; showWindow: integer; wait: boolean): boolean;
  var
    sei: TShellExecuteInfo;
  begin
    FillChar(sei, SizeOf(sei), 0);
    sei.cbSize := SizeOf(sei);
    if wait then
      sei.fMask := SEE_MASK_NOCLOSEPROCESS;
    sei.lpVerb := 'runas';
    sei.lpFile := PChar(path);
    sei.lpParameters := PChar(parameters);
    sei.lpDirectory := PChar(directory);
    sei.Wnd := parentWindow;
    sei.nShow := showWindow;
    Result := ShellExecuteEx(@sei);
    if Result and wait and (sei.hProcess <> 0) then begin
      WaitForSingleObject(sei.hProcess, INFINITE);
      CloseHandle(sei.hProcess);
    end;
  end; { DSiExecuteAsAdmin }

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

  {:Executes a process in a different session. Useful for starting interactive
    process from a service.
    @author  gabr
    @returns Returns MaxInt if any Win32 API fails or process exit code if wait is
             specified or 0 in other cases.
  }
  function  DSiExecuteInSession(sessionID: DWORD; const commandLine: string;
    var processInfo: TProcessInformation; workDir: string): boolean;
  var
    cmdLine : string;
    hToken  : THandle;
    pEnv    : pointer;
    pWorkDir: PChar;
    si      : TStartupInfo;
  begin
    Result := false;
    if not DSiWTSQueryUserToken(sessionId, hToken) then
      Exit;

    FillChar(si, SizeOf(si), 0);
    si.cb := SizeOf(TStartupInfo);
    si.lpDesktop := 'winsta0\default';

    FillChar(processInfo, SizeOf(processInfo), 0);

    pEnv := nil;
    if not DSiCreateEnvironmentBlock(pEnv, hToken, false) then
      Exit;

    cmdLine := commandLine;
    if workDir = '' then
      pWorkDir := nil
    else
      pWorkDir := @workDir[1];
    {$IFDEF Unicode}UniqueString(cmdLine);{$ENDIF}
    Result := CreateProcessAsUser(hToken, pWorkDir, PChar(cmdLine), nil, nil, false,
                NORMAL_PRIORITY_CLASS OR CREATE_UNICODE_ENVIRONMENT,
                pEnv, nil, si, processInfo);

    DSiDestroyEnvironmentBlock(pEnv);
  end; { DSiExecuteInSession }

  {:Executes console process in a hidden window and captures its output in a TStrings
    object.
    Totaly reworked on 2006-01-23. New code contributed by matej.
    Handles only up to 1 MB of console process output when `output` is assigned.
    Console output can be larger than 1 MB if `output` is `nil`. `onNewLine` callback
    should then be used to process console output.
    @returns ID of the console process or 0 if process couldn't be started.
    @author  aoven, Lee_Nover, gabr, matej, mitja
    @since   2003-05-24
  }
  function DSiExecuteAndCapture(const app: string; output: TStrings; const workDir: string;
    var exitCode: longword; waitTimeout_sec: integer; onNewLine: TDSiOnNewLineCallback;
    creationFlags: DWORD; const abortHandle: THandle): cardinal;
  var
    endTime_ms         : int64;
    lineBuffer         : PAnsiChar;
    lineBufferSize     : integer;
    partialLine        : string;
    runningTimeLeft_sec: integer;

    procedure ProcessPartialLine(buffer: PAnsiChar; numBytes: integer);
    var
      now_ms  : int64;
      p       : integer;
      tokenLen: integer;
    begin
      if numBytes = 0 then
        Exit;
      if lineBufferSize < (numBytes + 1) then begin
        lineBufferSize := numBytes + 1;
        ReallocMem(lineBuffer, lineBufferSize);
      end;
      // caller made sure that buffer is zero terminated
      OemToCharA(buffer, lineBuffer);

      {$IFDEF Unicode}
      partialLine := partialLine + UnicodeString(StrPasA(lineBuffer));
      {$ELSE}
      partialLine := partialLine + StrPas(lineBuffer);
      {$ENDIF Unicode}
      repeat
        p := Pos(#13#10, partialLine);
        if p <= 0 then begin
          tokenLen := 1;
          p := Pos(#10, partialLine);
          if p <= 0 then begin
            p := Pos(#13, partialLine);
            if p = Length(partialLine) then
              p := 0;
          end;
        end
        else
          tokenLen := 2;
        if p <= 0 then
          break; //repeat
        now_ms := DSiTimeGetTime64;
        runningTimeLeft_sec := (endTime_ms - now_ms) div 1000;
        onNewLine(Copy(partialLine, 1, p-1), runningTimeLeft_sec);
        endTime_ms := now_ms + runningTimeLeft_sec * 1000;
        Delete(partialLine, 1, p + tokenLen - 1);
      until false;
    end; { ProcessPartialLine }

//    function DisplayStr(buf: PAnsiChar; count: integer): string;
//    begin
//      Result := '';
//      while count > 0 do begin
//        if CharInSet(buf^, [#32..#126]) then
//          Result := Result + string(buf^)
//        else
//          Result := Result + Format('$%.2x', [byte(buf^)]);
//        Inc(buf);
//        Dec(count);
//      end;
//    end;

  const
    SizeReadBuffer = 1048576;  // 1 MB Buffer

  var
    appRunning     : DWORD;
    appW           : string;
    buffer         : PAnsiChar;
    bytesRead      : DWORD;
    err            : cardinal;
    processInfo    : TProcessInformation;
    readPipe       : THandle;
    security       : TSecurityAttributes;
    start          : TStartUpInfo;
    totalBytesAvail: DWORD;
    totalBytesRead : DWORD;
    useWorkDir     : string;
    writePipe      : THandle;
    waitHandles    : array of THandle;

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
    if CreatePipe(readPipe, writePipe, @security, 0) then begin
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
      appRunning := WAIT_FAILED; // in case CreateProcess fails
      {$IFDEF Unicode}UniqueString(appW);{$ENDIF Unicode}
      if CreateProcess(nil, PChar(appW), @security, @security, true,
           creationFlags, nil, PChar(useWorkDir), start, processInfo) then
      begin
        SetLastError(0); // [Mitja] found a situation where CreateProcess succeeded but the last error was 126
        Result := processInfo.hProcess;
        totalBytesRead := 0;
        SetLength(waitHandles, 1 + Ord(abortHandle > 0));
        waitHandles[0] := processInfo.hProcess;
        if abortHandle > 0 then
          waitHandles[1] := abortHandle;

        repeat
          appRunning := WaitForMultipleObjects(Length(waitHandles), @waitHandles[0], False, 100);
          if not PeekNamedPipe(readPipe, nil, 0, nil, @totalBytesAvail, nil) then
            break //repeat
          else if totalBytesAvail > 0 then begin
            if totalBytesAvail <= (SizeReadBuffer - totalBytesRead) then
              bytesRead := totalBytesAvail
            else
              bytesRead := SizeReadBuffer - totalBytesRead;
            if not ReadFile(readPipe, buffer[totalBytesRead], bytesRead, bytesRead, nil) then
              RaiseLastOSError
            else begin
              buffer[totalBytesRead + bytesRead] := #0; // required for ProcessPartialLine
              if assigned(onNewLine) then
                ProcessPartialLine(@buffer[totalBytesRead], bytesRead);

              if Assigned(output) then
                totalBytesRead := totalBytesRead + bytesRead
              else
                totalBytesRead := 0;
              if totalBytesRead = SizeReadBuffer then
                raise Exception.Create('DSiExecuteAndCapture: Buffer full!');
            end;
          end;
        until (appRunning <> WAIT_TIMEOUT) or (DSiTimeGetTime64 > endTime_ms);
        if DSiTimeGetTime64 > endTime_ms then
          SetLastError(ERROR_TIMEOUT);
        if partialLine <> '' then begin
          runningTimeLeft_sec := 0;
          onNewLine(partialLine, runningTimeLeft_sec);
        end;
        if Assigned(output) then begin
          OemToCharA(buffer, buffer);
          {$IFDEF Unicode}
          output.Text := UnicodeString(StrPasA(Buffer));
          {$ELSE}
          output.Text := StrPas(buffer);
          {$ENDIF Unicode}
        end;
      end
      else
        err := GetLastError;
      FreeMem(buffer);
      if appRunning = WAIT_OBJECT_1 then
      begin
        exitCode := 1;
        if TerminateProcess(processInfo.hProcess, exitCode) then
          WaitForSingleObject(processInfo.hProcess, INFINITE);
      end
      else begin
        GetExitCodeProcess(processInfo.hProcess, exitCode);
        if exitCode = STILL_ACTIVE then
          if TerminateProcess(processInfo.hProcess, exitCode) then begin
            WaitForSingleObject(processInfo.hProcess, INFINITE);
            GetExitCodeProcess(processInfo.hProcess, exitCode);
          end;
      end;
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
  function DSiGetProcessAffinityMask: DSiNativeUInt;
  var
    systemAffinityMask : DSiNativeUInt;
  begin
    if not DSiIsWinNT then
      Result := 1
    else
      GetProcessAffinityMask(GetCurrentProcess, Result, systemAffinityMask);
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
  function DSiGetSystemAffinityMask: DSiNativeUInt;
  var
    processAffinityMask: DSiNativeUInt;
  begin
    if not DSiIsWinNT then
      Result := 1
    else
      GetProcessAffinityMask(GetCurrentProcess, processAffinityMask, Result);
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
  function DSiGetThreadAffinityMask: DSiNativeUInt;
  var
    processAffinityMask: DSiNativeUInt;
    systemAffinityMask : DSiNativeUInt;
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
    maxWorkingSetSize: DSiNativeUInt;
    minWorkingSetSize: DSiNativeUInt;
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
  function DSiSetProcessAffinity(const affinity: string): string;
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
  function DSiSetThreadAffinity(const affinity: string): string;
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
  function DSiStringToAffinityMask(const affinity: string): DSiNativeUInt;
  var
    idxID: integer;
  begin
    Result := 0;
    for idxID := Length(DSiCPUIDs) downto 1 do begin
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
  function DSiValidateProcessAffinity(const affinity: string): string;
  begin
    Result := DSiAffinityMaskToString(DSiValidateProcessAffinityMask(
                DSiStringToAffinityMask(affinity)));
  end; { DSiValidateProcessAffinity }

  {:Validates process affinity mask (removes all CPUs that are not in the
    system affinity mask).
    @author  gabr
    @since   2003-11-14
  }
  function DSiValidateProcessAffinityMask(affinityMask: DSiNativeUInt): DSiNativeUInt;
  begin
    Result := DSiGetSystemAffinityMask AND affinityMask;
  end; { TDSiRegistry.DSiValidateProcessAffinityMask }

  {:Validates process affinity mask (removes all CPUs that are not in the
    system affinity mask).
    @author  gabr
    @since   2003-11-14
  }
  function DSiValidateThreadAffinity(const affinity: string): string;
  begin
    Result := DSiAffinityMaskToString(DSiValidateThreadAffinityMask(
                DSiStringToAffinityMask(affinity)));
  end; { DSiValidateThreadAffinityMask }

  function DSiValidateThreadAffinityMask(affinityMask: DSiNativeUInt): DSiNativeUInt;
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
  //DSiTimeGetTime64Safe lock & globals
  GDSiTimeGetTime64Safe: TRTLCriticalSection;
  GLastTimeGetTimeSafe : DWORD;
  GTimeGetTimeBaseSafe : int64;

  //Count of registered windows in this instance
  GDSiWndHandlerCount: integer;
  GTerminateBackgroundTasks: THandle;

  {:Class message dispatcher for the DSiUtilWindow class. Fetches instance's WndProc from
    the window extra data and calls it.
  }
  function DSiClassWndProc(Window: HWND; Message: cardinal;
    aWParam: {$IFDEF Unicode}WPARAM{$ELSE}longint{$ENDIF};
    aLParam: {$IFDEF Unicode}LPARAM{$ELSE}longint{$ENDIF}): longint; stdcall;
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
      msg.wParam := aWParam;
      msg.lParam := aLParam;
      msg.Result := 0;
      try
        TWndMethod(instanceWndProc)(msg);
      except
        if Assigned(ApplicationHandleException) then
          ApplicationHandleException(nil);
      end;
      Result := msg.Result;
    end
    else
      Result := DefWindowProc(Window, Message, {$IFDEF Unicode}WPARAM{$ENDIF}(aWParam), {$IFDEF Unicode}LPARAM{$ENDIF}(aLParam));
  end; { DSiClassWndProc }

  {:Thread-safe AllocateHwnd.
    @author  gabr [based on http://fidoforum.ru/pages/new46s35o217746.ru.delphi and
                   TIcsWndHandler.AllocateHWnd from ICS v6 (http://www.overbyte.be)]
    @since   2007-05-30
  }
  function DSiAllocateHWnd(wndProcMethod: TWndMethod; style: cardinal; parentWindow: HWND): HWND;
  var
    alreadyRegistered: boolean;
    tempClass        : TWndClass;
    utilWindowClass  : TWndClass;
  begin
    {$IFNDEF ConditionalExpressions}
    Result := 0;
    {$ELSE}{$IF CompilerVersion < 32} //with Tokyo this assignment causes 'value never used' hint
    Result := 0;
    {$IFEND}{$ENDIF}
    FillChar(utilWindowClass, SizeOf(utilWindowClass), 0);
    EnterCriticalSection(GDSiWndHandlerCritSect);
    try
      alreadyRegistered := GetClassInfo(HInstance, CDSiHiddenWindowName, tempClass);
      if (not alreadyRegistered) or (tempClass.lpfnWndProc <> @DSiClassWndProc) then begin
        if alreadyRegistered then
          {$IFDEF DSiScopedUnitNames}Winapi.{$ENDIF}Windows.UnregisterClass(CDSiHiddenWindowName, HInstance);
        utilWindowClass.lpszClassName := CDSiHiddenWindowName;
        utilWindowClass.hInstance := HInstance;
        utilWindowClass.lpfnWndProc := @DSiClassWndProc;
        utilWindowClass.cbWndExtra := SizeOf(TMethod);
        if {$IFDEF DSiScopedUnitNames}Winapi.{$ENDIF}Windows.RegisterClass(utilWindowClass) = 0 then
          raise Exception.CreateFmt('Unable to register DSiWin32 hidden window class. %s',
            [SysErrorMessage(GetLastError)]);
      end;
      Result := CreateWindowEx(style, CDSiHiddenWindowName, '', WS_POPUP,
        0, 0, 0, 0, parentWindow, 0, HInstance, nil);
      if Result = 0 then
        raise Exception.CreateFmt('Unable to create DSiWin32 hidden window. %s',
                [SysErrorMessage(GetLastError)]);
      {$IFDEF CPUX64}
      SetWindowLongPtr(Result, GWL_METHODDATA, NativeInt(TMethod(wndProcMethod).Data));
      SetWindowLongPtr(Result, GWL_METHODCODE, NativeInt(TMethod(wndProcMethod).Code));
      {$ELSE}
      SetWindowLong(Result, GWL_METHODDATA, DSiNativeInt(TMethod(wndProcMethod).Data));
      SetWindowLong(Result, GWL_METHODCODE, DSiNativeInt(TMethod(wndProcMethod).Code));
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
    SetWindowLong(wnd, GWL_METHODDATA, 0);
    SetWindowLong(wnd, GWL_METHODCODE, 0);
    DestroyWindow(wnd);
    EnterCriticalSection(GDSiWndHandlerCritSect);
    try
      Dec(GDSiWndHandlerCount);
      if GDSiWndHandlerCount <= 0 then
        {$IFDEF DSiScopedUnitNames}Winapi.{$ENDIF}Windows.UnregisterClass(CDSiHiddenWindowName, HInstance);
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

type
  TCaptWndInfo = record
    Caption    : string;
    WindowClass: string;
    FoundWindow: HWND;
  end; { TCaptWndInfo }
  PCaptWndInfo = ^TCaptWndInfo;

  function EnumGetProcessWindowByCaption(wnd: HWND; userParam: LPARAM): BOOL; stdcall;
  begin
    if SameText(DSiGetWindowText(wnd), PCaptWndInfo(userParam)^.Caption)
       and ((PCaptWndInfo(userParam)^.WindowClass = '')
            or SameText(DSiGetClassName(wnd), PCaptWndInfo(userParam)^.WindowClass)) then
    begin
      PCaptWndInfo(userParam)^.FoundWindow := Wnd;
      Result := false;
    end
    else
      Result := true;
  end; { EnumGetProcessWindowByCaption }

  {gp}
  function DSiFindWindow(const caption, wndClass: string): HWND;
  var
    captWndInfo: TCaptWndInfo;
  begin
    captWndInfo.Caption := caption;
    captWndInfo.WindowClass := wndClass;
    captWndInfo.FoundWindow := 0;
    EnumWindows(@EnumGetProcessWindowByCaption, LPARAM(@captWndInfo));
    Result := captWndInfo.FoundWindow;
  end; { DSiFindWindow }

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
    if GetClassName(hwnd, winClass, SizeOf(winClass) div SizeOf(char)) <> 0 then
      Result := winClass
    else
      Result := '';
  end; { DSiGetClassName }

  {Lee_Nover}
  function DSiGetFocusedWindow: THandle;
  var
    PId: DWORD;
    TId: DWORD;
    Wnd: THandle;
  begin
    Result := GetFocus;
    if Result = 0 then begin
      Wnd := GetForegroundWindow;
      if Wnd <> 0 then begin
        TId := GetWindowThreadProcessId(Wnd, PId);
        if AttachThreadInput(GetCurrentThreadId, TId, True) then try
          Result := GetFocus;
        finally AttachThreadInput(GetCurrentThreadId, TId, False); end;
      end;
    end;
  end; { DSiGetFocusedWindow }

type
  TProcWndInfo = record
    TargetProcessID: DWORD;
    FoundWindow    : HWND;
  end; { TProcWndInfo }
  PProcWndInfo = ^TProcWndInfo;

  function EnumGetProcessWindowByProcessID(wnd: HWND; userParam: LPARAM): BOOL; stdcall;
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
  end; { EnumGetProcessWindowByProcessID }

  {ln}
  function DSiGetProcessWindow(targetProcessID: cardinal): HWND;
  var
    procWndInfo: TProcWndInfo;
  begin
    procWndInfo.TargetProcessID := targetProcessID;
    procWndInfo.FoundWindow := 0;
    EnumWindows(@EnumGetProcessWindowByProcessID, LPARAM(@procWndInfo));
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
        {$IFDEF DSiScopedUnitNames}System.{$ENDIF}SysUtils.Format('%s%.8d', [CStartHTML, idxStartHtml]) + #13#10 +
        {$IFDEF DSiScopedUnitNames}System.{$ENDIF}SysUtils.Format('%s%.8d', [CEndHTML, idxEndHtml]) + #13#10 +
        {$IFDEF DSiScopedUnitNames}System.{$ENDIF}SysUtils.Format('%s%.8d', [CStartFragment, idxStartFragment]) + #13#10 +
        {$IFDEF DSiScopedUnitNames}System.{$ENDIF}SysUtils.Format('%s%.8d', [CEndFragment, idxEndFragment]) + #13#10;
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
      {$IFDEF DSiHasGetFolderLocation}
      if Succeeded(SHGetFolderLocation(0, CSIDL, 0, 0, pPIDL)) then
      {$ELSE ~DSiHasGetFolderLocation}
      if Succeeded(SHGetSpecialFolderLocation(0, CSIDL, pPIDL)) then
      {$ENDIF ~DSiHasGetFolderLocation}
      begin
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

  function DSiGetLogicalProcessorInfo(
    var info: TSystemLogicalProcessorInformationArr): boolean;
  var
    infoLen: DWORD;
  begin
    infoLen := 0;
    Result := DSiGetLogicalProcessorInformation(nil, infoLen);
    if GetLastError = ERROR_INSUFFICIENT_BUFFER then begin
      SetLength(info, infoLen div SizeOf(TSystemLogicalProcessorInformation));
      infoLen := Length(info) * SizeOf(TSystemLogicalProcessorInformation);
      Result := DSiGetLogicalProcessorInformation(@info[0], infoLen);
    end;
    if not Result then
      SetLength(info, 0);
  end; { DSiGetLogicalProcessorInfo }

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
    if ExportsAPI(hKernel32, 'CopyFile2') then
      Result := wvWin8
    else if ExportsAPI(hKernel32, 'CreateRemoteThreadEx') then
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
              case versionInfoEx.dwMinorVersion of
                0: Result := wvWinVista;
                1: Result := wvWin7;
                2: Result := wvWin8;
                else Result := wvWin81;
              end
            else
              case versionInfoEx.dwMinorVersion of
                0: Result := wvWinServer2008;
                1: Result := wvWinServer2008R2;
                2: Result := wvWinServer2012;
                else Result := wvWinServer2012R2;
              end;
          end;
          10: begin
            versionInfoEx.dwOSVersionInfoSize := SizeOf(versionInfoEx);
            GetVersionEx(versionInfoExFake);
            if versionInfoEx.wProductType = VER_NT_WORKSTATION then
              Result := wvWin10
            else
              Result := wvWinServer2016;
          end;
        end; //case versionInfo.dwMajorVersion
      end; //versionInfo.dwPlatformID
  end; { DSiGetWindowsVersion }

  {:Checks whether current user (user the current process is running under) is using
    roaming profile.
    Based on: https://superuser.com/a/1006358/960
    @author gabr
  }
  function  DSiHasRoamingProfile(var userHasRoamingProfile: boolean): boolean;
  var
    sid: string;
  begin
    Result := false;
    if not DSiGetProcessSID(sid) then
      Exit;
    userHasRoamingProfile := '' <> (DSiReadRegistry('\SOFTWARE\Microsoft\Windows NT\CurrentVersion\ProfileList\' + sid,
                                      'CentralProfile', '', HKEY_LOCAL_MACHINE));
    Result := true;
  end; { DSiHasRoamingProfile }

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

  {:Checks whether the code is signed and returns that value as a function result.
    On the NT platform also returns the name of the signing certificate in the `certName`
    parameter.
    Based on the code by [Craig Peterson] published on the StackOverflow:
    http://stackoverflow.com/questions/5993877/checking-digital-signature-programmatically-from-delphi
    @author  Craig Peterson, gabr
  }
  function DSiIsCodeSigned(const exeFileName: string; var certName: AnsiString): boolean;
  var
    cert        : PWinCertificate;
    certContext : PCCERT_CONTEXT;
    certCount   : DWORD;
    certNameLen : DWORD;
    fileInfo    : TWinTrustFileInfo;
    hExe        : HMODULE;
    trustData   : TWinTrustData;
    verifyParams: CRYPT_VERIFY_MESSAGE_PARA;
  begin
    certName := '';
    // Verify that the exe is signed and the checksum matches.
    FillChar(fileInfo, SizeOf(fileInfo), 0);
    fileInfo.cbStruct := sizeof(fileInfo);
    fileInfo.pcwszFilePath := PWideChar(WideString(exeFileName));
    FillChar(trustData, SizeOf(trustData), 0);
    trustData.cbStruct := sizeof(trustData);
    trustData.dwUIChoice := WTD_UI_NONE;
    trustData.fdwRevocationChecks := WTD_REVOKE_NONE;
    trustData.dwUnionChoice := WTD_CHOICE_FILE;
    trustData.pFile := @fileInfo;
    Result := DSiWinVerifyTrust(INVALID_HANDLE_VALUE, WINTRUST_ACTION_GENERIC_VERIFY_V2,
      @trustData) = ERROR_SUCCESS;
    if Result and DSiIsWinNT then begin
      // Verify that the exe was signed by our private key
      hExe := CreateFile(PChar(exeFileName), GENERIC_READ, FILE_SHARE_READ,
        nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_RANDOM_ACCESS, 0);
      if hExe = INVALID_HANDLE_VALUE then
        Exit;
      try
        // There should only be one certificate associated with the exe
        if (not DSiImageEnumerateCertificates(hExe, CERT_SECTION_TYPE_ANY, certCount, nil, 0)) or
           (certCount <> 1)
        then
          Exit;
        // Read the certificate header so we can get the size needed for the full cert
        GetMem(cert, SizeOf(TWinCertificate) + 3); // ImageGetCertificateHeader writes an DWORD at bCertificate for some reason
        try
          cert.dwLength := 0;
          cert.wRevision := WIN_CERT_REVISION_1_0;
          if not DSiImageGetCertificateHeader(hExe, 0, cert^) then
            Exit;
          // Read the full certificate
          ReallocMem(cert, SizeOf(TWinCertificate) + cert.dwLength);
          if not DSiImageGetCertificateData(hExe, 0, cert, cert.dwLength) then
            Exit;
          // Get the certificate context.  CryptVerifyMessageSignature has the
          // side effect of creating a context for the signing certificate.
          FillChar(verifyParams, SizeOf(verifyParams), 0);
          verifyParams.cbSize := SizeOf(verifyParams);
          verifyParams.dwMsgAndCertEncodingType := X509_ASN_ENCODING or PKCS_7_ASN_ENCODING;
          if not DSiCryptVerifyMessageSignature(verifyParams, 0, @cert.bCertificate,
             cert.dwLength, nil, nil, @certContext) then
            Exit;
          try
            // Extract and compare the certificate's subject names.  Don't
            // compare the entire certificate or the public key as those will
            // change when the certificate is renewed.
            certNameLen := DSiCertGetNameStringA(certContext,
              CERT_NAME_SIMPLE_DISPLAY_TYPE, 0, nil, nil, 0);
            SetLength(CertName, certNameLen - 1);
            DSiCertGetNameStringA(certContext, CERT_NAME_SIMPLE_DISPLAY_TYPE, 0,
              nil, PAnsiChar(CertName), certNameLen);
          finally DSiCertFreeCertificateContext(certContext); end;
        finally FreeMem(cert); end;
      finally CloseHandle(hExe); end;
      Result := True;
    end;
  end; { DSiIsCodeSigned }

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
    const description: string; const grouping: string; const serviceName: string;
    protocols: TDSiFwIPProtocols; const localPorts: string; profiles: TDSiFwIPProfiles): boolean;
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
    const description: string; const grouping: string; const serviceName: string;
    protocols: TDSiFwIPProtocols; const localPorts: string; profiles: TDSiFwIPProfiles): boolean;
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

  {:Returns SID of the current process.
    @author gabr
  }
  function DSiGetProcessSID(var sid: string): boolean;
  var
    dwLength : DWORD;
    hProcess : THandle;
    procToken: THandle;
    tkUser   : ^TOKEN_USER;
    wcSid    : PWideChar;
  begin
    Result := false;
    hProcess := OpenProcess(STANDARD_RIGHTS_READ OR PROCESS_QUERY_INFORMATION, false, GetCurrentProcessID);
    if hProcess = 0 then
      Exit;
    try
      if not OpenProcessToken(hProcess, TOKEN_QUERY, procToken) then
        Exit;
      try
        dwLength := 0;
        tkUser := nil;
        GetTokenInformation(procToken, TokenUser, tkUser, 0, dwLength);
        if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
          Exit;
        tkUser := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, dwLength);
        if tkUser = nil then
          Exit;
        try
          if not GetTokenInformation(procToken, TokenUser, tkUser, dwLength, dwLength) then
            Exit;
          if not ConvertSidToStringSid(tkUser.User.Sid, wcSid) then
            Exit;
          try
            sid := string(wcSid);
            Result := true;
          finally LocalFree(NativeUInt(wcSid)); end;
        finally HeapFree(GetProcessHeap, 0, tkUser); end;
      finally DSiCloseHandleAndNull(procToken); end;
    finally DSiCloseHandleAndNull(hProcess); end;
  end; { DSiGetProcessSID }

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

  {:Checks whether the specified timeout_ms period has elapsed. Start time must be a value
    returned from the DSiTimeGetTime64Safe.
  }
  function DSiHasElapsed64Safe(startTime: int64; timeout_ms: DWORD): boolean;
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
  }
  function DSiTimeGetTime64: int64;
  begin
    EnterCriticalSection(GDSiTimeGetTime64Safe);
    try
      Result := timeGetTime;
      if Result < GLastTimeGetTimeSafe then
        GTimeGetTimeBaseSafe := GTimeGetTimeBaseSafe + $100000000;
      GLastTimeGetTimeSafe := Result;
      Result := Result + GTimeGetTimeBaseSafe;
    finally LeaveCriticalSection(GDSiTimeGetTime64Safe); end;
  end; { DSiTimeGetTime64Safe }

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

  function DSiGetSystemTimePreciseAsFileTime(var fileTime: TFileTime): boolean;
  begin
    if not assigned(GGetSystemTimePreciseAsFileTime) then
      GGetSystemTimePreciseAsFileTime := DSiGetProcAddress('kernel32.dll', 'GetSystemTimePreciseAsFileTime');
    Result := assigned(GGetSystemTimePreciseAsFileTime);
    if Result then
      GGetSystemTimePreciseAsFileTime(fileTime);
  end; { DSiGetSystemTimePreciseAsFileTime }

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
  var
    hLibrary: HMODULE;
  begin
    hLibrary := DSiLoadLibrary(libFileName);
    if hLibrary = 0 then
      Result := nil
    else
      Result := GetProcAddress(hLibrary, PChar(procName));
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

  function DSiCertCreateCertificateContext(dwCertEncodingType: DWORD;
    pbCertEncoded: PBYTE; cbCertEncoded: DWORD): PCCERT_CONTEXT; stdcall; //external 'Crypt32.dll';
  begin
    if not assigned(GCertCreateCertificateContext) then
      GCertCreateCertificateContext := DSiGetProcAddress('crypt32.dll', 'CertCreateCertificateContext');
    if assigned(GCertCreateCertificateContext) then
      Result := GCertCreateCertificateContext(dwCertEncodingType, pbCertEncoded, cbCertEncoded)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := nil;
    end;
  end; { DSiCertCreateCertificateContext }

  function DSiCertFreeCertificateContext(pCertContext: PCCERT_CONTEXT): BOOL; stdcall; //external 'Crypt32.dll';
  begin
    if not assigned(GCertFreeCertificateContext) then
      GCertFreeCertificateContext := DSiGetProcAddress('crypt32.dll', 'CertFreeCertificateContext');
    if assigned(GCertFreeCertificateContext) then
      Result := GCertFreeCertificateContext(pCertContext)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiCertFreeCertificateContext }

  function DSiCertGetNameStringA(pCertContext: PCCERT_CONTEXT; dwType: DWORD; dwFlags: DWORD;
    pvTypePara: Pointer; pszNameString: PAnsiChar; cchNameString: DWORD): DWORD; stdcall;
  begin
    if not assigned(GCertGetNameStringA) then
      GCertGetNameStringA := DSiGetProcAddress('crypt32.dll', 'CertGetNameStringA');
    if assigned(GCertGetNameStringA) then
      Result := GCertGetNameStringA(pCertContext, dwType, dwFlags, pvTypePara,
        pszNameString, cchNameString)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := 0;
    end;
  end; { DSiCertGetNameStringA }

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

  function DSiCryptVerifyMessageSignature(const pVerifyPara: CRYPT_VERIFY_MESSAGE_PARA;
    dwSignerIndex: DWORD; pbSignedBlob: PByte; cbSignedBlob: DWORD; pbDecoded: PBYTE;
    pcbDecoded: PDWORD; ppSignerCert: PCCERT_CONTEXT): BOOL; stdcall;
  begin
    if not assigned(GCryptVerifyMessageSignature) then
      GCryptVerifyMessageSignature := DSiGetProcAddress('crypt32.dll', 'CryptVerifyMessageSignature');
    if assigned(GCryptVerifyMessageSignature) then
      Result := GCryptVerifyMessageSignature(pVerifypara, dwSignerIndex, pbSignedBlob,
        cbSignedBlob, pbDecoded, pcbDecoded, ppSignerCert)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiCryptVerifyMessageSignature }

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

  function DSiGetLogicalProcessorInformation(
    pBuffer: PSYSTEM_LOGICAL_PROCESSOR_INFORMATION;
    var ReturnLength: DWORD): BOOL;
  begin
    if not assigned(GGetLogicalProcessorInformation) then
      GGetLogicalProcessorInformation := DSiGetProcAddress('kernel32.dll', 'GetLogicalProcessorInformation');
    if assigned(GGetLogicalProcessorInformation) then
      Result := GGetLogicalProcessorInformation(pBuffer, ReturnLength)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiGetLogicalProcessorInformation }

  function DSiGetLogicalProcessorInformationEx(
    RelationshipType: LOGICAL_PROCESSOR_RELATIONSHIP;
    Buffer: PSYSTEM_LOGICAL_PROCESSOR_INFORMATION;
    var ReturnedLength: DWORD): BOOL;
  begin
    if not assigned(GGetLogicalProcessorInformationEx) then
      GGetLogicalProcessorInformationEx := DSiGetProcAddress('kernel32.dll', 'GetLogicalProcessorInformationEx');
    if assigned(GGetLogicalProcessorInformationEx) then
      Result := GGetLogicalProcessorInformationEx(RelationshipType, Buffer, ReturnedLength)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiGetLogicalProcessorInformationEx }

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

  function DSiGetThreadGroupAffinity(hThread: THandle; var GroupAffinity: TGroupAffinity): BOOL;
  begin
    if not assigned(GGetThreadGroupAffinity) then
      GGetThreadGroupAffinity := DSiGetProcAddress('kernel32.dll', 'GetThreadGroupAffinity');
    if assigned(GGetThreadGroupAffinity) then
      Result := GGetThreadGroupAffinity(hThread, GroupAffinity)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiGetThreadGroupAffinity }

  function DSiGetSystemFirmwareTable(FirmwareTableProviderSignature: DWORD;
    FirmwareTableID: DWORD; pFirmwareTableBuffer: pointer; BufferSize: DWORD): UInt;
  begin
    if not assigned(GGetSystemFirmwareTable) then
      GGetSystemFirmwareTable := DSiGetProcAddress('kernel32.dll', 'GetSystemFirmwareTable');
    if assigned(GGetSystemFirmwareTable) then
      Result := GGetSystemFirmwareTable(FirmwareTableProviderSignature, FirmwareTableID,
        pFirmwareTableBuffer, BufferSize)
    else
      Result := ERROR_NOT_SUPPORTED;
  end; { DSiGetSystemFirmwareTable }

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

  function DSiImageEnumerateCertificates(FileHandle: THandle; TypeFilter: WORD;
    out CertificateCount: DWORD; Indices: PDWORD; IndexCount: Integer): BOOL; stdcall;
  begin
    if not assigned(GImageEnumerateCertificates) then
      GImageEnumerateCertificates := DSiGetProcAddress('imagehlp.dll', 'ImageEnumerateCertificates');
    if assigned(GImageEnumerateCertificates) then
      Result := GImageEnumerateCertificates(FileHandle, TypeFilter, CertificateCount,
        Indices, IndexCount)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiImageEnumerateCertificates }

  function DSiImageGetCertificateData(FileHandle: THandle; CertificateIndex: Integer;
    Certificate: PWinCertificate; var RequiredLength: DWORD): BOOL; stdcall;
  begin
    if not assigned(GImageGetCertificateData) then
      GImageGetCertificateData := DSiGetProcAddress('imagehlp.dll', 'ImageGetCertificateData');
    if assigned(GImageGetCertificateData) then
      Result := GImageGetCertificateData(FileHandle, CertificateIndex, Certificate, RequiredLength)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiImageGetCertificateData }

  function DSiImageGetCertificateHeader(FileHandle: THandle; CertificateIndex: Integer;
    var CertificateHeader: TWinCertificate): BOOL; stdcall;
  begin
    if not assigned(GImageGetCertificateHeader) then
      GImageGetCertificateHeader := DSiGetProcAddress('imagehlp.dll', 'ImageGetCertificateHeader');
    if assigned(GImageGetCertificateHeader) then
      Result := GImageGetCertificateHeader(FileHandle, CertificateIndex, CertificateHeader)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiImageGetCertificateHeader }

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

  function DSiGetNumaHighestNodeNumber(var HighestNodeNunber: ULONG): BOOL;
  begin
    if not assigned(GGetNumaHighestNodeNumber) then
      GGetNumaHighestNodeNumber := DSiGetProcAddress('kernel32.dll', 'GetNumaHighestNodeNumber');
    if assigned(GGetNumaHighestNodeNumber) then
      Result := GGetNumaHighestNodeNumber(HighestNodeNunber)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiGetNumaHighestNodeNumber }

  function DSiGetNumaProximityNodeEx(ProximityId: ULONG; var NodeNumber: USHORT): BOOL; stdcall;
  begin
    if not assigned(GGetNumaProximityNodeEx) then
      GGetNumaProximityNodeEx := DSiGetProcAddress('kernel32.dll', 'GetNumaProximityNodeEx');
    if assigned(GGetNumaProximityNodeEx) then
      Result := GGetNumaProximityNodeEx(ProximityId, NodeNumber)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiGetNumaProximityNodeEx }

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
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiSetDllDirectory }

  function DSiSetSuspendState(hibernate, forceCritical, disableWakeEvent: BOOL): BOOL; stdcall;
  begin
    if not assigned(GSetSuspendState) then
      GSetSuspendState := DSiGetProcAddress('PowrProf.dll', 'SetSuspendState');
    if assigned(GSetSuspendState) then
      Result := GSetSuspendState(hibernate, forceCritical, disableWakeEvent)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiSetSuspendState }

  function DSiSetThreadGroupAffinity(hThread: THandle; const GroupAffinity: TGroupAffinity;
    PreviousGroupAffinity: PGroupAffinity): BOOL;
  begin
    if not assigned(GSetThreadGroupAffinity) then
      GSetThreadGroupAffinity := DSiGetProcAddress('kernel32.dll', 'SetThreadGroupAffinity');
    if assigned(GSetThreadGroupAffinity) then
      Result := GSetThreadGroupAffinity(hThread, GroupAffinity, PreviousGroupAffinity)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiSetThreadGroupAffinity }

  function DSiSHEmptyRecycleBin(Wnd: HWND; pszRootPath: PChar;
    dwFlags: DWORD): HRESULT; stdcall;
  begin
    if not assigned(GSHEmptyRecycleBin) then
      GSHEmptyRecycleBin := DSiGetProcAddress('shell32.dll', 'SHEmptyRecycleBin' + CAPISuffix);
    if assigned(GSHEmptyRecycleBin) then
      Result := GSHEmptyRecycleBin(Wnd, pszRootPath, dwFlags)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := S_FALSE;
    end;
  end; { DSiSHEmptyRecycleBin }

  function DSiWinVerifyTrust(hwnd: HWND; const ActionID: TGUID;
    ActionData: Pointer): Longint; stdcall;
  begin
    if not assigned(GWinVerifyTrust) then
      GWinVerifyTrust := DSiGetProcAddress('wintrust.dll', 'WinVerifyTrust');
    if assigned(GWinVerifyTrust) then
      Result := GWinVerifyTrust(hwnd, ActionID, ActionData)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := TRUST_E_ACTION_UNKNOWN;
    end;
  end; { DSiWinVerifyTrust }

  function DSiWow64DisableWow64FsRedirection(var oldStatus: pointer): BOOL; stdcall;
  begin
    if not assigned(GWow64DisableWow64FsRedirection) then
      GWow64DisableWow64FsRedirection := DSiGetProcAddress('kernel32.dll', 'Wow64DisableWow64FsRedirection');
    if assigned(GWow64DisableWow64FsRedirection) then
      Result := GWow64DisableWow64FsRedirection(oldStatus)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiWow64DisableWow64FsRedirection }

  function DSiWow64RevertWow64FsRedirection(const oldStatus: pointer): BOOL; stdcall;
  begin
    if not assigned(GWow64RevertWow64FsRedirection) then
      GWow64RevertWow64FsRedirection := DSiGetProcAddress('kernel32.dll', 'Wow64RevertWow64FsRedirection');
    if assigned(GWow64RevertWow64FsRedirection) then
      Result := GWow64RevertWow64FsRedirection(oldStatus)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiWow64RevertWow64FsRedirection }

  function DSiWTSQueryUserToken(sessionId: ULONG; var phToken: THandle): BOOL; stdcall;
  begin
    if not assigned(GWTSQueryUserToken) then
      GWTSQueryUserToken := DSiGetProcAddress('wtsapi32.dll', 'WTSQueryUserToken');
    if assigned(GWTSQueryUserToken) then
      Result := GWTSQueryUserToken(sessionId, phToken)
    else begin
      SetLastError(ERROR_NOT_SUPPORTED);
      Result := false;
    end;
  end; { DSiWTSQueryUserToken }

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

procedure InitializeGlobals;
begin
  InitializeCriticalSection(GDSiWndHandlerCritSect);
  InitializeCriticalSection(GDSiTimeGetTime64Safe);
  GTerminateBackgroundTasks := CreateEvent(nil, false, false, nil);
  GDSiWndHandlerCount := 0;
  GLastTimeGetTimeSafe := 0;
  GTimeGetTimeBaseSafe := 0;
  if not QueryPerformanceFrequency(GPerformanceFrequency) then
    GPerformanceFrequency := 0;
  GCF_HTML := RegisterClipboardFormat('HTML Format');
  DynaLoadAPIs;
  timeBeginPeriod(1);
  Assert(Length(DSiCPUIDs) = 64);
end; { InitializeGlobals }

procedure CleanupGlobals;
begin
  timeEndPeriod(1);
  DSiCloseHandleAndNull(GTerminateBackgroundTasks);
  DeleteCriticalSection(GDSiWndHandlerCritSect);
  DeleteCriticalSection(GDSiTimeGetTime64Safe);
  DSiUnloadLibrary;
  FreeAndNil(_GLibraryList);
end; { CleanupGlobals }

initialization
  InitializeGlobals;
finalization
  CleanupGlobals;
end.

