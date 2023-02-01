// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.Logger.pas' rev: 35.00 (Windows)

#ifndef Gls_LoggerHPP
#define Gls_LoggerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Winapi.ShellAPI.hpp>
#include <System.StrUtils.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.UITypes.hpp>
#include <System.SyncObjs.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Logger
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TLogBufferFlushThread;
class DELPHICLASS TLogCheckSizeThread;
class DELPHICLASS TLogSession;
class DELPHICLASS TGLSLogger;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TLogLevel : unsigned char { lkDebug, lkInfo, lkNotice, lkWarning, lkError, lkFatalError };

typedef System::Set<TLogLevel, TLogLevel::lkDebug, TLogLevel::lkFatalError> TLogLevels;

enum DECLSPEC_DENUM TLogMessageLimitAction : unsigned char { mlaContinue, mlaStopLogging, mlaHalt };

typedef System::StaticArray<System::UnicodeString, 6> Gls_Logger__1;

enum DECLSPEC_DENUM TLogTimeFormat : unsigned char { lfNone, lfDate, lfTime, lfTimeExact, lfDateTime, lfElapsed };

enum DECLSPEC_DENUM TLogBufferingMode : unsigned char { lbmWriteEmidiatly, lbmWritePeriodically, lbmWriteInTheEnd };

typedef System::TMetaClass* CLogSession;

class PASCALIMPLEMENTATION TLogBufferFlushThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	TLogSession* FParent;
	
protected:
	virtual void __fastcall Execute();
	
public:
	__fastcall TLogBufferFlushThread(TLogSession* const AParent);
public:
	/* TThread.Destroy */ inline __fastcall virtual ~TLogBufferFlushThread() { }
	
};


class PASCALIMPLEMENTATION TLogCheckSizeThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	TLogSession* FParent;
	
protected:
	virtual void __fastcall Execute();
	
public:
	__fastcall TLogCheckSizeThread(TLogSession* const AParent);
public:
	/* TThread.Destroy */ inline __fastcall virtual ~TLogCheckSizeThread() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TLogSession : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
	
private:
	typedef System::StaticArray<System::UnicodeString, 6> _TLogSession__1;
	
	
private:
	System::Classes::TStringList* FBuffer;
	bool FBuffered;
	TLogBufferFlushThread* FBufferProcessingThread;
	TLogCheckSizeThread* FCheckLogSizeThread;
	int FFlushBufferPeriod;
	System::TextFile FLogFile;
	bool FDestroying;
	System::UnicodeString FOriginalLogFileName;
	System::UnicodeString FCurrentLogFileName;
	System::Classes::TStringList* FUsedLogFileNames;
	TLogLevels FLogLevels;
	bool FEnabled;
	System::Syncobjs::TCriticalSection* FBufferCriticalSection;
	System::Syncobjs::TCriticalSection* FFileAccessCriticalSection;
	_TLogSession__1 FModeTitles;
	System::StaticArray<int, 6> FLogKindCount;
	bool FLogThreadId;
	TLogMessageLimitAction FMessageLimitAction;
	TLogTimeFormat FTimeFormat;
	unsigned FStartedMs;
	int FLogFileMaxSize;
	int FCheckFileSizePeriod;
	TLogLevels FDisplayLogOnExitIfItContains;
	bool FWriteInternalMessages;
	bool FDisplayErrorDialogs;
	
protected:
	__fastcall TLogSession();
	
private:
	void __fastcall SetBuffered(const bool Value);
	void __fastcall SetMode(const TLogLevels NewMode);
	void __fastcall ChangeBufferedState();
	void __fastcall SetEnabled(const bool Value);
	void __fastcall SetLogFileMaxSize(const int Value);
	
protected:
	void __fastcall PrintLogLevels();
	void __fastcall PrintLogStatistics();
	bool __fastcall AttachLogFile(const System::UnicodeString AFileName, const bool AResetFile = true);
	void __fastcall ClearLogsInTheSameDir();
	void __fastcall BackUpOldLogs(const System::UnicodeString ACurrentLogFileName);
	void __fastcall CreateNewLogFileIfNeeded();
	void __fastcall AppendLog(const System::UnicodeString AString, const TLogLevel ALevel, const bool ALogTime = true);
	bool __fastcall DoWriteToLog(const System::UnicodeString AString);
	bool __fastcall DoWriteBufferToLog();
	bool __fastcall DoResetLog();
	
public:
	__fastcall virtual TLogSession(const System::UnicodeString AFileName, const TLogTimeFormat ATimeFormat, const TLogLevels ALevels, const bool ALogThreadId, const bool ABuffered, const int AMaxSize, const bool ABackUpOldLogs, const bool AClearOldLogs, const bool AWriteInternalMessages);
	__fastcall virtual ~TLogSession();
	void __fastcall Log(const System::UnicodeString Desc, const TLogLevel Level = (TLogLevel)(0x1));
	void __fastcall LogAdv(const System::TVarRec *args, const int args_High, const TLogLevel ALevel = (TLogLevel)(0x4));
	void __fastcall LogException(System::Sysutils::Exception* const E, const System::UnicodeString aFunctionName, const System::TVarRec *args, const int args_High, const TLogLevel ALevel = (TLogLevel)(0x4));
	void __fastcall LogDebug(const System::UnicodeString Desc);
	void __fastcall LogInfo(const System::UnicodeString Desc);
	void __fastcall LogNotice(const System::UnicodeString Desc);
	void __fastcall LogWarning(const System::UnicodeString Desc);
	void __fastcall LogError(const System::UnicodeString Desc);
	void __fastcall LogFatalError(const System::UnicodeString Desc);
	void __fastcall LogEmtryLine();
	void __fastcall LogDebugFmt(const System::UnicodeString Desc, const System::TVarRec *args, const int args_High);
	void __fastcall LogInfoFmt(const System::UnicodeString Desc, const System::TVarRec *args, const int args_High);
	void __fastcall LogNoticeFmt(const System::UnicodeString Desc, const System::TVarRec *args, const int args_High);
	void __fastcall LogWarningFmt(const System::UnicodeString Desc, const System::TVarRec *args, const int args_High);
	void __fastcall LogErrorFmt(const System::UnicodeString Desc, const System::TVarRec *args, const int args_High);
	void __fastcall LogFatalErrorFmt(const System::UnicodeString Desc, const System::TVarRec *args, const int args_High);
	void __fastcall DisplayLog();
	void __fastcall FlushBuffer();
	__property TLogLevels LogLevels = {read=FLogLevels, write=SetMode, default=63};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=1};
	__property bool Buffered = {read=FBuffered, write=SetBuffered, default=0};
	__property int FlushBufferPeriod = {read=FFlushBufferPeriod, write=FFlushBufferPeriod, default=5000};
	__property bool LogThreadId = {read=FLogThreadId, write=FLogThreadId, default=1};
	__property bool DisplayErrorDialogs = {read=FDisplayErrorDialogs, write=FDisplayErrorDialogs, default=1};
	__property TLogMessageLimitAction MessageLimitAction = {read=FMessageLimitAction, write=FMessageLimitAction, default=2};
	__property bool WriteInternalMessages = {read=FWriteInternalMessages, write=FWriteInternalMessages, default=1};
	__property TLogLevels DisplayLogOnExitIfItContains = {read=FDisplayLogOnExitIfItContains, write=FDisplayLogOnExitIfItContains, default=63};
	__property int LogFileMaxSize = {read=FLogFileMaxSize, write=SetLogFileMaxSize, default=0};
	__property int CheckFileSizePeriod = {read=FCheckFileSizePeriod, write=FCheckFileSizePeriod, default=4000};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLSLogger : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FReplaceAssertion;
	TLogTimeFormat FTimeFormat;
	TLogLevels FLogLevels;
	TLogSession* FLog;
	void __fastcall SetReplaceAssertion(bool Value);
	TLogSession* __fastcall GetLog();
	
public:
	__fastcall virtual TGLSLogger(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLSLogger();
	void __fastcall DoPrimary();
	__property TLogSession* Log = {read=GetLog};
	
__published:
	__property bool ReplaceAssertion = {read=FReplaceAssertion, write=SetReplaceAssertion, default=0};
	__property TLogTimeFormat TimeFormat = {read=FTimeFormat, write=FTimeFormat, default=5};
	__property TLogLevels LogLevels = {read=FLogLevels, write=FLogLevels, default=63};
};


typedef void __fastcall (*TIDELogProc)(const System::UnicodeString AMsg);

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::StaticArray<int, 6> llMessageLimit;
extern DELPHI_PACKAGE Gls_Logger__1 lkPrefix;
extern DELPHI_PACKAGE TLogLevels llMax;
extern DELPHI_PACKAGE TLogLevels llMedium;
extern DELPHI_PACKAGE TLogLevels llMin;
extern DELPHI_PACKAGE TIDELogProc vIDELogProc;
extern DELPHI_PACKAGE TLogSession* __fastcall GLSLogger(void);
extern DELPHI_PACKAGE void __fastcall UseCustomGLSLogger(TLogSession* const ALogger);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ConstArrayToString(const System::TVarRec *Elements, const int Elements_High);
extern DELPHI_PACKAGE TLogSession* __fastcall UserLog(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ReadLine(System::TextFile &TextFile);
}	/* namespace Logger */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_LOGGER)
using namespace Gls::Logger;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_LoggerHPP
