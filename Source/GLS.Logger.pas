//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.Logger;

(*
  Activate USE_LOGGING in "GLScene.inc" to turn on inner GLScene logger.
  You may have only one instance of TGLSLogger
  To obtain it, call UserLog() function from any unit.
*)

interface

{$I GLScene.inc}

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellApi,
  System.StrUtils,
  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.SyncObjs;

type
  // Levels of importance of log messages
  TLogLevel = (lkDebug, lkInfo, lkNotice, lkWarning, lkError, lkFatalError);
  // Log level setting type
  TLogLevels = set of TLogLevel;

  // What to do when number of messages exceeds message limit.
  TLogMessageLimitAction = (mlaContinue, mlaStopLogging, mlaHalt);

var
  llMessageLimit: array [TLogLevel] of Integer = (
    MaxInt,
    MaxInt,
    MaxInt,
    500,
    100,
    10
  );

  lkPrefix: array [TLogLevel] of string = (
    ' (D)  ',
    ' (i)  ',
    ' (M)  ',
    ' (W)  ',
    ' (Er)  ',
    ' (!!)  '
  );

const
  llMax: TLogLevels = [lkDebug, lkInfo, lkNotice, lkWarning, lkError,
    lkFatalError];
  llMedium: TLogLevels = [lkNotice, lkWarning, lkError, lkFatalError];
  llMin: TLogLevels = [lkError, lkFatalError];

type
  // Log date and time setting type
  TLogTimeFormat = (
    // doesn't output any time information
    lfNone,
    // include date in the log
    lfDate,
    // include time in the log
    lfTime,
    // include time in the log, including milliseconds
    lfTimeExact,
    // include date and time in the log
    lfDateTime,
    // include time elapsed since startup in the log
    lfElapsed);
  // How log is buffered.
  TLogBufferingMode = (lbmWriteEmidiatly, lbmWritePeriodically,
    lbmWriteInTheEnd);

  // Class reference to log session class
  CLogSession = class of TLogSession;
  TLogSession = class;

  // Thread that periodically flushes the buffer to disk.
  TLogBufferFlushThread = class(TThread)
  private
    FParent: TLogSession;
  protected
    procedure Execute; override;
  public
    constructor Create(const AParent: TLogSession);
  end;

  // Thread that checks file size and splits the file if nessesary.
  TLogCheckSizeThread = class(TThread)
  private
    FParent: TLogSession;
  protected
    procedure Execute; override;
  public
    constructor Create(const AParent: TLogSession);
  end;

  // Abstract Logger class
  TLogSession = class(TPersistent)
  private
    FBuffer: TStringList;
    FBuffered: Boolean;
    FBufferProcessingThread: TLogBufferFlushThread;
    FCheckLogSizeThread: TLogCheckSizeThread;
    FFlushBufferPeriod: Integer;
    FLogFile: Text; // TextFile.
    FDestroying: Boolean;

    FOriginalLogFileName: string; // Original name
    FCurrentLogFileName: string;
    // Current log file, if original exceeded certain size limit.
    FUsedLogFileNames: TStringList; // List of all created log files.

    FLogLevels: TLogLevels;
    FEnabled: Boolean;
    FBufferCriticalSection: TCriticalSection;
    FFileAccessCriticalSection: TCriticalSection;
    FModeTitles: array [TLogLevel] of string;
    FLogKindCount: array [TLogLevel] of Integer;
    FLogThreadId: Boolean;
    FMessageLimitAction: TLogMessageLimitAction;
    // Determines which date or time to include in the log
    FTimeFormat: TLogTimeFormat;
    // Startup timestamp in milliseconds
    FStartedMs: Cardinal;
    FLogFileMaxSize: Integer;
    FCheckFileSizePeriod: Integer;
    FDisplayLogOnExitIfItContains: TLogLevels;
    FWriteInternalMessages: Boolean;
    FDisplayErrorDialogs: Boolean;
{$IFNDEF USE_LOGGING}
    constructor OnlyCreate;
{$ENDIF}
    procedure SetBuffered(const Value: Boolean);
    procedure SetMode(const NewMode: TLogLevels);
    procedure ChangeBufferedState();
    procedure SetEnabled(const Value: Boolean);
    procedure SetLogFileMaxSize(const Value: Integer);
  protected
    procedure PrintLogLevels();
    procedure PrintLogStatistics();
    function AttachLogFile(const AFileName: string;
      const AResetFile: Boolean = True): Boolean;
    procedure ClearLogsInTheSameDir();
    procedure BackUpOldLogs(const ACurrentLogFileName: string);
    procedure CreateNewLogFileIfNeeded();
    // Appends a string to log. Thread-safe.
    procedure AppendLog(const AString: string; const ALevel: TLogLevel;
      const ALogTime: Boolean = True);
    // Writes string to log. Returns True if everything went ok.
    function DoWriteToLog(const AString: string): Boolean;
    // Writes FBuffer to log. Returns True if everything went ok.
    function DoWriteBufferToLog(): Boolean;
    // Resets log. Returns True if everything went ok.
    function DoResetLog: Boolean;
  public
    // Initializes a log session with the specified log file name, time and level settings
    constructor Init(const AFileName: string; const ATimeFormat: TLogTimeFormat;
      const ALevels: TLogLevels; const ALogThreadId: Boolean = True;
      const ABuffered: Boolean = False; const AMaxSize: Integer = 0;
      const ABackUpOldLogs: Boolean = False;
      const AClearOldLogs: Boolean = True;
      const AWriteInternalMessages: Boolean = True); virtual;
    destructor Destroy; override;
    // General Logging procedures
    procedure Log(const Desc: string; const Level: TLogLevel = lkInfo);
    procedure LogAdv(const args: array of const;
      const ALevel: TLogLevel = lkError);
    procedure LogException(const E: Exception; const aFunctionName: string;
      const args: array of const; const ALevel: TLogLevel = lkError);
    (* Logs a string  Desc  if  Level
      matches current USE_LOGGING level (see @Link(LogLevels)) *)
    procedure LogDebug(const Desc: string);
    procedure LogInfo(const Desc: string);
    procedure LogNotice(const Desc: string);
    procedure LogWarning(const Desc: string);
    procedure LogError(const Desc: string);
    procedure LogFatalError(const Desc: string);
    procedure LogEmtryLine();
    // Logs a formatted string assembled from a format string and an array of arguments.
    procedure LogDebugFmt(const Desc: string; const args: array of const);
    procedure LogInfoFmt(const Desc: string; const args: array of const);
    procedure LogNoticeFmt(const Desc: string; const args: array of const);
    procedure LogWarningFmt(const Desc: string; const args: array of const);
    procedure LogErrorFmt(const Desc: string; const args: array of const);
    procedure LogFatalErrorFmt(const Desc: string; const args: array of const);
    // Mics procedures.
    procedure DisplayLog();
    procedure FlushBuffer();
    // If log is buffered, calling this will flush the buffer.
    // Set of levels which to include in the log
    property LogLevels: TLogLevels read FLogLevels write SetMode
      default [lkDebug, lkInfo, lkNotice, lkWarning, lkError, lkFatalError];
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Buffered: Boolean read FBuffered write SetBuffered default False;
    property FlushBufferPeriod: Integer read FFlushBufferPeriod
      write FFlushBufferPeriod default 5000; // In ms.
    property LogThreadId: Boolean read FLogThreadId write FLogThreadId
      default True;
    property DisplayErrorDialogs: Boolean read FDisplayErrorDialogs
      write FDisplayErrorDialogs default True;
    property MessageLimitAction: TLogMessageLimitAction read FMessageLimitAction
      write FMessageLimitAction default mlaHalt;
    property WriteInternalMessages: Boolean read FWriteInternalMessages
      write FWriteInternalMessages default True;
    // To always display log, put all log types. To never display log, leave this empty.
    property DisplayLogOnExitIfItContains: TLogLevels
      read FDisplayLogOnExitIfItContains write FDisplayLogOnExitIfItContains
      default [lkDebug, lkInfo, lkNotice, lkWarning, lkError, lkFatalError];
    (* If LogFileMaxSize is not 0, then:
      1) At start, all logs with the same extention will be deleted.
      2) All logs wil be periodically cheked for FileSize.
      New log file will be created when this size exceeds limit. *)
    property LogFileMaxSize: Integer read FLogFileMaxSize
      write SetLogFileMaxSize default 0; // In bytes, limited to 2Gb.
    property CheckFileSizePeriod: Integer read FCheckFileSizePeriod
      write FCheckFileSizePeriod default 4000; // In ms.
  end;

  // Abstract class for control logging.
  TGLSLogger = class(TComponent)
  private
    FReplaceAssertion: Boolean;
    FTimeFormat: TLogTimeFormat;
    FLogLevels: TLogLevels;
    FLog: TLogSession;
    procedure SetReplaceAssertion(Value: Boolean);
    function GetLog: TLogSession;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Set component primary and then UserLog return it's log
    procedure DoPrimary;
    property Log: TLogSession read GetLog;
  published
    property ReplaceAssertion: Boolean read FReplaceAssertion
      write SetReplaceAssertion default False;
    // Only design time sets. Define Log initial properties
    property TimeFormat: TLogTimeFormat read FTimeFormat write FTimeFormat
      default lfElapsed;
    property LogLevels: TLogLevels read FLogLevels write FLogLevels
      default [lkDebug, lkInfo, lkNotice, lkWarning, lkError, lkFatalError];
  end;

  TIDELogProc = procedure(const AMsg: string);

// Return logger wich created by TGLSLogger component
function UserLog: TLogSession;
function ReadLine(var TextFile: Text): string;

(* Inner logger.
  Converted to a function, because in case of a DLL and main app using this module,
  log is written to the same file on initialization and finalization,
  which is not what one might want. This also allows to create a GLSLogger with
  custom parameters for user's application, for example a different log path
  (Often the EXE application directory is read-only).
*)
function GLSLogger(): TLogSession;
procedure UseCustomGLSLogger(const ALogger: TLogSession);
function ConstArrayToString(const Elements: array of const): String;

var
  vIDELogProc: TIDELogProc;

// --------------------------------------------------------------------------
implementation
// --------------------------------------------------------------------------

var
  v_GLSLogger: TLogSession;
  vAssertErrorHandler: TAssertErrorProc;
  vCurrentLogger: TGLSLogger;

// Inner logger. Create on first use, not in unit initialization. }
function GLSLogger(): TLogSession;
begin
  if v_GLSLogger = nil then
  begin
{$IFDEF USE_LOGGING}
    v_GLSLogger := TLogSession.Init(Copy(ExtractFileName(ParamStr(0)), 1,
      Length(ExtractFileName(ParamStr(0))) - Length(ExtractFileExt(ParamStr(0)))
      ) + '.log', lfElapsed, llMax);
{$ELSE}
    v_GLSLogger := TLogSession.OnlyCreate;
{$ENDIF}
  end;
  Result := v_GLSLogger;
end;

procedure UseCustomGLSLogger(const ALogger: TLogSession);
begin
  if (v_GLSLogger <> nil) then
    v_GLSLogger.Destroy;
  v_GLSLogger := ALogger;
end;

const
  // VarRec -> String
  vTypeDesc: Array [0 .. 16] of String = ('vtInteger', 'vtBoolean', 'vtChar',
    'vtExtended', 'vtString', 'vtPointer', 'vtPChar', 'vtObject', 'vtClass',
    'vtWideChar', 'vtPWideChar', 'vtAnsiString', 'vtCurrency', 'vtVariant',
    'vtInterface', 'vtWideString', 'vtInt64');
  vTypeAsSring: Array [0 .. 17] of String = ('Integer     : ', 'Boolean     : ',
    'Char        : ', 'Extended    : ', 'String      : ', 'Pointer     : ',
    'PChar       : ', 'TObject     : ', 'Class       : ', 'WideChar    : ',
    'PWideChar   : ', 'AnsiString  : ', 'Currency    : ', 'Variant     : ',
    'Interface   : ', 'WideString  : ', 'Int64       : ', '#HLType     : ');

// Function from HotLog by Olivier Touzot "QnnO".
function GetOriginalValue(const s: String): String;
// Called to remove the false 'AnsiString :' assertion, for pointers and objects
begin
  Result := RightStr(s, Length(s) - 19);
end;

// Function from HotLog by Olivier Touzot "QnnO".
function VarRecToStr(const vr: TVarRec): String;
// See D6PE help topic "TVarRec"
begin
  Result := vTypeAsSring[vr.VType] + ' ';
  try
    with vr do
      case VType of
        vtInteger:  Result := Result + IntToStr(VInteger);
        vtBoolean:  Result := Result + BoolToStr(VBoolean, True);
        vtChar:     Result := Result + string(VChar);
        vtExtended: Result := Result + FloatToStr(VExtended^);
        vtString:   Result := Result + string(VString^);
        // maintened in case of future need, but will actually not arrive.
        vtPointer:  Result := Result + '^(' + Format('%P', [(addr(VPointer))]) + ')';
        vtPChar:    Result := Result + string(VPChar);
        // ...
        vtObject:
          begin
            if VObject = Nil Then
              Result := Result + '^(NIL)'
            else
              Result := Result + VObject.classname;
          end;
        // ...
        vtClass:      Result := Result + VClass.classname;
        vtWideChar:   Result := Result + string(VWideChar);
        vtPWideChar:  Result := Result + VPWideChar;
        vtAnsiString: Result := Result + string(VAnsiString);
        vtCurrency:   Result := Result + CurrToStr(VCurrency^);
        vtVariant:    Result := Result + string(VVariant^);
        vtInterface:  Result := Result + '(Interfaced object)';
        vtWideString: Result := Result + string(VWideString^);
        vtInt64:      Result := Result + IntToStr(VInt64^);
      else
        Result := Result + Format('[#HLvrType(%d)]', // "Else" not possible...
          [Integer(vr.VType)]); // ...with D6, but laters ?
      end; { case }
  EXCEPT
    Result := Result + Format('[#HLvrValue(%s)]', [vTypeDesc[vr.VType]]);
  end;
end;

// Function from HotLog by Olivier Touzot "QnnO".
function GetBasicValue(const s: String; vKind: Byte): String;
var
  iTmp: Integer;
  wasTObject: Boolean;
begin
  Result := s;
  If s = '' then
    exit;
  try
    iTmp := Pos('$_H_', s);
    wasTObject := (Pos('$_H_TObject', s) > 0);
    if (iTmp > 0) then
      Result := GetOriginalValue(s); // converts fake strings back to original
    Result := RightStr(Result, Length(Result) - 15);
    // From now on, works on "result"
    if (vKind In [vtString, vtAnsiString, vtWideString, vtPChar, vtWideChar,
      vtPWideChar]) And Not(wasTObject) then
      exit
    else
    begin
      iTmp := Pos(' ', Result);
      If (iTmp > 0) and (iTmp < Length(Result)) then
        Result := LeftStr(Result, iTmp);
    end;
  EXCEPT
    ;
  end;
end;

// Function from HotLog by Olivier Touzot "QnnO".
function ConstArrayToString(const Elements: array of const): String;
// -2-> Returns à string, surrounded by parenthesis : '(elts[0]; ...; elts[n-1]);'
// ("Basic infos" only.)
var
  i: Integer;
  s, sep: String;
Begin
  TRY
    if Length(Elements) = 0 then
    begin
      Result := '';
      exit;
    end;
    Result := '(';
    sep := '; ';
    for i := Low(Elements) to High(Elements) do
    begin
      s := VarRecToStr(Elements[i]);
      Result := Result + GetBasicValue(s, Elements[i].VType) + sep;
    end;
    Result := LeftStr(Result, Length(Result) - 2) + ');';
    // replaces last ", " by final ");".

  except
    Result := '[#HLvrConvert]';
  eND;
end;

function UserLog: TLogSession;
begin
  if Assigned(vCurrentLogger) then
    Result := vCurrentLogger.Log
  else
    Result := nil;
end;

function RemovePathAndExt(const AFileName: string): string;
var
  lExtIndex: Integer;
begin
  Result := ExtractFileName(AFileName);
  lExtIndex := Pos(ExtractFileExt(Result), Result);
  Result := Copy(Result, 1, lExtIndex - 1);
end;

procedure LogedAssert(const Message, FileName: string; LineNumber: Integer;
  ErrorAddr: Pointer);
begin
  UserLog.Log(Message + ': in ' + FileName + ' at line ' +
    IntToStr(LineNumber), lkError);
  Abort;
end;

function FileSize(const AFileName: String): Integer;
var
  sr: TSearchRec;
begin
  if FindFirst(AFileName, faAnyFile, sr) = 0 then
  begin
    Result := sr.Size;
    FindClose(sr);
  end
  else
    Result := -1;
end;

function ReadLine(var TextFile: Text): string;
var
  i: Word;
var
  s: string;
begin
  if EOF(TextFile) then
    exit;
  i := 1;
  repeat
    readln(TextFile, s);
  until (s <> '') and (s[1] <> '#') or EOF(TextFile);
  if s <> '' then
  begin
    while s[i] = ' ' do
      inc(i);
    if i = Length(s) then
      s := ''
    else
      s := Copy(s, i, Length(s) - i + 1);
  end;
  Result := s;
end;

// ------------------
// ------------------ TGLSLogger ------------------
// ------------------

constructor TGLSLogger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeFormat := lfElapsed;
  FLogLevels := llMax;
  vAssertErrorHandler := AssertErrorProc;
  vCurrentLogger := Self;
end;

destructor TGLSLogger.Destroy;
begin
  if vCurrentLogger = Self then
    vCurrentLogger := nil;
  if Assigned(FLog) then
    FLog.Destroy;
  inherited Destroy;
end;

function TGLSLogger.GetLog: TLogSession;
begin
  if not Assigned(FLog) then
    FLog := TLogSession.Init(Name + '.log', FTimeFormat, FLogLevels);
  Result := FLog;
end;

procedure TGLSLogger.DoPrimary;
begin
  vCurrentLogger := Self;
end;

procedure TGLSLogger.SetReplaceAssertion(Value: Boolean);
begin
  if Value <> FReplaceAssertion then
  begin
    FReplaceAssertion := Value;
    case FReplaceAssertion of
      True:
        AssertErrorProc := @LogedAssert;
      False:
        AssertErrorProc := @vAssertErrorHandler;
    end;
  end;
end;

// ------------------
// ------------------ TLogSession ------------------
// ------------------

procedure TLogSession.BackUpOldLogs(const ACurrentLogFileName: string);
var
  sRec: TSearchRec;
  lLogFileName: string;
  lLogOriginalDir: string;
  lLogSaveDir: string;
  lLogExt: string;

  procedure SaveCurrentFile();
  var
    lErrorMessage: string;
    lFile: File;
  begin
    if not FDisplayErrorDialogs then
      RenameFile(lLogOriginalDir + sRec.Name, lLogSaveDir + sRec.Name)
    else
    begin
      lErrorMessage := 'Renaming of "%s" failed with error : %d. Try again?';
      while not RenameFile(lLogOriginalDir + sRec.Name,
        lLogSaveDir + sRec.Name) do
      begin
        Log(lErrorMessage + '(' + FModeTitles[lkError] + ' = ' + IntToStr(FLogKindCount[lkError]) + ')');
        SetBuffered(False);

        AssignFile(lFile, lLogOriginalDir + sRec.Name);
        CloseFile(lFile);
        Halt;
      end;
    end;
  end;

begin
  lLogExt := ExtractFileExt(ACurrentLogFileName);
  lLogFileName := RemovePathAndExt(ACurrentLogFileName);
  lLogOriginalDir := ExtractFilePath(ACurrentLogFileName);
  lLogSaveDir := lLogOriginalDir + FormatDateTime('yyyy-mm-dd  hh-nn-ss', Now);

  if not CreateDir(lLogSaveDir) then
    exit;
  lLogSaveDir := lLogSaveDir + PathDelim;

  If FindFirst(lLogOriginalDir + lLogFileName + '*' + lLogExt, faAnyFile,
    sRec) = 0 then
  begin
    try
      SaveCurrentFile();
    except
    end;

    while (FindNext(sRec) = 0) do
      try
        SaveCurrentFile();
      except
      end;
    FindClose(sRec);
  end;
end;

procedure TLogSession.SetBuffered(const Value: Boolean);
begin
  if FBuffered = Value then
    exit;
  FBuffered := Value;
  ChangeBufferedState();
end;

procedure TLogSession.SetEnabled(const Value: Boolean);
begin
  if (FEnabled = Value) then
    exit;
  FEnabled := Value;
  if (FEnabled) then
    Log('Logging session resumed')
  else
    Log('Logging session paused');
end;

procedure TLogSession.SetLogFileMaxSize(const Value: Integer);
begin
  if FLogFileMaxSize = Value then
    exit;
  FLogFileMaxSize := Value;

  if FLogFileMaxSize > 0 then
  begin
    FCheckLogSizeThread := TLogCheckSizeThread.Create(Self);
    FCheckLogSizeThread.Start();
  end
  else
  begin
    FCheckLogSizeThread.Terminate();

    // Not really safe because we can wait forever.
    // But other methods known to me are platform-dependant.
    FCheckLogSizeThread.WaitFor();

    FCheckLogSizeThread.Free();
  end;
end;

procedure TLogSession.SetMode(const NewMode: TLogLevels);
begin
{$IFNDEF USE_LOGGING}
  if Self = v_GLSLogger then
    exit;
{$ENDIF}
  FLogLevels := NewMode;
  PrintLogLevels();
end;

function TLogSession.DoResetLog: Boolean;
begin
  try
    FFileAccessCriticalSection.Enter;
    Rewrite(FLogFile);
    CloseFile(FLogFile);
    FFileAccessCriticalSection.Leave;
    Result := True;
  except
    on E: Exception do
    begin
      // Ignore exceptions.
      Result := False;
      FFileAccessCriticalSection.Leave;
    end;
  end;
end;

function TLogSession.DoWriteBufferToLog: Boolean;
var
  i: Integer;
  lLast: Integer;
begin
  try
    // Open file.
    FFileAccessCriticalSection.Enter;
    Append(FLogFile);

    // Write buffer.
    lLast := FBuffer.Count - 1;
    for i := 0 to lLast do
      WriteLn(FLogFile, FBuffer[i]);

    // Clear buffer.
    FBufferCriticalSection.Enter;
    FBuffer.Clear();
    FBufferCriticalSection.Leave;

    // Close file.
    CloseFile(FLogFile);
    FFileAccessCriticalSection.Release();

    Result := True;
  except
    // Ignore exceptions.
    Result := False;
    FFileAccessCriticalSection.Release();
  end;
end;

function TLogSession.DoWriteToLog(const AString: string): Boolean;
begin
  try
    FFileAccessCriticalSection.Enter;
    Append(FLogFile);
    WriteLn(FLogFile, AString);
    CloseFile(FLogFile);
    FFileAccessCriticalSection.Release();
    Result := True;
  except
    // Ignore exceptions.
    Result := False;
    FFileAccessCriticalSection.Release();
  end;
end;

procedure TLogSession.FlushBuffer;
begin
  if Buffered then
    DoWriteBufferToLog();
end;

constructor TLogSession.Init(const AFileName: string;
  const ATimeFormat: TLogTimeFormat; const ALevels: TLogLevels;
  const ALogThreadId: Boolean = True; const ABuffered: Boolean = False;
  const AMaxSize: Integer = 0; const ABackUpOldLogs: Boolean = False;
  const AClearOldLogs: Boolean = True;
  const AWriteInternalMessages: Boolean = True);
var
  i: Integer;
  ModeStr: string;
begin
  FBuffer := TStringList.Create();
  FLogThreadId := ALogThreadId;
  FFlushBufferPeriod := 5000; // 5 sec.
  FCheckFileSizePeriod := 4000; // 4 sec.
  FBufferCriticalSection := TCriticalSection.Create;
  FFileAccessCriticalSection := TCriticalSection.Create;
  FBuffered := ABuffered; // Do not call the setter, create thread later.
  FStartedMs := GetTickCount;
  FTimeFormat := ATimeFormat;
  FLogLevels := ALevels;
  FMessageLimitAction := mlaHalt;
  FDisplayErrorDialogs := True;
  FDisplayLogOnExitIfItContains := [lkError, lkFatalError];
  FWriteInternalMessages := AWriteInternalMessages;

  // Set up strings.
  FModeTitles[lkDebug] := 'debug info';
  FModeTitles[lkInfo] := 'info';
  FModeTitles[lkNotice] := 'notices';
  FModeTitles[lkWarning] := 'warnings';
  FModeTitles[lkError] := 'errors';
  FModeTitles[lkFatalError] := 'fatal errors';

  case FTimeFormat of
    lfNone:
      ModeStr := 'no timestamp mode.';
    lfDate:
      ModeStr := 'date only mode.';
    lfTime:
      ModeStr := 'time only mode.';
    lfTimeExact:
      ModeStr := 'time mode with milliseconds.';
    lfDateTime:
      ModeStr := 'date and time mode.';
    lfElapsed:
      ModeStr := 'elapsed time mode.';
  end;

  if ABackUpOldLogs then
    BackUpOldLogs(AFileName);

  // Attach log file.
  FUsedLogFileNames := TStringList.Create();
  FOriginalLogFileName := AFileName;
  FEnabled := AttachLogFile(AFileName, AClearOldLogs);

  // Clear all logs and set log max size.
  if AMaxSize > 0 then
    ClearLogsInTheSameDir();
  Self.SetLogFileMaxSize(AMaxSize);

  // Reset log counters.
  for i := Ord(Low(TLogLevel)) to Ord(High(TLogLevel)) do
    FLogKindCount[TLogLevel(i)] := 0;

  // Print some initial logs.
  if FWriteInternalMessages then
  begin
    Log('Log subsystem started in ' + ModeStr, lkInfo);
    PrintLogLevels();
    Log('Buffered mode: ' + BoolToStr(FBuffered, True), lkInfo);
  end;

  // Start BufferProcessing thread.
  if FBuffered then
    ChangeBufferedState();
end;

{$IFNDEF USE_LOGGING}

constructor TLogSession.OnlyCreate;
begin
  inherited;
end;

{$ENDIF}

procedure TLogSession.PrintLogLevels;
var
  ModeStr: string;
  i: Integer;
begin
  ModeStr := '[';
  for i := Ord(Low(TLogLevel)) to Ord(High(TLogLevel)) do
    if TLogLevel(i) in FLogLevels then
    begin
      if ModeStr <> '[' then
        ModeStr := ModeStr + ', ';
      ModeStr := ModeStr + FModeTitles[TLogLevel(i)] + ' ' +
        Trim(lkPrefix[TLogLevel(i)]);
    end;
  ModeStr := ModeStr + ']';
  if FLogLevels = [] then
    ModeStr := 'nothing';
  Log('Logging ' + ModeStr, lkInfo);
end;

procedure TLogSession.PrintLogStatistics;
begin
  Log('Logged fatal_errors: ' + IntToStr(FLogKindCount[lkFatalError]) +
    ', errors: ' + IntToStr(FLogKindCount[lkError]) + ', warnings: ' +
    IntToStr(FLogKindCount[lkWarning]) + ', notices: ' +
    IntToStr(FLogKindCount[lkNotice]) + ', info: ' +
    IntToStr(FLogKindCount[lkInfo]) + ', debug: ' +
    IntToStr(FLogKindCount[lkDebug]));
end;

function TLogSession.AttachLogFile(const AFileName: string;
  const AResetFile: Boolean = True): Boolean;
var
  lPath: string;
begin
  try
    lPath := ExtractFilePath(AFileName);
    if Length(lPath) > 0 then
    begin
      FCurrentLogFileName := AFileName;
      ForceDirectories(lPath);
    end
    else
      FCurrentLogFileName := IncludeTrailingPathDelimiter(GetCurrentDir) +
        AFileName;

    FFileAccessCriticalSection.Enter;
    AssignFile(FLogFile, FCurrentLogFileName);
    FFileAccessCriticalSection.Leave;
    FUsedLogFileNames.Add(FCurrentLogFileName);

    if not FileExists(FCurrentLogFileName) then
      Result := DoResetLog()
    else
    begin
      if not AResetFile then
        Result := True
      else
        Result := DoResetLog();
    end;

  except
    FFileAccessCriticalSection.Leave;
    Result := False;
  end;
end;

procedure TLogSession.ChangeBufferedState();
begin
  if (FBuffered) then
  begin
    FBufferProcessingThread := TLogBufferFlushThread.Create(Self);
    FBufferProcessingThread.Start();
  end
  else
  begin
    FBufferProcessingThread.Terminate();

    // Not really safe because we can wait forever.
    // But other methods known to me are platform-dependant.
    FBufferProcessingThread.WaitFor();

    FBufferProcessingThread.Free();
  end;
end;

procedure TLogSession.ClearLogsInTheSameDir;
var
  sRec: TSearchRec;
  lFilePath: string;

  procedure DeleteCurrentFile();
  begin
    if FCurrentLogFileName <> lFilePath + sRec.Name then
      DeleteFile(lFilePath + sRec.Name);
  end;

begin
  lFilePath := ExtractFilePath(FCurrentLogFileName);

  If FindFirst(lFilePath + RemovePathAndExt(FCurrentLogFileName) + '*' +
    ExtractFileExt(FCurrentLogFileName), faAnyFile, sRec) = 0 then
  begin
    try
      DeleteCurrentFile()
    except
    end;

    while (FindNext(sRec) = 0) do
      try
        DeleteCurrentFile();
      except
      end;
    FindClose(sRec);
  end;
end;

procedure TLogSession.CreateNewLogFileIfNeeded;
var
  lNewFileName: string;
  i, Index: Integer;
  lFileSize: Integer;
begin
  try
    FFileAccessCriticalSection.Enter;
    lFileSize := FileSize(FCurrentLogFileName);
    FFileAccessCriticalSection.Leave();
  except
    lFileSize := -1;
    FFileAccessCriticalSection.Leave();
  end;

  if lFileSize >= FLogFileMaxSize then
  begin
    i := 1;
    lNewFileName := FOriginalLogFileName;

    repeat
      Index := LastDelimiter('.', FOriginalLogFileName);
      if Index = -1 then
        exit;
      lNewFileName := FOriginalLogFileName;
      Insert('_' + IntToStr(i), lNewFileName, Index);
      inc(i);
    until not FileExists(lNewFileName);

    if FWriteInternalMessages then
    begin
      Log(Format
        ('Creating new log file "%s" because old one became too big (%d bytes)',
        [lNewFileName, lFileSize]));
    end;
    AttachLogFile(lNewFileName, True);
  end;
end;

destructor TLogSession.Destroy;
var
  i: TLogLevel;
begin
  FDestroying := True;
{$IFNDEF USE_LOGGING}
  if Self = v_GLSLogger then
    exit;
{$ENDIF}
  if FWriteInternalMessages then
  begin
    PrintLogStatistics();
    Log('Log session shutdown');
  end;

  SetBuffered(False);
  DoWriteBufferToLog(); // Terminates TLogBufferFlushThread.
  FBuffer.Free;
  SetLogFileMaxSize(0); // Terminates TLogCheckSizeThread.

  // Display log?
  for i := Low(TLogLevel) to High(TLogLevel) do
    if (i in FDisplayLogOnExitIfItContains) and (FLogKindCount[i] > 0) then
    begin
      DisplayLog();
      Break;
    end;

  if Self = v_GLSLogger then
    v_GLSLogger := nil;

  FUsedLogFileNames.Destroy;
  FBufferCriticalSection.Destroy;
  FFileAccessCriticalSection.Destroy;
end;

procedure TLogSession.DisplayLog;
{$IF Defined(LINUX) and not Defined(CROSSVCL)}
var
  lProcess: TProcess;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', 'C:\WINDOWS\notepad.exe',
    PChar(FCurrentLogFileName), nil, 1);
{$ENDIF}
{$IF Defined(LINUX) and not Defined(CROSSVCL)}
  lProcess := TProcess.Create(nil);
  lProcess.CommandLine := 'gedit ' + FCurrentLogFileName;
  lProcess.Execute;
  lProcess.Destroy;
{$ENDIF}
end;

procedure TLogSession.Log(const Desc: string; const Level: TLogLevel = lkInfo);
begin
  AppendLog(Desc, Level);
end;

procedure TLogSession.LogAdv(const args: array of const;
  const ALevel: TLogLevel);
begin
  Log(ConstArrayToString(args), ALevel);
end;

procedure TLogSession.LogDebug(const Desc: string);
begin
  Log(Desc, lkDebug);
end;

procedure TLogSession.LogInfo(const Desc: string);
begin
  Log(Desc, lkInfo);
end;

procedure TLogSession.LogNotice(const Desc: string);
begin
  Log(Desc, lkNotice);
end;

procedure TLogSession.LogWarning(const Desc: string);
begin
  Log(Desc, lkWarning);
end;

procedure TLogSession.LogEmtryLine;
begin
  if not FEnabled then
    exit;

{$IFNDEF USE_LOGGING}
  if Self = v_GLSLogger then
    exit;
{$ENDIF}
  if FBuffered then
  begin
    // Critical section is always used.
    FBufferCriticalSection.Enter;
    FBuffer.Add('');
    FBufferCriticalSection.Leave;
  end
  else
  begin
    DoWriteToLog('');
  end;

  // IDELogProc.
  if (Self = v_GLSLogger) and Assigned(vIDELogProc) then
    vIDELogProc('');
end;

procedure TLogSession.LogError(const Desc: string);
begin
  Log(Desc, lkError);
end;

procedure TLogSession.LogFatalError(const Desc: string);
begin
  Log(Desc, lkFatalError);
end;

procedure TLogSession.LogDebugFmt(const Desc: string;
  const args: array of const);
begin
  Log(Format(Desc, args), lkDebug);
end;

procedure TLogSession.LogInfoFmt(const Desc: string;
  const args: array of const);
begin
  Log(Format(Desc, args), lkInfo);
end;

procedure TLogSession.LogNoticeFmt(const Desc: string;
  const args: array of const);
begin
  Log(Format(Desc, args), lkWarning);
end;

procedure TLogSession.LogWarningFmt(const Desc: string;
  const args: array of const);
begin
  Log(Format(Desc, args), lkWarning);
end;

procedure TLogSession.LogErrorFmt(const Desc: string;
  const args: array of const);
begin
  Log(Format(Desc, args), lkError);
end;

procedure TLogSession.LogException(const E: Exception;
  const aFunctionName: string; const args: array of const;
  const ALevel: TLogLevel = lkError);
begin
  Log('Exception in ' + aFunctionName + ': ' + E.Message + string(#13#10) +
    'Input parameters:' + string(#13#10) + ConstArrayToString(args), ALevel);
end;

procedure TLogSession.LogFatalErrorFmt(const Desc: string;
  const args: array of const);
begin
  Log(Format(Desc, args), lkFatalError);
end;

procedure TLogSession.AppendLog(const AString: string; const ALevel: TLogLevel;
  const ALogTime: Boolean);
var
  line: string;
begin
{$IFNDEF USE_LOGGING}
  if Self = v_GLSLogger then
    exit;
{$ENDIF}
  if not(ALevel in LogLevels) or not FEnabled then
    exit;

  if ALogTime then
    case FTimeFormat of
      lfNone:
        line := lkPrefix[ALevel] + AString;
      lfDate:
        line := DateToStr(Now) + #9 + lkPrefix[ALevel] + AString;
      lfTime:
        line := TimeToStr(Now) + #9 + lkPrefix[ALevel] + AString;
      lfTimeExact:
        line := FormatDateTime('hh:nn:ss zzz "ms"', Now) + #9 + lkPrefix[ALevel]
          + AString;
      lfDateTime:
        line := DateTimeToStr(Now) + #9 + lkPrefix[ALevel] + AString;
      lfElapsed:
        line := IntToStr(GetTickCount - FStartedMs) + #9 + lkPrefix[ALevel]
          + AString;
    end
  else
    line := AString;

{$IFDEF USE_MULTITHREAD}
  if (FLogThreadId) then
    line := #9 + 'Thread ID ' + IntToStr(GetCurrentThreadId) + #9 + line;
{$ENDIF}
  if FBuffered then
  begin
    // Critical section is always used.
    FBufferCriticalSection.Enter;
    FBuffer.Add(line);
    FBufferCriticalSection.Leave;
  end
  else
  begin
    DoWriteToLog(line);
  end;

  // IDELogProc.
  if (Self = v_GLSLogger) and Assigned(vIDELogProc) then
    vIDELogProc('Scene: ' + line);

  // Message limit?
  inc(FLogKindCount[ALevel]);
  if llMessageLimit[ALevel] < FLogKindCount[ALevel] then
    case FMessageLimitAction of
      mlaContinue: // Do nothing.
        ;

      mlaStopLogging:
        begin
          Log('Logging stopped due to reaching message limit (' + FModeTitles
            [ALevel] + ' = ' + IntToStr(FLogKindCount[ALevel]) + ')');
          FEnabled := False;
        end;

      mlaHalt:
        begin
          Log('Application halted due to reaching log message limit (' +
            FModeTitles[ALevel] + ' = ' + IntToStr(FLogKindCount[ALevel]) + ')');
          SetBuffered(False);
          Halt;
        end;
    end;
end;

// TLogBufferFlushThread

constructor TLogBufferFlushThread.Create(const AParent: TLogSession);
begin
  FParent := AParent;
  inherited Create(True);
end;

procedure TLogBufferFlushThread.Execute;
begin
  while (not Terminated) or (FParent.FBuffer.Count > 0) do
  begin
    FParent.DoWriteBufferToLog();
    Sleep(FParent.FFlushBufferPeriod);
  end;
end;

//-------------------------------
// TLogCheckSizeThread
//-------------------------------
constructor TLogCheckSizeThread.Create(const AParent: TLogSession);
begin
  FParent := AParent;
  inherited Create(True);
end;

procedure TLogCheckSizeThread.Execute;
begin
  while (not Terminated and not FParent.FDestroying) do
  begin
    FParent.CreateNewLogFileIfNeeded();
    Sleep(FParent.FCheckFileSizePeriod);
  end;
end;

// -----------------------------------------------
initialization
// -----------------------------------------------

finalization

if (v_GLSLogger <> nil) then
  v_GLSLogger.Destroy;

end.
