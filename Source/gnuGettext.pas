{ *------------------------------------------------------------------------------
  GNU gettext translation system for Delphi, Kylix, C++ Builder and others.
  All parts of the translation system are kept in this unit.

  @author Lars B. Dybdahl and others
  ------------------------------------------------------------------------------- }
unit gnugettext;
(* ************************************************************ *)
(* *)
(* (C) Copyright by Lars B. Dybdahl and others *)
(* E-mail: Lars@dybdahl.dk, phone +45 70201241 *)
(* *)
(* Contributors: Peter Thornqvist, Troy Wolbrink, *)
(* Frank Andreas de Groot, Igor Siticov, *)
(* Jacques Garcia Vazquez, Igor Gitman, *)
(* Arvid Winkelsdorf *)
(* *)
(* See http://dybdahl.dk/dxgettext/ for more information *)
(* *)
(* ************************************************************ *)

// Changes J. Rathlev (kontakt(a)rathlev-home.de)
// see: JR - 2011-07-29 / 2012-09-10
// All Delphi version related compiler switches removed - needs at least Delphi XE2
// GetWindowsLanguage uses GetUserPreferredUILanguages (Sept. 2023)

// Information about this file:
// $LastChangedDate: 2024-08-24 $

// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// The names of any contributor may not be used to endorse or promote
// products derived from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

interface

// If the conditional define DXGETTEXTDEBUG is defined, debugging log is activated.
// Use DefaultInstance.DebugLogToFile() to write the log to a file.
{ $define DXGETTEXTDEBUG }

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ELSE}
  Libc,
{$IFDEF FPC}
  CWString,
{$ENDIF}
{$ENDIF}
  System.Classes, System.StrUtils, System.SysUtils, System.TypInfo;

(* *************************************************************************** *)
(* *)
(* MAIN API *)
(* *)
(* *************************************************************************** *)

type
{$IFNDEF UNICODE}
  UnicodeString = WideString;
  RawUtf8String = AnsiString;
  RawByteString = AnsiString;
{$ELSE}
  RawUtf8String = RawByteString;
{$ENDIF}
  DomainString = string;
  LanguageString = string;
  ComponentNameString = string;
  FilenameString = string;
  MsgIdString = UnicodeString;
  TranslatedUnicodeString = UnicodeString;

  // Main GNU gettext functions. See documentation for instructions on how to use them.
function _(const szMsgId: MsgIdString): TranslatedUnicodeString;
function gettext(const szMsgId: MsgIdString): TranslatedUnicodeString;
function gettext_NoExtract(const szMsgId: MsgIdString): TranslatedUnicodeString;
function gettext_NoOp(const szMsgId: MsgIdString): TranslatedUnicodeString;
function dgettext(const szDomain: DomainString; const szMsgId: MsgIdString)
  : TranslatedUnicodeString;
function dgettext_NoExtract(const szDomain: DomainString;
  const szMsgId: MsgIdString): TranslatedUnicodeString;
function dngettext(const szDomain: DomainString;
  const singular, plural: MsgIdString; Number: longint)
  : TranslatedUnicodeString;
function ngettext(const singular, plural: MsgIdString; Number: longint)
  : TranslatedUnicodeString;
function ngettext_NoExtract(const singular, plural: MsgIdString;
  Number: longint): TranslatedUnicodeString;
procedure textdomain(const szDomain: DomainString);
function getcurrenttextdomain: DomainString;
procedure bindtextdomain(const szDomain: DomainString;
  const szDirectory: FilenameString);

// Set language to use
procedure UseLanguage(LanguageCode: LanguageString);
function GetCurrentLanguage: LanguageString;

// Translates a component (form, frame etc.) to the currently selected language.
// Put TranslateComponent(self) in the OnCreate event of all your forms.
// See the manual for documentation on these functions
type
  TTranslator = procedure(obj: TObject) of object;

procedure TP_Ignore(AnObject: TObject; const name: ComponentNameString);
procedure TP_IgnoreClass(IgnClass: TClass);
procedure TP_IgnoreClassProperty(IgnClass: TClass;
  const propertyname: ComponentNameString);
procedure TP_GlobalIgnoreClass(IgnClass: TClass);
procedure TP_GlobalIgnoreClassProperty(IgnClass: TClass;
  const propertyname: ComponentNameString);
procedure TP_GlobalHandleClass(HClass: TClass; Handler: TTranslator);
procedure TranslateComponent(AnObject: TComponent;
  const textdomain: DomainString = '');
procedure RetranslateComponent(AnObject: TComponent;
  const textdomain: DomainString = '');

// Add more domains that resourcestrings can be extracted from. If a translation
// is not found in the default domain, this domain will be searched, too.
// This is useful for adding mo files for certain runtime libraries and 3rd
// party component libraries
procedure AddDomainForResourceString(const domain: DomainString);
procedure RemoveDomainForResourceString(const domain: DomainString);
procedure AddDomains(const Domains: array of DomainString);

// Add more domains that component strings can be extracted from. If a translation
// is not found in the default domain, this domain will be searched, too.
// This is useful when an application inherits components from a 3rd
// party component libraries
procedure AddDomainForComponent(const domain: DomainString);
procedure RemoveDomainForComponent(const domain: DomainString);

// Unicode-enabled way to get resourcestrings, automatically translated
// Use like this: ws:=LoadResStringW(@NameOfResourceString);
function LoadResString(ResStringRec: PResStringRec): WideString;
function LoadResStringW(ResStringRec: PResStringRec): UnicodeString;

// This returns an empty string if not translated or translator name is not specified.
function GetTranslatorNameAndEmail: TranslatedUnicodeString;

(* *************************************************************************** *)
(* *)
(* ADVANCED FUNCTIONALITY *)
(* *)
(* *************************************************************************** *)

const
  DefaultTextDomain = 'default';

var
  ExecutableFilename: FilenameString;
  // This is set to paramstr(0) or the name of the DLL you are creating.

const
  PreferExternal = True;
  // Set to true, to prefer external *.mo over embedded translation
  UseMemoryMappedFiles = True;
  // Set to False, to use the mo-file as independent copy in memory (you can update the file while it is in use)
  ReReadMoFileOnSameLanguage = True;
  // Set to True, to reread mo-file if the current language is selected again

const
  // Subversion source code version control version information
  VCSVersion = '$LastChangedRevision: 220 $';

type
  EGnuGettext = class(Exception);
  EGGProgrammingError = class(EGnuGettext);
  EGGComponentError = class(EGnuGettext);
  EGGIOError = class(EGnuGettext);
  EGGAnsi2WideConvError = class(EGnuGettext);

  // This function will turn resourcestring hooks on or off, eventually with BPL file support.
  // Please do not activate BPL file support when the package is in design mode.
const
  AutoCreateHooks = True;
procedure HookIntoResourceStrings(enabled: boolean = True;
  SupportPackages: boolean = false);

(* *************************************************************************** *)
(* *)
(* CLASS based implementation. *)
(* Use TGnuGettextInstance to have more than one language *)
(* in your application at the same time *)
(* *)
(* *************************************************************************** *)

type
  TOnDebugLine = Procedure(Sender: TObject; const Line: String;
    var Discard: boolean) of Object;
  // Set Discard to false if output should still go to ordinary debug log
  TGetPluralForm = function(Number: longint): Integer;
  TDebugLogger = procedure(Line: AnsiString) of object;

  { *------------------------------------------------------------------------------
    Handles .mo files, in separate files or inside the exe file.
    Don't use this class. It's for internal use.
    ------------------------------------------------------------------------------- }
  TMoFile = class
  /// Threadsafe. Only constructor and destructor are writing to memory
  private
    doswap: boolean;
  public
    Users: Integer;
    /// Reference count. If it reaches zero, this object should be destroyed.
    constructor Create(const filename: FilenameString; const Offset: int64;
      Size: int64; const xUseMemoryMappedFiles: boolean);
    destructor Destroy; override;
    function gettext(const msgid: RawUtf8String; var found: boolean)
      : RawUtf8String; // uses mo file and utf-8
    property isSwappedArchitecture: boolean read doswap;
  private
    N, O, T: Cardinal;
    /// Values defined at http://www.linuxselfhelp.com/gnu/gettext/html_chapter/gettext_6.html
    startindex, startstep: Integer;
    FUseMemoryMappedFiles: boolean;
    mo: THandle;
    momapping: THandle;
    momemoryHandle: PAnsiChar;
    momemory: PAnsiChar;
    function autoswap32(i: Cardinal): Cardinal;
    function CardinalInMem(baseptr: PAnsiChar; Offset: Cardinal): Cardinal;
  end;

  { *------------------------------------------------------------------------------
    Handles all issues regarding a specific domain.
    Don't use this class. It's for internal use.
    ------------------------------------------------------------------------------- }
  TDomain = class
  private
    enabled: boolean;
    vDirectory: FilenameString;
    procedure setDirectory(const dir: FilenameString);
  public
    DebugLogger: TDebugLogger;
    domain: DomainString;
    property Directory: FilenameString read vDirectory write setDirectory;
    constructor Create;
    destructor Destroy; override;
    // Set parameters
    procedure SetLanguageCode(const langcode: LanguageString);
    procedure SetFilename(const filename: FilenameString);
    // Bind this domain to a specific file
    // Get information
    procedure GetListOfLanguages(list: TStrings);
    function GetTranslationProperty(propertyname: ComponentNameString)
      : TranslatedUnicodeString;
    function gettext(const msgid: RawUtf8String): RawUtf8String;
    // uses mo file and utf-8
  private
    mofile: TMoFile;
    SpecificFilename: FilenameString;
    curlang: LanguageString;
    OpenHasFailedBefore: boolean;
    procedure OpenMoFile;
    procedure CloseMoFile;
  end;

  { *------------------------------------------------------------------------------
    Helper class for invoking events.
    ------------------------------------------------------------------------------- }
  TExecutable = class
    procedure Execute; virtual; abstract;
  end;

  { *------------------------------------------------------------------------------
    Interface to implement if you want to register as a language change listener
    ------------------------------------------------------------------------------- }
  IGnuGettextInstanceWhenNewLanguageListener = interface
    procedure WhenNewLanguage(const LanguageID: LanguageString);
  end;

  { *------------------------------------------------------------------------------
    The main translation engine.
    ------------------------------------------------------------------------------- }
  TGnuGettextInstance = class
  private
    fOnDebugLine: TOnDebugLine;
  public
    enabled: boolean;
    /// Set this to false to disable translations
    DesignTimeCodePage: Integer;
    /// See MultiByteToWideChar() in Win32 API for documentation
    constructor Create;
    destructor Destroy; override;
    procedure UseLanguage(LanguageCode: LanguageString);
    procedure GetListOfLanguages(const domain: DomainString; list: TStrings);
    // Puts list of language codes, for which there are translations in the specified domain, into list
{$IFNDEF UNICODE}
    function gettext(const szMsgId: AnsiString): TranslatedUnicodeString;
      overload; virtual;
    function ngettext(const singular, plural: AnsiString; Number: longint)
      : TranslatedUnicodeString; overload; virtual;
{$ENDIF}
    function gettext(const szMsgId: MsgIdString): TranslatedUnicodeString;
      overload; virtual;
    function gettext_NoExtract(const szMsgId: MsgIdString)
      : TranslatedUnicodeString;
    function gettext_NoOp(const szMsgId: MsgIdString): TranslatedUnicodeString;
    function ngettext(const singular, plural: MsgIdString; Number: longint)
      : TranslatedUnicodeString; overload; virtual;
    function ngettext_NoExtract(const singular, plural: MsgIdString;
      Number: longint): TranslatedUnicodeString;
    function GetCurrentLanguage: LanguageString;
    function GetTranslationProperty(const propertyname: ComponentNameString)
      : TranslatedUnicodeString;
    function GetTranslatorNameAndEmail: TranslatedUnicodeString;

    // Form translation tools, these are not threadsafe. All TP_ procs must be called just before TranslateProperites()
    procedure TP_Ignore(AnObject: TObject; const name: ComponentNameString);
    procedure TP_IgnoreClass(IgnClass: TClass);
    procedure TP_IgnoreClassProperty(IgnClass: TClass;
      propertyname: ComponentNameString);
    procedure TP_GlobalIgnoreClass(IgnClass: TClass);
    procedure TP_GlobalIgnoreClassProperty(IgnClass: TClass;
      propertyname: ComponentNameString);
    procedure TP_GlobalHandleClass(HClass: TClass; Handler: TTranslator);
    procedure TranslateProperties(AnObject: TObject;
      textdomain: DomainString = '');
    procedure TranslateComponent(AnObject: TComponent;
      const textdomain: DomainString = '');
    procedure RetranslateComponent(AnObject: TComponent;
      const textdomain: DomainString = '');

    // Multi-domain functions
{$IFNDEF UNICODE}
    function dgettext(const szDomain: DomainString; const szMsgId: AnsiString)
      : TranslatedUnicodeString; overload; virtual;
    function dngettext(const szDomain: DomainString;
      const singular, plural: AnsiString; Number: longint)
      : TranslatedUnicodeString; overload; virtual;
{$ENDIF}
    function dgettext(const szDomain: DomainString; const szMsgId: MsgIdString)
      : TranslatedUnicodeString; overload; virtual;
    function dgettext_NoExtract(const szDomain: DomainString;
      const szMsgId: MsgIdString): TranslatedUnicodeString;
    function dngettext(const szDomain: DomainString;
      const singular, plural: MsgIdString; Number: longint)
      : TranslatedUnicodeString; overload; virtual;
    function dngettext_NoExtract(const szDomain: DomainString;
      const singular, plural: MsgIdString; Number: longint)
      : TranslatedUnicodeString;
    procedure textdomain(const szDomain: DomainString);
    function getcurrenttextdomain: DomainString;
    procedure bindtextdomain(const szDomain: DomainString;
      const szDirectory: FilenameString);
    procedure bindtextdomainToFile(const szDomain: DomainString;
      const filename: FilenameString);
    // Also works with files embedded in exe file

    // Windows API functions
    function LoadResString(ResStringRec: PResStringRec): UnicodeString;

    // Output all log info to this file. This may only be called once.
    procedure DebugLogToFile(const filename: FilenameString;
      append: boolean = false);
    procedure DebugLogPause(PauseEnabled: boolean);
    property OnDebugLine: TOnDebugLine read fOnDebugLine write fOnDebugLine;
    // If set, all debug output goes here
{$IFNDEF UNICODE}
    // Conversion according to design-time character set
    function ansi2wideDTCP(const s: AnsiString): MsgIdString;
    // Convert using Design Time Code Page
{$ENDIF}
    procedure RegisterWhenNewLanguageListener
      (Listener: IGnuGettextInstanceWhenNewLanguageListener);
    procedure UnregisterWhenNewLanguageListener
      (Listener: IGnuGettextInstanceWhenNewLanguageListener);
  protected
    procedure TranslateStrings(sl: TStrings; const textdomain: DomainString);

    // Override these three, if you want to inherited from this class
    // to create a new class that handles other domain and language dependent
    // issues
    procedure WhenNewLanguage(const LanguageID: LanguageString); virtual;
    // Override to know when language changes
    procedure WhenNewDomain(const textdomain: DomainString); virtual;
    // Override to know when text domain changes. Directory is purely informational
    procedure WhenNewDomainDirectory(const textdomain: DomainString;
      const Directory: FilenameString); virtual;
    // Override to know when any text domain's directory changes. It won't be called if a domain is fixed to a specific file.
  private
    curlang: LanguageString;
    curGetPluralForm: TGetPluralForm;
    curmsgdomain: DomainString;
    savefileCS: TMultiReadExclusiveWriteSynchronizer;
    savefile: TextFile;
    savememory: TStringList;
    DefaultDomainDirectory: FilenameString;
    domainlist: TStringList;
    /// List of domain names. Objects are TDomain.
    TP_IgnoreList: TStringList;
    /// Temporary list, reset each time TranslateProperties is called
    TP_ClassHandling: TList;
    /// Items are TClassMode. If a is derived from b, a comes first
    TP_GlobalClassHandling: TList;
    /// Items are TClassMode. If a is derived from b, a comes first
    TP_Retranslator: TExecutable;
    /// Cast this to TTP_Retranslator
    fWhenNewLanguageListeners: TInterfaceList;
    /// List of all registered WhenNewLanguage listeners
{$IFDEF DXGETTEXTDEBUG}
    DebugLogCS: TMultiReadExclusiveWriteSynchronizer;
    DebugLog: TStream;
    DebugLogOutputPaused: boolean;
{$ENDIF}
    function TP_CreateRetranslator: TExecutable; // Must be freed by caller!
    procedure FreeTP_ClassHandlingItems;
{$IFDEF DXGETTEXTDEBUG}
    procedure DebugWriteln(Line: AnsiString);
{$ENDIF}
    procedure TranslateProperty(AnObject: TObject; PropInfo: PPropInfo;
      TodoList: TStrings; const textdomain: DomainString);
    function Getdomain(const domain: DomainString;
      const DefaultDomainDirectory: FilenameString;
      const curlang: LanguageString): TDomain;
    // Translates a single property of an object
  end;

var
  DefaultInstance: TGnuGettextInstance;
  /// Default instance of the main API for singlethreaded applications.

implementation

{$IFNDEF MSWINDOWS}
{$IFNDEF LINUX}
  'This version of gnugettext.pas is only meant to be compiled with Kylix 3,'
  'Delphi 6, Delphi 7 and later versions. If you use other versions, please'
  'get the gnugettext.pas version from the Delphi 5 directory.'
{$ENDIF}
{$ENDIF}
(* ************************************************************************ *)
// Some comments on the implementation:
// This unit should be independent of other units where possible.
// It should have a small footprint in any way.
(* ************************************************************************ *)
// TMultiReadExclusiveWriteSynchronizer is used instead of TCriticalSection
// because it makes this unit independent of the SyncObjs unit
(* ************************************************************************ *)

{$B-,R+,I+,Q+}
  type TTP_RetranslatorItem = class obj: TObject;
Propname:
ComponentNameString;
OldValue:
TranslatedUnicodeString;
end;
TTP_Retranslator = class(TExecutable)textdomain: DomainString;
Instance:
TGnuGettextInstance;

constructor Create;
  destructor Destroy; override;
    procedure Remember(obj: TObject; Propname: ComponentNameString;
      OldValue: TranslatedUnicodeString);
      procedure Execute; override;
      private
        list: TList;
        end;
        TEmbeddedFileInfo = class Offset, Size: int64;
        end;
        TFileLocator = class
        // This class finds files even when embedded inside executable
          constructor Create;
        destructor Destroy;
        override;
        function FindSignaturePos(const signature: RawByteString;
          str: TFileStream): int64;
        procedure Analyze; // List files embedded inside executable
        function FileExists(filename: FilenameString): boolean;
        function GetMoFile(filename: FilenameString;
          DebugLogger: TDebugLogger): TMoFile;
        procedure ReleaseMoFile(mofile: TMoFile);
      private
        basedirectory: FilenameString;
        filelist: TStringList;
        // Objects are TEmbeddedFileInfo. Filenames are relative to .exe file
        MoFilesCS: TMultiReadExclusiveWriteSynchronizer;
        MoFiles: TStringList;
        // Objects are filenames+offset, objects are TMoFile
        function ReadInt64(str: TStream): int64;
        end;
        TGnuGettextComponentMarker = class(TComponent)public LastLanguage
          : LanguageString;
        Retranslator: TExecutable;
        destructor Destroy;
        override;
        end;
        TClassMode = class HClass: TClass;
        SpecialHandler: TTranslator;
        PropertiesToIgnore: TStringList; // This is ignored if Handler is set
        constructor Create;
        destructor Destroy;
        override;
        end;
        TRStrinfo = record strlength, stroffset: Cardinal;
        end;
        TStrInfoArr = array [0 .. 10000000] of TRStrinfo;
        PStrInfoArr = ^TStrInfoArr;
        TCharArray5 = array [0 .. 4] of ansichar;
        THook = // Replaces a runtime library procedure with a custom procedure
          class public constructor Create(OldProcedure, NewProcedure: pointer;
          FollowJump: boolean = false);
        destructor Destroy;
        override; // Restores unhooked state
        procedure Reset(FollowJump: boolean = false);
        // Disables and picks up patch points again
        procedure Disable;
        procedure Enable;
      private
        oldproc, newproc: pointer;
        Patch: TCharArray5;
        Original: TCharArray5;
        PatchPosition: PAnsiChar;
        procedure Shutdown;
        // Same as destroy, except that object is not destroyed
        end;

      var
        // System information
        Win32PlatformIsUnicode: boolean = false;

        // Information about files embedded inside .exe file
        FileLocator: TFileLocator;

        // Hooks into runtime library functions
        ResourceStringDomainListCS: TMultiReadExclusiveWriteSynchronizer;
        ResourceStringDomainList: TStringList;
        ComponentDomainListCS: TMultiReadExclusiveWriteSynchronizer;
        ComponentDomainList: TStringList;
        HookLoadResString: THook;
        HookLoadStr: THook;
        HookFmtLoadStr: THook;

        function GGGetEnvironmentVariable(const name: WideString): WideString;
        var
          Len: Integer;
          W: WideString;
        begin
          Result := '';
          SetLength(W, 1);
          Len := GetEnvironmentVariableW(PWideChar(Name), PWideChar(W), 1);
          if Len > 0 then
          begin
            SetLength(Result, Len - 1);
            GetEnvironmentVariableW(PWideChar(Name), PWideChar(Result), Len);
          end;
        end;

        function StripCRRawMsgId(s: RawUtf8String): RawUtf8String;
        var
          i: Integer;
        begin
          i := 1;
          while i <= length(s) do
          begin
            if s[i] = #13 then
              delete(s, i, 1)
            else
              inc(i);
          end;
          Result := s;
        end;

        function EnsureLineBreakInTranslatedString(s: RawUtf8String)
          : RawUtf8String;
{$IFDEF MSWINDOWS}
        var
          i: Integer;
{$ENDIF}
        begin
{$IFDEF MSWINDOWS}
          Assert(sLinebreak = AnsiString(#13#10));
          i := 1;
          while i <= length(s) do
          begin
            if (s[i] = #10) and (MidStr(s, i - 1, 1) <> #13) then
            begin
              insert(#13, s, i);
              inc(i, 2);
            end
            else
              inc(i);
          end;
{$ENDIF}
          Result := s;
        end;

        function IsWriteProp(Info: PPropInfo): boolean;
        begin
          Result := Assigned(Info) and (Info^.SetProc <> nil);
        end;

        function ResourceStringGettext(msgid: MsgIdString)
          : TranslatedUnicodeString;
        var
          i: Integer;
        begin
          if (msgid = '') or (ResourceStringDomainListCS = nil) then
          begin
            // This only happens during very complicated program startups that fail,
            // or when Msgid=''
            Result := msgid;
            exit;
          end;
          ResourceStringDomainListCS.BeginRead;
          try
            for i := 0 to ResourceStringDomainList.Count - 1 do
            begin
              Result := dgettext(ResourceStringDomainList.Strings[i], msgid);
              if Result <> msgid then
                break;
            end;
          finally
            ResourceStringDomainListCS.EndRead;
          end;
        end;

        function ComponentGettext(msgid: MsgIdString;
          Instance: TGnuGettextInstance = nil): TranslatedUnicodeString;
        var
          i: Integer;
        begin
          if (msgid = '') or (ComponentDomainListCS = nil) then
          begin
            // This only happens during very complicated program startups that fail,
            // or when Msgid=''
            Result := msgid;
            exit;
          end;
          ComponentDomainListCS.BeginRead;
          try
            for i := 0 to ComponentDomainList.Count - 1 do
            begin
              if Assigned(Instance) then
                Result := Instance.dgettext
                  (ComponentDomainList.Strings[i], msgid)
              else
                Result := dgettext(ComponentDomainList.Strings[i], msgid);
              if Result <> msgid then
                break;
            end;
          finally
            ComponentDomainListCS.EndRead;
          end;
        end;

        function gettext(const szMsgId: MsgIdString): TranslatedUnicodeString;
        begin
          Result := DefaultInstance.gettext(szMsgId);
        end;

        function gettext_NoExtract(const szMsgId: MsgIdString)
          : TranslatedUnicodeString;
        begin
          // This one is very useful for translating text in variables.
          // This can sometimes be necessary, and by using this function,
          // the source code scanner will not trigger warnings.
          Result := gettext(szMsgId);
        end;

        function gettext_NoOp(const szMsgId: MsgIdString)
          : TranslatedUnicodeString;
        begin
          // *** With this function Strings can be added to the po-file without beeing
          // ResourceStrings (dxgettext will add the string and this function will
          // return it without a change)
          // see gettext manual
          // 4.7 - Special Cases of Translatable Strings
          // http://www.gnu.org/software/hello/manual/gettext/Special-cases.html#Special-cases
          Result := DefaultInstance.gettext_NoOp(szMsgId);
        end;

      { *------------------------------------------------------------------------------
        This is the main translation procedure used in programs. It takes a parameter,
        looks it up in the translation dictionary, and returns the translation.
        If no translation is found, the parameter is returned.

        @param szMsgId The text, that should be displayed if no translation is found.
        ------------------------------------------------------------------------------- }
        function _(const szMsgId: MsgIdString): TranslatedUnicodeString;
        begin
          Result := DefaultInstance.gettext(szMsgId);
        end;

      { *------------------------------------------------------------------------------
        Translates a text, using a specified translation domain.
        If no translation is found, the parameter is returned.

        @param szDomain Which translation domain that should be searched for a translation.
        @param szMsgId The text, that should be displayed if no translation is found.
        ------------------------------------------------------------------------------- }
        function dgettext(const szDomain: DomainString;
          const szMsgId: MsgIdString): TranslatedUnicodeString;
        begin
          Result := DefaultInstance.dgettext(szDomain, szMsgId);
        end;

        function dgettext_NoExtract(const szDomain: DomainString;
          const szMsgId: MsgIdString): TranslatedUnicodeString;
        begin
          // This one is very useful for translating text in variables.
          // This can sometimes be necessary, and by using this function,
          // the source code scanner will not trigger warnings.
          Result := dgettext(szDomain, szMsgId);
        end;

        function dngettext(const szDomain: DomainString;
          const singular, plural: MsgIdString; Number: longint)
          : TranslatedUnicodeString;
        begin
          Result := DefaultInstance.dngettext(szDomain, singular,
            plural, Number);
        end;

        function ngettext(const singular, plural: MsgIdString; Number: longint)
          : TranslatedUnicodeString;
        begin
          Result := DefaultInstance.ngettext(singular, plural, Number);
        end;

        function ngettext_NoExtract(const singular, plural: MsgIdString;
          Number: longint): TranslatedUnicodeString;
        begin
          // This one is very useful for translating text in variables.
          // This can sometimes be necessary, and by using this function,
          // the source code scanner will not trigger warnings.
          Result := ngettext(singular, plural, Number);
        end;

        procedure textdomain(const szDomain: DomainString);
        begin
          DefaultInstance.textdomain(szDomain);
        end;

        procedure SetGettextEnabled(enabled: boolean);
        begin
          DefaultInstance.enabled := enabled;
        end;

        function getcurrenttextdomain: DomainString;
        begin
          Result := DefaultInstance.getcurrenttextdomain;
        end;

        procedure bindtextdomain(const szDomain: DomainString;
          const szDirectory: FilenameString);
        begin
          DefaultInstance.bindtextdomain(szDomain, szDirectory);
        end;

        procedure TP_Ignore(AnObject: TObject; const name: FilenameString);
        begin
          DefaultInstance.TP_Ignore(AnObject, name);
        end;

        procedure TP_GlobalIgnoreClass(IgnClass: TClass);
        begin
          DefaultInstance.TP_GlobalIgnoreClass(IgnClass);
        end;

        procedure TP_IgnoreClass(IgnClass: TClass);
        begin
          DefaultInstance.TP_IgnoreClass(IgnClass);
        end;

        procedure TP_IgnoreClassProperty(IgnClass: TClass;
          const propertyname: ComponentNameString);
        begin
          DefaultInstance.TP_IgnoreClassProperty(IgnClass, propertyname);
        end;

        procedure TP_GlobalIgnoreClassProperty(IgnClass: TClass;
          const propertyname: ComponentNameString);
        begin
          DefaultInstance.TP_GlobalIgnoreClassProperty(IgnClass, propertyname);
        end;

        procedure TP_GlobalHandleClass(HClass: TClass; Handler: TTranslator);
        begin
          DefaultInstance.TP_GlobalHandleClass(HClass, Handler);
        end;

        procedure TranslateComponent(AnObject: TComponent;
          const textdomain: DomainString = '');
        begin
          DefaultInstance.TranslateComponent(AnObject, textdomain);
        end;

        procedure RetranslateComponent(AnObject: TComponent;
          const textdomain: DomainString = '');
        begin
          DefaultInstance.RetranslateComponent(AnObject, textdomain);
        end;

{$IFDEF MSWINDOWS}

      // These constants are only used in Windows 95
      // Thanks to Frank Andreas de Groot for this table
      const
        IDAfrikaans = $0436;
        IDAlbanian = $041C;
        IDArabicAlgeria = $1401;
        IDArabicBahrain = $3C01;
        IDArabicEgypt = $0C01;
        IDArabicIraq = $0801;
        IDArabicJordan = $2C01;
        IDArabicKuwait = $3401;
        IDArabicLebanon = $3001;
        IDArabicLibya = $1001;
        IDArabicMorocco = $1801;
        IDArabicOman = $2001;
        IDArabicQatar = $4001;
        IDArabic = $0401;
        IDArabicSyria = $2801;
        IDArabicTunisia = $1C01;
        IDArabicUAE = $3801;
        IDArabicYemen = $2401;
        IDArmenian = $042B;
        IDAssamese = $044D;
        IDAzeriCyrillic = $082C;
        IDAzeriLatin = $042C;
        IDBasque = $042D;
        IDByelorussian = $0423;
        IDBengali = $0445;
        IDBulgarian = $0402;
        IDBurmese = $0455;
        IDCatalan = $0403;
        IDChineseHongKong = $0C04;
        IDChineseMacao = $1404;
        IDSimplifiedChinese = $0804;
        IDChineseSingapore = $1004;
        IDTraditionalChinese = $0404;
        IDCroatian = $041A;
        IDCzech = $0405;
        IDDanish = $0406;
        IDBelgianDutch = $0813;
        IDDutch = $0413;
        IDEnglishAUS = $0C09;
        IDEnglishBelize = $2809;
        IDEnglishCanadian = $1009;
        IDEnglishCaribbean = $2409;
        IDEnglishIreland = $1809;
        IDEnglishJamaica = $2009;
        IDEnglishNewZealand = $1409;
        IDEnglishPhilippines = $3409;
        IDEnglishSouthAfrica = $1C09;
        IDEnglishTrinidad = $2C09;
        IDEnglishUK = $0809;
        IDEnglishUS = $0409;
        IDEnglishZimbabwe = $3009;
        IDEstonian = $0425;
        IDFaeroese = $0438;
        IDFarsi = $0429;
        IDFinnish = $040B;
        IDBelgianFrench = $080C;
        IDFrenchCameroon = $2C0C;
        IDFrenchCanadian = $0C0C;
        IDFrenchCotedIvoire = $300C;
        IDFrench = $040C;
        IDFrenchLuxembourg = $140C;
        IDFrenchMali = $340C;
        IDFrenchMonaco = $180C;
        IDFrenchReunion = $200C;
        IDFrenchSenegal = $280C;
        IDSwissFrench = $100C;
        IDFrenchWestIndies = $1C0C;
        IDFrenchZaire = $240C;
        IDFrisianNetherlands = $0462;
        IDGaelicIreland = $083C;
        IDGaelicScotland = $043C;
        IDGalician = $0456;
        IDGeorgian = $0437;
        IDGermanAustria = $0C07;
        IDGerman = $0407;
        IDGermanLiechtenstein = $1407;
        IDGermanLuxembourg = $1007;
        IDSwissGerman = $0807;
        IDGreek = $0408;
        IDGujarati = $0447;
        IDHebrew = $040D;
        IDHindi = $0439;
        IDHungarian = $040E;
        IDIcelandic = $040F;
        IDIndonesian = $0421;
        IDItalian = $0410;
        IDSwissItalian = $0810;
        IDJapanese = $0411;
        IDKannada = $044B;
        IDKashmiri = $0460;
        IDKazakh = $043F;
        IDKhmer = $0453;
        IDKirghiz = $0440;
        IDKonkani = $0457;
        IDKorean = $0412;
        IDLao = $0454;
        IDLatvian = $0426;
        IDLithuanian = $0427;
        IDMacedonian = $042F;
        IDMalaysian = $043E;
        IDMalayBruneiDarussalam = $083E;
        IDMalayalam = $044C;
        IDMaltese = $043A;
        IDManipuri = $0458;
        IDMarathi = $044E;
        IDMongolian = $0450;
        IDNepali = $0461;
        IDNorwegianBokmol = $0414;
        IDNorwegianNynorsk = $0814;
        IDOriya = $0448;
        IDPolish = $0415;
        IDBrazilianPortuguese = $0416;
        IDPortuguese = $0816;
        IDPunjabi = $0446;
        IDRhaetoRomanic = $0417;
        IDRomanianMoldova = $0818;
        IDRomanian = $0418;
        IDRussianMoldova = $0819;
        IDRussian = $0419;
        IDSamiLappish = $043B;
        IDSanskrit = $044F;
        IDSerbianCyrillic = $0C1A;
        IDSerbianLatin = $081A;
        IDSesotho = $0430;
        IDSindhi = $0459;
        IDSlovak = $041B;
        IDSlovenian = $0424;
        IDSorbian = $042E;
        IDSpanishArgentina = $2C0A;
        IDSpanishBolivia = $400A;
        IDSpanishChile = $340A;
        IDSpanishColombia = $240A;
        IDSpanishCostaRica = $140A;
        IDSpanishDominicanRepublic = $1C0A;
        IDSpanishEcuador = $300A;
        IDSpanishElSalvador = $440A;
        IDSpanishGuatemala = $100A;
        IDSpanishHonduras = $480A;
        IDMexicanSpanish = $080A;
        IDSpanishNicaragua = $4C0A;
        IDSpanishPanama = $180A;
        IDSpanishParaguay = $3C0A;
        IDSpanishPeru = $280A;
        IDSpanishPuertoRico = $500A;
        IDSpanishModernSort = $0C0A;
        IDSpanish = $040A;
        IDSpanishUruguay = $380A;
        IDSpanishVenezuela = $200A;
        IDSutu = $0430;
        IDSwahili = $0441;
        IDSwedishFinland = $081D;
        IDSwedish = $041D;
        IDTajik = $0428;
        IDTamil = $0449;
        IDTatar = $0444;
        IDTelugu = $044A;
        IDThai = $041E;
        IDTibetan = $0451;
        IDTsonga = $0431;
        IDTswana = $0432;
        IDTurkish = $041F;
        IDTurkmen = $0442;
        IDUkrainian = $0422;
        IDUrdu = $0420;
        IDUzbekCyrillic = $0843;
        IDUzbekLatin = $0443;
        IDVenda = $0433;
        IDVietnamese = $042A;
        IDWelsh = $0452;
        IDXhosa = $0434;
        IDZulu = $0435;

        function GetWindowsLanguage: WideString;
        var
          langid, N, pc: Cardinal;
          buf: array of Char;
          langcode: WideString;
          CountryName: array [0 .. 4] of widechar;
          LanguageName: array [0 .. 4] of widechar;
          works: boolean;
        begin
          if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6)
          then
          begin
            pc := 0; // Retrieve the UI language (not the localeinfo) JR - Sept. 2023
            works := GetUserPreferredUILanguages(MUI_LANGUAGE_NAME, @N, nil,
              @pc); // available since Vista
            if works then
            begin
              SetLength(buf, pc);
              works := GetUserPreferredUILanguages(MUI_LANGUAGE_NAME, @N,
                @buf[0], @pc);
              if works then
                langcode := ReplaceStr(PChar(@buf[0]), '-', '_');
              buf := nil;
            end;
          end
          else
          begin // XP and older
            // The return value of GetLocaleInfo is compared with 3 = 2 characters and a zero
            works := 3 = GetLocaleInfoW(LOCALE_USER_DEFAULT,
              LOCALE_SISO639LANGNAME, LanguageName, SizeOf(LanguageName));
            works := works and
              (3 = GetLocaleInfoW(LOCALE_USER_DEFAULT, LOCALE_SISO3166CTRYNAME,
              CountryName, SizeOf(CountryName)));
            if works then
            begin
              // Windows 98, Me, NT4, 2000, XP and newer
              langcode := PWideChar(@(LanguageName[0]));
              if lowercase(langcode) = 'no' then
                langcode := 'nb';
              langcode := langcode + '_' + PWideChar(@CountryName[0]);
            end;
          end;
          if not works then
          begin
            // This part should only happen on Windows 95.
            langid := GetThreadLocale;
            case langid of
              IDBelgianDutch:
                langcode := 'nl_BE';
              IDBelgianFrench:
                langcode := 'fr_BE';
              IDBrazilianPortuguese:
                langcode := 'pt_BR';
              IDDanish:
                langcode := 'da_DK';
              IDDutch:
                langcode := 'nl_NL';
              IDEnglishUK:
                langcode := 'en_GB';
              IDEnglishUS:
                langcode := 'en_US';
              IDFinnish:
                langcode := 'fi_FI';
              IDFrench:
                langcode := 'fr_FR';
              IDFrenchCanadian:
                langcode := 'fr_CA';
              IDGerman:
                langcode := 'de_DE';
              IDGermanLuxembourg:
                langcode := 'de_LU';
              IDGreek:
                langcode := 'el_GR';
              IDIcelandic:
                langcode := 'is_IS';
              IDItalian:
                langcode := 'it_IT';
              IDKorean:
                langcode := 'ko_KO';
              IDNorwegianBokmol:
                langcode := 'nb_NO';
              IDNorwegianNynorsk:
                langcode := 'nn_NO';
              IDPolish:
                langcode := 'pl_PL';
              IDPortuguese:
                langcode := 'pt_PT';
              IDRussian:
                langcode := 'ru_RU';
              IDSpanish, IDSpanishModernSort:
                langcode := 'es_ES';
              IDSwedish:
                langcode := 'sv_SE';
              IDSwedishFinland:
                langcode := 'sv_FI';
            else
              langcode := 'C';
            end;
          end;
          Result := langcode;
        end;
{$ENDIF}
{$IFNDEF UNICODE}
        function LoadResStringA(ResStringRec: PResStringRec): AnsiString;
        begin
          Result := DefaultInstance.LoadResString(ResStringRec);
        end;
{$ENDIF}
        function GetTranslatorNameAndEmail: TranslatedUnicodeString;
        begin
          Result := DefaultInstance.GetTranslatorNameAndEmail;
        end;

        procedure UseLanguage(LanguageCode: LanguageString);
        begin
          DefaultInstance.UseLanguage(LanguageCode);
        end;

      type
        PStrData = ^TStrData;

        TStrData = record
          Ident: Integer;
          str: String;
        end;

        function SysUtilsEnumStringModules(Instance: HINST;
          Data: pointer): boolean;
{$IFDEF MSWINDOWS}
        var
          Buffer: array [0 .. 1023] of Char;
          // WideChar in Delphi 2008, AnsiChar before that
        begin
          with PStrData(Data)^ do
          begin
            SetString(str, Buffer, LoadString(Instance, Ident, @Buffer[0],
              SizeOf(Buffer)));
            Result := str = '';
          end;
        end;
{$ENDIF}
{$IFDEF LINUX}

      var
        rs: TResStringRec;
        Module: HModule;
      begin
        Module := Instance;
        rs.Module := @Module;
        with PStrData(Data)^ do
        begin
          rs.Identifier := Ident;
          str := System.LoadResString(@rs);
          Result := str = '';
        end;
      end;
{$ENDIF}
      function SysUtilsFindStringResource(Ident: NativeInt): string;
      var
        StrData: TStrData;
      begin
        StrData.Ident := Ident;
        StrData.str := '';
        EnumResourceModules(SysUtilsEnumStringModules, @StrData);
        Result := StrData.str;
      end;

      function SysUtilsLoadStr(Ident: Integer): string;
      begin
{$IFDEF DXGETTEXTDEBUG}
        DefaultInstance.DebugWriteln('Sysutils.LoadRes(' + IntToStr(Ident) +
          ') called');
{$ENDIF}
        Result := ResourceStringGettext(SysUtilsFindStringResource(Ident));
      end;

      function SysUtilsFmtLoadStr(Ident: Integer;
        const Args: array of const): string;
      begin
{$IFDEF DXGETTEXTDEBUG}
        DefaultInstance.DebugWriteln('Sysutils.FmtLoadRes(' + IntToStr(Ident) +
          ',Args) called');
{$ENDIF}
        FmtStr(Result, ResourceStringGettext(SysUtilsFindStringResource
          (Ident)), Args);
      end;

      function LoadResString(ResStringRec: PResStringRec): WideString;
      begin
        Result := DefaultInstance.LoadResString(ResStringRec);
      end;

      function LoadResStringW(ResStringRec: PResStringRec): UnicodeString;
      begin
        Result := DefaultInstance.LoadResString(ResStringRec);
      end;

      function GetCurrentLanguage: LanguageString;
      begin
        Result := DefaultInstance.GetCurrentLanguage;
      end;

    { TDomain }

      procedure TDomain.CloseMoFile;
      begin
        if mofile <> nil then
        begin
          FileLocator.ReleaseMoFile(mofile);
          mofile := nil;
        end;
        OpenHasFailedBefore := false;
      end;

      destructor TDomain.Destroy;
      begin
        CloseMoFile;
        inherited;
      end;

{$IFDEF mswindows}
      function GetLastWinError: WideString;
      var
        errcode: Cardinal;
      begin
        SetLength(Result, 2000);
        errcode := GetLastError();
        FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM, nil, errcode, 0,
          PWideChar(Result), 2000, nil);
        Result := PWideChar(Result);
      end;
{$ENDIF}
      procedure TDomain.OpenMoFile;
      var
        filename: FilenameString;
      begin
        // Check if it is already open
        if mofile <> nil then
          exit;

        // Check if it has been attempted to open the file before
        if OpenHasFailedBefore then
          exit;

        if SpecificFilename <> '' then
        begin
          filename := SpecificFilename;
{$IFDEF DXGETTEXTDEBUG}
          DebugLogger('Domain ' + domain + ' is bound to specific file ' +
            filename);
{$ENDIF}
        end
        else
        begin
          filename := Directory + curlang + PathDelim + 'LC_MESSAGES' +
            PathDelim + domain + '.mo';
          if (not FileLocator.FileExists(filename)) and
            (not FileExists(filename)) then
          begin
{$IFDEF DXGETTEXTDEBUG}
            DebugLogger('Domain ' + domain +
              ': File does not exist, neither embedded or in file system: ' +
              filename);
{$ENDIF}
            filename := Directory + MidStr(curlang, 1, 2) + PathDelim +
              'LC_MESSAGES' + PathDelim + domain + '.mo';
{$IFDEF DXGETTEXTDEBUG}
            DebugLogger('Domain ' + domain + ' will attempt to use this file: '
              + filename);
{$ENDIF}
          end
          else
          begin
{$IFDEF DXGETTEXTDEBUG}
            if FileLocator.FileExists(filename) then
              DebugLogger('Domain ' + domain +
                ' will attempt to use this embedded file: ' + filename)
            else
              DebugLogger('Domain ' + domain +
                ' will attempt to use this file that was found on the file system: '
                + filename);
{$ENDIF}
          end;
        end;
        if (not FileLocator.FileExists(filename)) and (not FileExists(filename))
        then
        begin
{$IFDEF DXGETTEXTDEBUG}
          DebugLogger('Domain ' + domain + ' failed to locate the file: ' +
            filename);
{$ENDIF}
          OpenHasFailedBefore := True;
          exit;
        end;
{$IFDEF DXGETTEXTDEBUG}
        DebugLogger('Domain ' + domain + ' now accesses the file.');
{$ENDIF}
        mofile := FileLocator.GetMoFile(filename, DebugLogger);

{$IFDEF DXGETTEXTDEBUG}
        if mofile.isSwappedArchitecture then
          DebugLogger
            ('.mo file is swapped (comes from another CPU architecture)');
{$ENDIF}
        // Check, that the contents of the file is utf-8
        if pos('CHARSET=UTF-8',
          uppercase(GetTranslationProperty('Content-Type'))) = 0 then
        begin
          CloseMoFile;
{$IFDEF DXGETTEXTDEBUG}
          DebugLogger('The translation for the language code ' + curlang +
            ' (in ' + filename +
            ') does not have charset=utf-8 in its Content-Type. Translations are turned off.');
{$ENDIF}
{$IFDEF MSWINDOWS}
          MessageBoxW(0,
            PWideChar(WideString('The translation for the language code ' +
            curlang + ' (in ' + filename +
            ') does not have charset=utf-8 in its Content-Type. Translations are turned off.')
            ), 'Localization problem', MB_OK);
{$ELSE}
          writeln(stderr, 'The translation for the language code ' + curlang +
            ' (in ' + filename +
            ') does not have charset=utf-8 in its Content-Type. Translations are turned off.');
{$ENDIF}
          enabled := false;
        end;
      end;

{$IFDEF UNICODE}
      function utf8decode(s: RawByteString): UnicodeString; inline;
      begin
        Result := UTF8ToWideString(s);
      end;
{$ENDIF}
      function TDomain.GetTranslationProperty(propertyname: ComponentNameString)
        : TranslatedUnicodeString;
      var
        sl: TStringList;
        i: Integer;
        s: string;
      begin
        propertyname := uppercase(propertyname) + ': ';
        sl := TStringList.Create;
        try
          sl.Text := utf8decode(gettext(''));
          for i := 0 to sl.Count - 1 do
          begin
            s := sl.Strings[i];
            if uppercase(MidStr(s, 1, length(propertyname))) = propertyname then
            begin
              Result := trim(MidStr(s, length(propertyname) + 1, maxint));

{$IFDEF DXGETTEXTDEBUG}
              DebugLogger('GetTranslationProperty(' + propertyname +
                ') returns ''' + Result + '''.');
{$ENDIF}
              exit;
            end;
          end;
        finally
          FreeAndNil(sl);
        end;
        Result := '';
{$IFDEF DXGETTEXTDEBUG}
        DebugLogger('GetTranslationProperty(' + propertyname +
          ') did not find any value. An empty string is returned.');
{$ENDIF}
      end;

      procedure TDomain.setDirectory(const dir: FilenameString);
      begin
        vDirectory := IncludeTrailingPathDelimiter(dir);
        SpecificFilename := '';
        CloseMoFile;
      end;

      procedure AddDomainForResourceString(const domain: DomainString);
      begin
{$IFDEF DXGETTEXTDEBUG}
        DefaultInstance.DebugWriteln('Extra domain for resourcestring: '
          + domain);
{$ENDIF}
        ResourceStringDomainListCS.BeginWrite;
        try
          if ResourceStringDomainList.IndexOf(domain) = -1 then
            ResourceStringDomainList.Add(domain);
        finally
          ResourceStringDomainListCS.EndWrite;
        end;
      end;

      procedure RemoveDomainForResourceString(const domain: DomainString);
      var
        i: Integer;
      begin
{$IFDEF DXGETTEXTDEBUG}
        DefaultInstance.DebugWriteln('Remove domain for resourcestring: '
          + domain);
{$ENDIF}
        ResourceStringDomainListCS.BeginWrite;
        try
          i := ResourceStringDomainList.IndexOf(domain);
          if i <> -1 then
            ResourceStringDomainList.delete(i);
        finally
          ResourceStringDomainListCS.EndWrite;
        end;
      end;

      procedure AddDomains(const Domains: array of DomainString);
      var
        i: Integer;
      begin
        for i := 0 to High(Domains) do
          AddDomainForResourceString(Domains[i]);
      end;

      procedure AddDomainForComponent(const domain: DomainString);
      begin
{$IFDEF DXGETTEXTDEBUG}
        DefaultInstance.DebugWriteln('Extra domain for component: ' + domain);
{$ENDIF}
        ComponentDomainListCS.BeginWrite;
        try
          if ComponentDomainList.IndexOf(domain) = -1 then
            ComponentDomainList.Add(domain);
        finally
          ComponentDomainListCS.EndWrite;
        end;
      end;

      procedure RemoveDomainForComponent(const domain: DomainString);
      var
        i: Integer;
      begin
{$IFDEF DXGETTEXTDEBUG}
        DefaultInstance.DebugWriteln('Remove domain for component: ' + domain);
{$ENDIF}
        ComponentDomainListCS.BeginWrite;
        try
          i := ComponentDomainList.IndexOf(domain);
          if i <> -1 then
            ComponentDomainList.delete(i);
        finally
          ComponentDomainListCS.EndWrite;
        end;
      end;

      procedure TDomain.SetLanguageCode(const langcode: LanguageString);
      begin
        CloseMoFile;
        curlang := langcode;
      end;

      function GetPluralForm2EN(Number: Integer): Integer;
      begin
        Number := abs(Number);
        if Number = 1 then
          Result := 0
        else
          Result := 1;
      end;

      function GetPluralForm1(Number: Integer): Integer;
      begin
        Result := 0;
      end;

      function GetPluralForm2FR(Number: Integer): Integer;
      begin
        Number := abs(Number);
        if (Number = 1) or (Number = 0) then
          Result := 0
        else
          Result := 1;
      end;

      function GetPluralForm3LV(Number: Integer): Integer;
      begin
        Number := abs(Number);
        if (Number mod 10 = 1) and (Number mod 100 <> 11) then
          Result := 0
        else if Number <> 0 then
          Result := 1
        else
          Result := 2;
      end;

      function GetPluralForm3GA(Number: Integer): Integer;
      begin
        Number := abs(Number);
        if Number = 1 then
          Result := 0
        else if Number = 2 then
          Result := 1
        else
          Result := 2;
      end;

      function GetPluralForm3LT(Number: Integer): Integer;
      var
        n1, n2: byte;
      begin
        Number := abs(Number);
        n1 := Number mod 10;
        n2 := Number mod 100;
        if (n1 = 1) and (n2 <> 11) then
          Result := 0
        else if (n1 >= 2) and ((n2 < 10) or (n2 >= 20)) then
          Result := 1
        else
          Result := 2;
      end;

      function GetPluralForm3PL(Number: Integer): Integer;
      var
        n1, n2: byte;
      begin
        Number := abs(Number);
        n1 := Number mod 10;
        n2 := Number mod 100;

        if Number = 1 then
          Result := 0
        else if (n1 >= 2) and (n1 <= 4) and ((n2 < 10) or (n2 >= 20)) then
          Result := 1
        else
          Result := 2;
      end;

      function GetPluralForm3RU(Number: Integer): Integer;
      var
        n1, n2: byte;
      begin
        Number := abs(Number);
        n1 := Number mod 10;
        n2 := Number mod 100;
        if (n1 = 1) and (n2 <> 11) then
          Result := 0
        else if (n1 >= 2) and (n1 <= 4) and ((n2 < 10) or (n2 >= 20)) then
          Result := 1
        else
          Result := 2;
      end;

      function GetPluralForm3SK(Number: Integer): Integer;
      begin
        Number := abs(Number);
        if Number = 1 then
          Result := 0
        else if (Number < 5) and (Number <> 0) then
          Result := 1
        else
          Result := 2;
      end;

      function GetPluralForm4SL(Number: Integer): Integer;
      var
        n2: byte;
      begin
        Number := abs(Number);
        n2 := Number mod 100;
        if n2 = 1 then
          Result := 0
        else if n2 = 2 then
          Result := 1
        else if (n2 = 3) or (n2 = 4) then
          Result := 2
        else
          Result := 3;
      end;

      procedure TDomain.GetListOfLanguages(list: TStrings);
      var
        sr: TSearchRec;
        more: boolean;
        filename, path: FilenameString;
        langcode: LanguageString;
        i, j: Integer;
      begin
        list.Clear;

        // Iterate through filesystem
        more := FindFirst(Directory + '*', faAnyFile, sr) = 0;
        try
          while more do
          begin
            if (sr.Attr and faDirectory <> 0) and (sr.name <> '.') and
              (sr.name <> '..') then
            begin
              filename := Directory + sr.name + PathDelim + 'LC_MESSAGES' +
                PathDelim + domain + '.mo';
              if FileExists(filename) then
              begin
                langcode := lowercase(sr.name);
                if list.IndexOf(langcode) = -1 then
                  list.Add(langcode);
              end;
            end;
            more := FindNext(sr) = 0;
          end;
        finally
          FindClose(sr);
        end;

        // Iterate through embedded files
        for i := 0 to FileLocator.filelist.Count - 1 do
        begin
          filename := FileLocator.basedirectory +
            FileLocator.filelist.Strings[i];
          path := Directory;
{$IFDEF MSWINDOWS}
          path := uppercase(path);
          filename := uppercase(filename);
{$ENDIF}
          j := length(path);
          if MidStr(filename, 1, j) = path then
          begin
            path := PathDelim + 'LC_MESSAGES' + PathDelim + domain + '.mo';
{$IFDEF MSWINDOWS}
            path := uppercase(path);
{$ENDIF}
            if MidStr(filename, length(filename) - length(path) + 1,
              length(path)) = path then
            begin
              langcode :=
                lowercase(MidStr(filename, j + 1,
                length(filename) - length(path) - j));
              langcode := LeftStr(langcode, 3) +
                uppercase(MidStr(langcode, 4, maxint));
              if list.IndexOf(langcode) = -1 then
                list.Add(langcode);
            end;
          end;
        end;
      end;

      procedure TDomain.SetFilename(const filename: FilenameString);
      begin
        CloseMoFile;
        vDirectory := '';
        SpecificFilename := filename;
      end;

      function TDomain.gettext(const msgid: RawUtf8String): RawUtf8String;
      var
        found: boolean;
      begin
        if not enabled then
        begin
          Result := msgid;
          exit;
        end;
        if (mofile = nil) and (not OpenHasFailedBefore) then
          OpenMoFile;
        if mofile = nil then
        begin
{$IFDEF DXGETTEXTDEBUG}
          DebugLogger('.mo file is not open. Not translating "' + msgid + '"');
{$ENDIF}
          Result := msgid;
        end
        else
        begin
          Result := mofile.gettext(msgid, found);
{$IFDEF DXGETTEXTDEBUG}
          if found then
            DebugLogger('Found in .mo (' + domain + '): "' + utf8encode(msgid) +
              '"->"' + utf8encode(Result) + '"')
          else
            DebugLogger('Translation not found in .mo file (' + domain + ') : "'
              + utf8encode(msgid) + '"');
{$ENDIF}
        end;
      end;

      constructor TDomain.Create;
      begin
        inherited Create;
        enabled := True;
      end;

    { TGnuGettextInstance }

      procedure TGnuGettextInstance.bindtextdomain(const szDomain: DomainString;
        const szDirectory: FilenameString);
      var
        dir: FilenameString;
      begin
        dir := IncludeTrailingPathDelimiter(szDirectory);
{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln('Text domain "' + szDomain + '" is now located at "' +
          dir + '"');
{$ENDIF}
        Getdomain(szDomain, DefaultDomainDirectory, curlang).Directory := dir;
        WhenNewDomainDirectory(szDomain, szDirectory);
      end;

      constructor TGnuGettextInstance.Create;
      begin
{$IFDEF MSWindows}
        DesignTimeCodePage := CP_ACP;
{$ENDIF}
{$IFDEF DXGETTEXTDEBUG}
        DebugLogCS := TMultiReadExclusiveWriteSynchronizer.Create;
        DebugLog := TMemoryStream.Create;
        DebugWriteln('Debug log started ' + DateTimeToStr(Now));
        DebugWriteln('GNU gettext module version: ' + VCSVersion);
        DebugWriteln('');
{$ENDIF}
        curGetPluralForm := GetPluralForm2EN;
        enabled := True;
        curmsgdomain := DefaultTextDomain;
        savefileCS := TMultiReadExclusiveWriteSynchronizer.Create;
        domainlist := TStringList.Create;
        TP_IgnoreList := TStringList.Create;
        TP_IgnoreList.Sorted := True;
        TP_GlobalClassHandling := TList.Create;
        TP_ClassHandling := TList.Create;
        fWhenNewLanguageListeners := TInterfaceList.Create;

        // Set some settings
        DefaultDomainDirectory := IncludeTrailingPathDelimiter
          (extractfilepath(ExecutableFilename)) + 'locale';

        UseLanguage('');

        bindtextdomain(DefaultTextDomain, DefaultDomainDirectory);
        textdomain(DefaultTextDomain);

        // Add default properties to ignore
        TP_GlobalIgnoreClassProperty(TComponent, 'Name');
        TP_GlobalIgnoreClassProperty(TCollection, 'PropName');
      end;

      destructor TGnuGettextInstance.Destroy;
      begin
        if savememory <> nil then
        begin
          savefileCS.BeginWrite;
          try
            CloseFile(savefile);
          finally
            savefileCS.EndWrite;
          end;
          FreeAndNil(savememory);
        end;
        FreeAndNil(savefileCS);
        FreeAndNil(TP_IgnoreList);
        while TP_GlobalClassHandling.Count <> 0 do
        begin
          TObject(TP_GlobalClassHandling.Items[0]).Free;
          TP_GlobalClassHandling.delete(0);
        end;
        FreeAndNil(TP_GlobalClassHandling);
        FreeTP_ClassHandlingItems;
        FreeAndNil(TP_ClassHandling);
        while domainlist.Count <> 0 do
        begin
          domainlist.Objects[0].Free;
          domainlist.delete(0);
        end;
        FreeAndNil(domainlist);
        fWhenNewLanguageListeners.Free;
{$IFDEF DXGETTEXTDEBUG}
        FreeAndNil(DebugLog);
        FreeAndNil(DebugLogCS);
{$ENDIF}
        inherited;
      end;

{$IFNDEF UNICODE}
      function TGnuGettextInstance.dgettext(const szDomain: DomainString;
        const szMsgId: AnsiString): TranslatedUnicodeString;
      begin
        Result := dgettext(szDomain, ansi2wideDTCP(szMsgId));
      end;
{$ENDIF}
      function TGnuGettextInstance.dgettext(const szDomain: DomainString;
        const szMsgId: MsgIdString): TranslatedUnicodeString;
      begin
        if not enabled then
        begin
{$IFDEF DXGETTEXTDEBUG}
          DebugWriteln
            ('Translation has been disabled. Text is not being translated: '
            + szMsgId);
{$ENDIF}
          Result := szMsgId;
        end
        else
        begin
          Result := utf8decode(EnsureLineBreakInTranslatedString
            (Getdomain(szDomain, DefaultDomainDirectory, curlang)
            .gettext(StripCRRawMsgId(utf8encode(szMsgId)))));

{$IFDEF DXGETTEXTDEBUG}
          if (szMsgId <> '') and (Result = '') then
            DebugWriteln
              (Format('Error: Translation of %s was an empty string. This may never occur.',
              [szMsgId]));
{$ENDIF}
        end;
      end;

      function TGnuGettextInstance.dgettext_NoExtract(const szDomain
        : DomainString; const szMsgId: MsgIdString): TranslatedUnicodeString;
      begin
        // This one is very useful for translating text in variables.
        // This can sometimes be necessary, and by using this function,
        // the source code scanner will not trigger warnings.
        Result := dgettext(szDomain, szMsgId);
      end;

      function TGnuGettextInstance.GetCurrentLanguage: LanguageString;
      begin
        Result := curlang;
      end;

      function TGnuGettextInstance.getcurrenttextdomain: DomainString;
      begin
        Result := curmsgdomain;
      end;

{$IFNDEF UNICODE}
      function TGnuGettextInstance.gettext(const szMsgId: AnsiString)
        : TranslatedUnicodeString;
      begin
        Result := dgettext(curmsgdomain, szMsgId);
      end;
{$ENDIF}
      function TGnuGettextInstance.gettext(const szMsgId: MsgIdString)
        : TranslatedUnicodeString;
      begin
        Result := dgettext(curmsgdomain, szMsgId);
      end;

      function TGnuGettextInstance.gettext_NoExtract(const szMsgId: MsgIdString)
        : TranslatedUnicodeString;
      begin
        // This one is very useful for translating text in variables.
        // This can sometimes be necessary, and by using this function,
        // the source code scanner will not trigger warnings.
        Result := gettext(szMsgId);
      end;

      function TGnuGettextInstance.gettext_NoOp(const szMsgId: MsgIdString)
        : TranslatedUnicodeString;
      begin
        // *** With this function Strings can be added to the po-file without beeing
        // ResourceStrings (dxgettext will add the string and this function will
        // return it without a change)
        // see gettext manual
        // 4.7 - Special Cases of Translatable Strings
        // http://www.gnu.org/software/hello/manual/gettext/Special-cases.html#Special-cases
        Result := TranslatedUnicodeString(szMsgId);
      end;

      procedure TGnuGettextInstance.textdomain(const szDomain: DomainString);
      begin
{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln('Changed text domain to "' + szDomain + '"');
{$ENDIF}
        curmsgdomain := szDomain;
        WhenNewDomain(szDomain);
      end;

      function TGnuGettextInstance.TP_CreateRetranslator: TExecutable;
      var
        ttpr: TTP_Retranslator;
      begin
        ttpr := TTP_Retranslator.Create;
        ttpr.Instance := self;
        TP_Retranslator := ttpr;
        Result := ttpr;
{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln('A retranslator was created.');
{$ENDIF}
      end;

      procedure TGnuGettextInstance.TP_GlobalHandleClass(HClass: TClass;
        Handler: TTranslator);
      var
        cm: TClassMode;
        i: Integer;
      begin
        for i := 0 to TP_GlobalClassHandling.Count - 1 do
        begin
          cm := TObject(TP_GlobalClassHandling.Items[i]) as TClassMode;
          if cm.HClass = HClass then
            raise EGGProgrammingError.Create
              ('You cannot set a handler for a class that has already been assigned otherwise.');
          if HClass.InheritsFrom(cm.HClass) then
          begin
            // This is the place to insert this class
            cm := TClassMode.Create;
            cm.HClass := HClass;
            cm.SpecialHandler := Handler;
            TP_GlobalClassHandling.insert(i, cm);
{$IFDEF DXGETTEXTDEBUG}
            DebugWriteln('A handler was set for class ' +
              HClass.ClassName + '.');
{$ENDIF}
            exit;
          end;
        end;
        cm := TClassMode.Create;
        cm.HClass := HClass;
        cm.SpecialHandler := Handler;
        TP_GlobalClassHandling.Add(cm);
{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln('A handler was set for class ' + HClass.ClassName + '.');
{$ENDIF}
      end;

      procedure TGnuGettextInstance.TP_GlobalIgnoreClass(IgnClass: TClass);
      var
        cm: TClassMode;
        i: Integer;
      begin
        for i := 0 to TP_GlobalClassHandling.Count - 1 do
        begin
          cm := TObject(TP_GlobalClassHandling.Items[i]) as TClassMode;
          if cm.HClass = IgnClass then
            raise EGGProgrammingError.Create
              ('You cannot add a class to the ignore list that is already on that list: '
              + IgnClass.ClassName +
              '. You should keep all TP_Global functions in one place in your source code.');
          if IgnClass.InheritsFrom(cm.HClass) then
          begin
            // This is the place to insert this class
            cm := TClassMode.Create;
            cm.HClass := IgnClass;
            TP_GlobalClassHandling.insert(i, cm);
{$IFDEF DXGETTEXTDEBUG}
            DebugWriteln('Globally, class ' + IgnClass.ClassName +
              ' is being ignored.');
{$ENDIF}
            exit;
          end;
        end;
        cm := TClassMode.Create;
        cm.HClass := IgnClass;
        TP_GlobalClassHandling.Add(cm);
{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln('Globally, class ' + IgnClass.ClassName +
          ' is being ignored.');
{$ENDIF}
      end;

      procedure TGnuGettextInstance.TP_GlobalIgnoreClassProperty
        (IgnClass: TClass; propertyname: ComponentNameString);
      var
        cm: TClassMode;
        i, idx: Integer;
      begin
        propertyname := uppercase(propertyname);
        for i := 0 to TP_GlobalClassHandling.Count - 1 do
        begin
          cm := TObject(TP_GlobalClassHandling.Items[i]) as TClassMode;
          if cm.HClass = IgnClass then
          begin
            if Assigned(cm.SpecialHandler) then
              raise EGGProgrammingError.Create
                ('You cannot ignore a class property for a class that has a handler set.');
            if not cm.PropertiesToIgnore.Find(propertyname, idx) then
              cm.PropertiesToIgnore.Add(propertyname);
{$IFDEF DXGETTEXTDEBUG}
            DebugWriteln('Globally, the ' + propertyname + ' property of class '
              + IgnClass.ClassName + ' is being ignored.');
{$ENDIF}
            exit;
          end;
          if IgnClass.InheritsFrom(cm.HClass) then
          begin
            // This is the place to insert this class
            cm := TClassMode.Create;
            cm.HClass := IgnClass;
            cm.PropertiesToIgnore.Add(propertyname);
            TP_GlobalClassHandling.insert(i, cm);
{$IFDEF DXGETTEXTDEBUG}
            DebugWriteln('Globally, the ' + propertyname + ' property of class '
              + IgnClass.ClassName + ' is being ignored.');
{$ENDIF}
            exit;
          end;
        end;
        cm := TClassMode.Create;
        cm.HClass := IgnClass;
        cm.PropertiesToIgnore.Add(propertyname);
        TP_GlobalClassHandling.Add(cm);
{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln('Globally, the ' + propertyname + ' property of class ' +
          IgnClass.ClassName + ' is being ignored.');
{$ENDIF}
      end;

      procedure TGnuGettextInstance.TP_Ignore(AnObject: TObject;
        const name: ComponentNameString);
      begin
        TP_IgnoreList.Add(uppercase(name));
{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln('On object with class name ' + AnObject.ClassName +
          ', ignore is set on ' + name);
{$ENDIF}
      end;

      procedure TGnuGettextInstance.TranslateComponent(AnObject: TComponent;
        const textdomain: DomainString);
      var
        comp: TGnuGettextComponentMarker;
      begin
{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln
          ('======================================================================');
        DebugWriteln
          ('TranslateComponent() was called for a component with name ' +
          AnObject.name + '.');
{$ENDIF}
        comp := AnObject.FindComponent('GNUgettextMarker')
          as TGnuGettextComponentMarker;
        if comp = nil then
        begin
          comp := TGnuGettextComponentMarker.Create(nil);
          comp.name := 'GNUgettextMarker';
          comp.Retranslator := TP_CreateRetranslator;
          TranslateProperties(AnObject, textdomain);
          AnObject.InsertComponent(comp);
{$IFDEF DXGETTEXTDEBUG}
          DebugWriteln
            ('This is the first time, that this component has been translated. A retranslator component has been created for this component.');
{$ENDIF}
        end
        else
        begin
{$IFDEF DXGETTEXTDEBUG}
          DebugWriteln
            ('This is not the first time, that this component has been translated.');
{$ENDIF}
          if comp.LastLanguage <> curlang then
          begin
{$IFDEF DXGETTEXTDEBUG}
            DebugWriteln
              ('ERROR: TranslateComponent() was called twice with different languages. This indicates an attempt to switch language at runtime, but by using TranslateComponent every time. This API has changed - please use RetranslateComponent() instead.');
{$ENDIF}
{$IFDEF mswindows}
            MessageBox(0,
              'This application tried to switch the language, but in an incorrect way. The programmer needs to replace a call to TranslateComponent with a call to RetranslateComponent(). The programmer should see the changelog of gnugettext.pas for more information.',
              'Error', MB_OK);
{$ELSE}
            writeln(stderr,
              'This application tried to switch the language, but in an incorrect way. The programmer needs to replace a call to TranslateComponent with a call to RetranslateComponent(). The programmer should see the changelog of gnugettext.pas for more information.');
{$ENDIF}
          end
          else
          begin
{$IFDEF DXGETTEXTDEBUG}
            DebugWriteln
              ('ERROR: TranslateComponent has been called twice, but with the same language chosen. This is a mistake, but in order to prevent that the application breaks, no exception is raised.');
{$ENDIF}
          end;
        end;
        comp.LastLanguage := curlang;
{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln
          ('======================================================================');
{$ENDIF}
      end;

      procedure TGnuGettextInstance.TranslateProperty(AnObject: TObject;
        PropInfo: PPropInfo; TodoList: TStrings;
        const textdomain: DomainString);
      var
        ppi: PPropInfo;
        ws: TranslatedUnicodeString;
        old: TranslatedUnicodeString;
        compmarker: TComponent;
        obj: TObject;
        Propname: ComponentNameString;
      begin
        Propname := string(PropInfo^.name);
        try
          // Translate certain types of properties
          case PropInfo^.PropType^.Kind of
{$IFDEF UNICODE}
            // All dfm files returning tkUString
            tkString, tkLString, tkWString, tkUString:
{$ELSE}
            tkString, tkLString, tkWString:
{$ENDIF}
              begin
                // prevent items in RadioGroup from duplicate translation - JR - 2011-07-29
                if AnObject.ClassName = 'TGroupButton' then
                  exit;
{$IFDEF DXGETTEXTDEBUG}
                if AnObject is TComponent then
                  DebugWriteln('Translating ' + AnObject.ClassName + '.' +
                    Propname + ' (' + TComponent(AnObject).name + ')')
                else
                  DebugWriteln('Translating ' + AnObject.ClassName + '.' +
                    Propname);;
{$ENDIF}
                case PropInfo^.PropType^.Kind of
                  tkString, tkLString:
                    old := GetStrProp(AnObject, Propname);
                  tkWString:
                    old := GetWideStrProp(AnObject, Propname);
{$IFDEF UNICODE}
                  tkUString:
                    old := GetUnicodeStrProp(AnObject, Propname);
{$ENDIF}
                else
                  raise Exception.Create
                    ('Internal error: Illegal property type. This problem needs to be solved by a programmer, try to find a workaround.');
                end;
{$IFDEF DXGETTEXTDEBUG}
                if old = '' then
                  DebugWriteln('(Empty, not translated)')
                else
                  DebugWriteln('Old value: "' + old + '"');
{$ENDIF}
                if (old <> '') and (IsWriteProp(PropInfo)) then
                begin
                  if TP_Retranslator <> nil then
                    (TP_Retranslator as TTP_Retranslator)
                      .Remember(AnObject, Propname, old);
                  if textdomain = '' then
                    ws := ComponentGettext(old)
                  else
                    ws := dgettext(textdomain, old);
                  if ws <> old then
                  begin
                    ppi := GetPropInfo(AnObject, Propname);
                    if ppi <> nil then
                    begin
                      SetWideStrProp(AnObject, ppi, ws);
                    end
                    else
                    begin
{$IFDEF DXGETTEXTDEBUG}
                      DebugWriteln('ERROR: Property disappeared: ' + Propname +
                        ' for object of type ' + AnObject.ClassName);
{$ENDIF}
                    end;
                  end;
                end;
              end { case item };
            tkClass:
              begin
                obj := GetObjectProp(AnObject, Propname);
                if obj <> nil then
                begin
                  if obj is TComponent then
                  begin
                    compmarker := TComponent(obj)
                      .FindComponent('GNUgettextMarker');
                    if Assigned(compmarker) then
                      exit;
                  end;
                  TodoList.AddObject('', obj);
                end;
              end { case item };
          end { case };
        except
          on E: Exception do
            raise EGGComponentError.Create('Property cannot be translated.' +
              sLinebreak + 'Add TP_GlobalIgnoreClassProperty(' +
              AnObject.ClassName + ',''' + Propname +
              ''') to your source code or use' + sLinebreak +
              'TP_Ignore (self,''.' + Propname + ''') to prevent this message.'
              + sLinebreak + 'Reason: ' + E.Message);
        end;
      end;

      procedure TGnuGettextInstance.TranslateProperties(AnObject: TObject;
        textdomain: DomainString = '');
      var
        TodoList: TStringList; // List of Name/TObject's that is to be processed
        DoneList: TStringList;
        // List of hex codes representing pointers to objects that have been done
        i, j, Count: Integer;
        PropList: PPropList;
        UPropName: ComponentNameString;
        PropInfo: PPropInfo;
        compmarker, comp: TComponent;
        cm, currentcm: TClassMode;
        // currentcm is nil or contains special information about how to handle the current object
        ObjectPropertyIgnoreList: TStringList;
        objid: string;
        name: ComponentNameString;
      begin
{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln
          ('----------------------------------------------------------------------');
        DebugWriteln('TranslateProperties() was called for an object of class '
          + AnObject.ClassName + ' with domain "' + textdomain + '".');
{$ENDIF}
        if TP_Retranslator <> nil then
          if textdomain = '' then
            (TP_Retranslator as TTP_Retranslator).textdomain := curmsgdomain
          else
            (TP_Retranslator as TTP_Retranslator).textdomain := textdomain;
{$IFDEF FPC}
        DoneList := TCSStringList.Create;
        TodoList := TCSStringList.Create;
        ObjectPropertyIgnoreList := TCSStringList.Create;
{$ELSE}
        DoneList := TStringList.Create;
        TodoList := TStringList.Create;
        ObjectPropertyIgnoreList := TStringList.Create;
{$ENDIF}
        try
          TodoList.AddObject('', AnObject);
          DoneList.Sorted := True;
          ObjectPropertyIgnoreList.Sorted := True;
          ObjectPropertyIgnoreList.Duplicates := dupIgnore;
          ObjectPropertyIgnoreList.CaseSensitive := false;
          DoneList.Duplicates := dupError;
          DoneList.CaseSensitive := True;

          while TodoList.Count <> 0 do
          begin
            AnObject := TodoList.Objects[0];
            Name := TodoList.Strings[0];
            TodoList.delete(0);
            if (AnObject <> nil) and (AnObject is TPersistent) then
            begin
              // Make sure each object is only translated once
              Assert(SizeOf({$IFDEF CPUx64}NativeInt{$ELSE}Integer{$ENDIF CPUx64}) = SizeOf(TObject));
              objid := IntToHex
                ({$IFDEF CPUx64}NativeInt{$ELSE}Integer{$ENDIF CPUx64}(AnObject), 8);
              if DoneList.Find(objid, i) then
              begin
                continue;
              end
              else
              begin
                DoneList.Add(objid);
              end;

              ObjectPropertyIgnoreList.Clear;

              // Find out if there is special handling of this object
              currentcm := nil;
              // First check the local handling instructions
              for j := 0 to TP_ClassHandling.Count - 1 do
              begin
                cm := TObject(TP_ClassHandling.Items[j]) as TClassMode;
                if AnObject.InheritsFrom(cm.HClass) then
                begin
                  if cm.PropertiesToIgnore.Count <> 0 then
                  begin
                    ObjectPropertyIgnoreList.AddStrings(cm.PropertiesToIgnore);
                  end
                  else
                  begin
                    // Ignore the entire class
                    currentcm := cm;
                    break;
                  end;
                end;
              end;
              // Then check the global handling instructions
              if currentcm = nil then
                for j := 0 to TP_GlobalClassHandling.Count - 1 do
                begin
                  cm := TObject(TP_GlobalClassHandling.Items[j]) as TClassMode;
                  if AnObject.InheritsFrom(cm.HClass) then
                  begin
                    if cm.PropertiesToIgnore.Count <> 0 then
                    begin
                      ObjectPropertyIgnoreList.AddStrings
                        (cm.PropertiesToIgnore);
                    end
                    else
                    begin
                      // Ignore the entire class
                      currentcm := cm;
                      break;
                    end;
                  end;
                end;
              if currentcm <> nil then
              begin
                ObjectPropertyIgnoreList.Clear;
                // Ignore or use special handler
                if Assigned(currentcm.SpecialHandler) then
                begin
                  currentcm.SpecialHandler(AnObject);
{$IFDEF DXGETTEXTDEBUG}
                  DebugWriteln('Special handler activated for ' +
                    AnObject.ClassName);
{$ENDIF}
                end
                else
                begin
{$IFDEF DXGETTEXTDEBUG}
                  DebugWriteln('Ignoring object ' + AnObject.ClassName);
{$ENDIF}
                end;
                continue;
              end;

              Count := GetPropList(AnObject, PropList);
              try
                for j := 0 to Count - 1 do
                begin
                  PropInfo := PropList[j];
{$IFDEF UNICODE}
                  if not(PropInfo^.PropType^.Kind in [tkString, tkLString,
                    tkWString, tkClass, tkUString]) then
{$ELSE}
                  if not(PropInfo^.PropType^.Kind in [tkString, tkLString,
                    tkWString, tkClass]) then
{$ENDIF}
                    continue;
                  UPropName := uppercase(string(PropInfo^.name));
                  // Ignore properties that are meant to be ignored
                  if ((currentcm = nil) or
                    (not currentcm.PropertiesToIgnore.Find(UPropName, i))) and
                    (not TP_IgnoreList.Find(Name + '.' + UPropName, i)) and
                    (not ObjectPropertyIgnoreList.Find(UPropName, i)) then
                  begin
                    TranslateProperty(AnObject, PropInfo, TodoList, textdomain);
                  end; // if
                end; // for
              finally
                if Count <> 0 then
                  FreeMem(PropList);
              end;
              if AnObject is TStrings then
              begin
                if ((AnObject as TStrings).Text <> '') and
                  (TP_Retranslator <> nil) then
                  (TP_Retranslator as TTP_Retranslator)
                    .Remember(AnObject, 'Text', (AnObject as TStrings).Text);
                TranslateStrings(AnObject as TStrings, textdomain);
              end;
              // Check for TCollection
              if AnObject is TCollection then
              begin
                for i := 0 to (AnObject as TCollection).Count - 1 do
                begin
                  // Only add the object if it's not totally ignored already
                  if not Assigned(currentcm) or
                    not AnObject.InheritsFrom(currentcm.HClass) then
                    TodoList.AddObject('', (AnObject as TCollection).Items[i]);
                end;
              end;
              if AnObject is TComponent then
              begin
                for i := 0 to TComponent(AnObject).ComponentCount - 1 do
                begin
                  comp := TComponent(AnObject).Components[i];
                  if (not TP_IgnoreList.Find(uppercase(comp.name), j)) then
                  begin
                    // Only add the object if it's not totally ignored or translated already
                    if not Assigned(currentcm) or
                      not AnObject.InheritsFrom(currentcm.HClass) then
                    begin
                      compmarker := comp.FindComponent('GNUgettextMarker');
                      if not Assigned(compmarker) then
                        TodoList.AddObject(uppercase(comp.name), comp);
                    end;
                  end;
                end;
              end;
            end { if AnObject<>nil };
          end { while todolist.count<>0 };
        finally
          FreeAndNil(TodoList);
          FreeAndNil(ObjectPropertyIgnoreList);
          FreeAndNil(DoneList);
        end;
        FreeTP_ClassHandlingItems;
        TP_IgnoreList.Clear;
        TP_Retranslator := nil;
{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln
          ('----------------------------------------------------------------------');
{$ENDIF}
      end;

      procedure TGnuGettextInstance.UnregisterWhenNewLanguageListener
        (Listener: IGnuGettextInstanceWhenNewLanguageListener);
      begin
        fWhenNewLanguageListeners.Remove(Listener);
      end;

      procedure TGnuGettextInstance.UseLanguage(LanguageCode: LanguageString);
      var
        i, p: Integer;
        dom: TDomain;
        l2: string;
      begin
{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln('UseLanguage(''' + LanguageCode + '''); called');
{$ENDIF}
        if LanguageCode = '' then
        begin
          LanguageCode := GGGetEnvironmentVariable('LANG');
{$IFDEF DXGETTEXTDEBUG}
          DebugWriteln('LANG env variable is ''' + LanguageCode + '''.');
{$ENDIF}
{$IFDEF MSWINDOWS}
          if LanguageCode = '' then
          begin
            LanguageCode := GetWindowsLanguage;
{$IFDEF DXGETTEXTDEBUG}
            DebugWriteln('Found Windows language code to be ''' +
              LanguageCode + '''.');
{$ENDIF}
          end;
{$ENDIF}
          p := pos('.', LanguageCode);
          if p <> 0 then
            LanguageCode := LeftStr(LanguageCode, p - 1);
{$IFDEF DXGETTEXTDEBUG}
          DebugWriteln('Language code that will be set is ''' +
            LanguageCode + '''.');
{$ENDIF}
        end;

        curlang := LanguageCode;
        for i := 0 to domainlist.Count - 1 do
        begin
          dom := domainlist.Objects[i] as TDomain;
          dom.SetLanguageCode(curlang);
        end;

        l2 := lowercase(LeftStr(curlang, 2));
        if (l2 = 'en') or (l2 = 'de') then
          curGetPluralForm := GetPluralForm2EN
        else if (l2 = 'hu') or (l2 = 'ko') or (l2 = 'zh') or (l2 = 'ja') or
          (l2 = 'tr') then
          curGetPluralForm := GetPluralForm1
        else if (l2 = 'fr') or (l2 = 'fa') or (lowercase(curlang) = 'pt_br')
        then
          curGetPluralForm := GetPluralForm2FR
        else if (l2 = 'lv') then
          curGetPluralForm := GetPluralForm3LV
        else if (l2 = 'ga') then
          curGetPluralForm := GetPluralForm3GA
        else if (l2 = 'lt') then
          curGetPluralForm := GetPluralForm3LT
        else if (l2 = 'ru') or (l2 = 'uk') or (l2 = 'hr') then
          curGetPluralForm := GetPluralForm3RU
        else if (l2 = 'cs') or (l2 = 'sk') then
          curGetPluralForm := GetPluralForm3SK
        else if (l2 = 'pl') then
          curGetPluralForm := GetPluralForm3PL
        else if (l2 = 'sl') then
          curGetPluralForm := GetPluralForm4SL
        else
        begin
          curGetPluralForm := GetPluralForm2EN;
{$IFDEF DXGETTEXTDEBUG}
          DebugWriteln
            ('Plural form for the language was not found. English plurality system assumed.');
{$ENDIF}
        end;

        WhenNewLanguage(curlang);

{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln('');
{$ENDIF}
      end;

      procedure TGnuGettextInstance.TranslateStrings(sl: TStrings;
        const textdomain: DomainString);
      var
        Line: string;
        i: Integer;
        s: TStringList;
        slAsTStringList: TStringList;
        originalOwnsObjects: boolean;
      begin
        if sl.Count > 0 then
        begin
          // From D2009 onward, the TStringList class has a OwnsObjects property, just like
          // TObjectList has. This means that when we will be calling Clear on the given
          // list in the sl parameter, we could destroy the objects it contains.
          // To avoid this we must disable OwnsObjects while we replace the strings, but
          // only if sl is a TStringList instance and if using Delphi 2009 or upper.
          originalOwnsObjects := false; // avoid warning
          if sl is TStringList then
            slAsTStringList := TStringList(sl)
          else
            slAsTStringList := nil;

          sl.BeginUpdate;
          try
            s := TStringList.Create;
            try
              // don't use Assign here as it will propagate the Sorted property (among others)
              // in versions of Delphi from Delphi XE ownard
              s.AddStrings(sl);

              for i := 0 to s.Count - 1 do
              begin
                Line := s.Strings[i];
                if Line <> '' then
                  if textdomain = '' then
                    s.Strings[i] := ComponentGettext(Line)
                  else
                    s.Strings[i] := dgettext(textdomain, Line);
              end;

              if Assigned(slAsTStringList) then
              begin
                originalOwnsObjects := slAsTStringList.OwnsObjects;
                slAsTStringList.OwnsObjects := false;
              end;
              try
                // same here, we don't want to modify the properties of the orignal string list
                sl.Clear;
                sl.AddStrings(s);
              finally
                if Assigned(slAsTStringList) then
                  slAsTStringList.OwnsObjects := originalOwnsObjects;
              end;
            finally
              FreeAndNil(s);
            end;
          finally
            sl.EndUpdate;
          end;
        end;
      end;

      function TGnuGettextInstance.GetTranslatorNameAndEmail
        : TranslatedUnicodeString;
      begin
        Result := GetTranslationProperty('LAST-TRANSLATOR');
      end;

      function TGnuGettextInstance.GetTranslationProperty(const propertyname
        : ComponentNameString): TranslatedUnicodeString;
      begin
        Result := Getdomain(curmsgdomain, DefaultDomainDirectory, curlang)
          .GetTranslationProperty(propertyname);
      end;

      function TGnuGettextInstance.dngettext(const szDomain: DomainString;
        const singular, plural: MsgIdString; Number: Integer)
        : TranslatedUnicodeString;
      var
        org: MsgIdString;
        trans: TranslatedUnicodeString;
        idx: Integer;
        p: Integer;
      begin
{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln('dngettext translation (domain ' + szDomain +
          ', number is ' + IntToStr(Number) + ') of ' + singular + '/'
          + plural);
{$ENDIF}
        org := singular + #0 + plural;
        trans := dgettext(szDomain, org);
        if org = trans then
        begin
{$IFDEF DXGETTEXTDEBUG}
          DebugWriteln
            ('Translation was equal to english version. English plural forms assumed.');
{$ENDIF}
          idx := GetPluralForm2EN(Number)
        end
        else
          idx := curGetPluralForm(Number);
{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln('Index ' + IntToStr(idx) + ' will be used');
{$ENDIF}
        while True do
        begin
          p := pos(#0, trans);
          if p = 0 then
          begin
{$IFDEF DXGETTEXTDEBUG}
            DebugWriteln('Last translation used: ' + utf8encode(trans));
{$ENDIF}
            Result := trans;
            exit;
          end;
          if idx = 0 then
          begin
{$IFDEF DXGETTEXTDEBUG}
            DebugWriteln('Translation found: ' + utf8encode(trans));
{$ENDIF}
            Result := LeftStr(trans, p - 1);
            exit;
          end;
          delete(trans, 1, p);
          dec(idx);
        end;
      end;

      function TGnuGettextInstance.dngettext_NoExtract(const szDomain
        : DomainString; const singular, plural: MsgIdString; Number: Integer)
        : TranslatedUnicodeString;
      begin
        // This one is very useful for translating text in variables.
        // This can sometimes be necessary, and by using this function,
        // the source code scanner will not trigger warnings.
        Result := dngettext(szDomain, singular, plural, Number);
      end;

{$IFNDEF UNICODE}
      function TGnuGettextInstance.ngettext(const singular, plural: AnsiString;
        Number: Integer): TranslatedUnicodeString;
      begin
        Result := dngettext(curmsgdomain, singular, plural, Number);
      end;
{$ENDIF}
      function TGnuGettextInstance.ngettext(const singular, plural: MsgIdString;
        Number: Integer): TranslatedUnicodeString;
      begin
        Result := dngettext(curmsgdomain, singular, plural, Number);
      end;

      function TGnuGettextInstance.ngettext_NoExtract(const singular,
        plural: MsgIdString; Number: Integer): TranslatedUnicodeString;
      begin
        // This one is very useful for translating text in variables.
        // This can sometimes be necessary, and by using this function,
        // the source code scanner will not trigger warnings.
        Result := ngettext(singular, plural, Number);
      end;

      procedure TGnuGettextInstance.WhenNewDomain(const textdomain
        : DomainString);
      begin
        // This is meant to be empty.
      end;

      procedure TGnuGettextInstance.WhenNewLanguage(const LanguageID
        : LanguageString);
      var
        i: Integer;
      begin
        for i := 0 to fWhenNewLanguageListeners.Count - 1 do
          IGnuGettextInstanceWhenNewLanguageListener
            (fWhenNewLanguageListeners[i]).WhenNewLanguage(LanguageID);
      end;

      procedure TGnuGettextInstance.WhenNewDomainDirectory(const textdomain
        : DomainString; const Directory: FilenameString);
      begin
        // This is meant to be empty.
      end;

      procedure TGnuGettextInstance.GetListOfLanguages
        (const domain: DomainString; list: TStrings);
      begin
        Getdomain(domain, DefaultDomainDirectory, curlang)
          .GetListOfLanguages(list);
      end;

      procedure TGnuGettextInstance.bindtextdomainToFile(const szDomain
        : DomainString; const filename: FilenameString);
      begin
{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln('Text domain "' + szDomain +
          '" is now bound to file named "' + filename + '"');
{$ENDIF}
        Getdomain(szDomain, DefaultDomainDirectory, curlang)
          .SetFilename(filename);
      end;

      procedure TGnuGettextInstance.DebugLogPause(PauseEnabled: boolean);
      begin
{$IFDEF DXGETTEXTDEBUG}
        DebugLogOutputPaused := PauseEnabled;
{$ENDIF}
      end;

      procedure TGnuGettextInstance.DebugLogToFile(const filename
        : FilenameString; append: boolean = false);
{$IFDEF DXGETTEXTDEBUG}
      var
        fs: TFileStream;
        marker: AnsiString;
{$ENDIF}
      begin
{$IFDEF DXGETTEXTDEBUG}
        // Create the file if needed
        if (not FileExists(filename)) or (not append) then
          fileclose(filecreate(filename));

        // Open file
        fs := TFileStream.Create(filename, fmOpenWrite or fmShareDenyWrite);
        if append then
          fs.Seek(0, soFromEnd);

        // Write header if appending
        if fs.Position <> 0 then
        begin
          marker := sLinebreak +
            '==========================================================================='
            + sLinebreak;
          fs.WriteBuffer(marker[1], length(marker));
        end;

        // Copy the memorystream contents to the file
        if DebugLog <> nil then
        begin
          DebugLog.Seek(0, soFromBeginning);
          fs.CopyFrom(DebugLog, 0);
        end;

        // Make DebugLog point to the filestream
        FreeAndNil(DebugLog);
        DebugLog := fs;
{$ENDIF}
      end;

{$IFDEF DXGETTEXTDEBUG}
      procedure TGnuGettextInstance.DebugWriteln(Line: AnsiString);
      Var
        Discard: boolean;
      begin
        Assert(DebugLogCS <> nil);
        Assert(DebugLog <> nil);

        DebugLogCS.BeginWrite;
        try
          if DebugLogOutputPaused then
            exit;

          if Assigned(fOnDebugLine) then
          begin
            Discard := True;
            fOnDebugLine(self, Line, Discard);
            If Discard then
              exit;
          end;

          Line := Line + sLinebreak;

          // Ensure that memory usage doesn't get too big.
          if (DebugLog is TMemoryStream) and (DebugLog.Position > 1000000) then
          begin
            Line := sLinebreak + sLinebreak + sLinebreak + sLinebreak +
              sLinebreak +
              'Debug log halted because memory usage grew too much.' +
              sLinebreak +
              'Specify a filename to store the debug log in or disable debug loggin in gnugettext.pas.'
              + sLinebreak + sLinebreak + sLinebreak + sLinebreak + sLinebreak;
            DebugLogOutputPaused := True;
          end;
          DebugLog.WriteBuffer(Line[1], length(Line));
        finally
          DebugLogCS.EndWrite;
        end;
      end;
{$ENDIF}
      function TGnuGettextInstance.Getdomain(const domain: DomainString;
        const DefaultDomainDirectory: FilenameString;
        const curlang: LanguageString): TDomain;
      // Retrieves the TDomain object for the specified domain.
      // Creates one, if none there, yet.
      var
        idx: Integer;
      begin
        idx := domainlist.IndexOf(domain);
        if idx = -1 then
        begin
          Result := TDomain.Create;
{$IFDEF DXGETTEXTDEBUG}
          Result.DebugLogger := DebugWriteln;
{$ENDIF}
          Result.domain := domain;
          Result.Directory := DefaultDomainDirectory;
          Result.SetLanguageCode(curlang);
          domainlist.AddObject(domain, Result);
        end
        else
        begin
          Result := domainlist.Objects[idx] as TDomain;
        end;
      end;

      function TGnuGettextInstance.LoadResString(ResStringRec: PResStringRec)
        : UnicodeString;
{$IFDEF MSWINDOWS}
      var
        Len: Integer;
{$IFDEF UNICODE}
        Buffer: array [0 .. 1023] of widechar;
{$ELSE}
        Buffer: array [0 .. 1023] of ansichar;
{$ENDIF}
{$ENDIF}
{$IFDEF LINUX }
      const
        ResStringTableLen = 16;
      type
        ResStringTable = array [0 .. ResStringTableLen - 1] of LongWord;
      var
        Handle: TResourceHandle;
        Tab: ^ResStringTable;
        ResMod: HModule;
{$ENDIF }
      begin
        if ResStringRec = nil then
          exit;
        if ResStringRec.Identifier >= 64 * 1024 then
        begin
{$IFDEF DXGETTEXTDEBUG}
          DebugWriteln
            ('LoadResString was given an invalid ResStringRec.Identifier');
{$ENDIF}
          Result := 'ERROR';
          exit;
        end
        else
        begin
{$IFDEF LINUX}
          // This works with Unicode if the Linux has utf-8 character set
          // Result:=System.LoadResString(ResStringRec);
          ResMod := FindResourceHInstance(ResStringRec^.Module^);
          Handle := FindResource(ResMod,
            PAnsiChar(ResStringRec^.Identifier div ResStringTableLen),
            PAnsiChar(6)); // RT_STRING
          Tab := pointer(LoadResource(ResMod, Handle));
          if Tab = nil then
            Result := ''
          else
            Result := PWideChar(PAnsiChar(Tab) +
              Tab[ResStringRec^.Identifier mod ResStringTableLen]);
{$ENDIF}
{$IFDEF MSWINDOWS}
          if not Win32PlatformIsUnicode then
          begin
            SetString(Result, Buffer,
              LoadString(FindResourceHInstance(ResStringRec.Module^),
              ResStringRec.Identifier, Buffer, SizeOf(Buffer)))
          end
          else
          begin
            Result := '';
            Len := 0;
            While length(Result) <= Len + 1 do
            begin
              if length(Result) = 0 then
                SetLength(Result, 1024)
              else
                SetLength(Result, length(Result) * 2);
              Len := LoadStringW(FindResourceHInstance(ResStringRec.Module^),
                ResStringRec.Identifier, PWideChar(Result), length(Result));
            end;
            SetLength(Result, Len);
          end;
{$ENDIF}
        end;
{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln('Loaded resourcestring: ' + utf8encode(Result));
{$ENDIF}
        Result := ResourceStringGettext(Result);
      end;

      procedure TGnuGettextInstance.RegisterWhenNewLanguageListener
        (Listener: IGnuGettextInstanceWhenNewLanguageListener);
      begin
        fWhenNewLanguageListeners.Add(Listener);
      end;

      procedure TGnuGettextInstance.RetranslateComponent(AnObject: TComponent;
        const textdomain: DomainString);
      var
        comp: TGnuGettextComponentMarker;
      begin
{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln
          ('======================================================================');
        DebugWriteln
          ('RetranslateComponent() was called for a component with name ' +
          AnObject.name + '.');
{$ENDIF}
        comp := AnObject.FindComponent('GNUgettextMarker')
          as TGnuGettextComponentMarker;
        if comp = nil then
        begin
{$IFDEF DXGETTEXTDEBUG}
          DebugWriteln
            ('Retranslate was called on an object that has not been translated before. An Exception is being raised.');
{$ENDIF}
          raise EGGProgrammingError.Create
            ('Retranslate was called on an object that has not been translated before. Please use TranslateComponent() before RetranslateComponent().');
        end
        else
        begin
          // *** if param ReReadMoFileOnSameLanguage is set, use the ReTranslate
          // function nevertheless if the current language is the same like the
          // new (-> reread the current .mo-file from the file system).
          if ReReadMoFileOnSameLanguage or (comp.LastLanguage <> curlang) then
          begin
{$IFDEF DXGETTEXTDEBUG}
            DebugWriteln('The retranslator is being executed.');
{$ENDIF}
            comp.Retranslator.Execute;
          end
          else
          begin
{$IFDEF DXGETTEXTDEBUG}
            DebugWriteln
              ('The language has not changed. The retranslator is not executed.');
{$ENDIF}
          end;
        end;
        comp.LastLanguage := curlang;

{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln
          ('======================================================================');
{$ENDIF}
      end;

      procedure TGnuGettextInstance.TP_IgnoreClass(IgnClass: TClass);
      var
        cm: TClassMode;
        i: Integer;
      begin
        for i := 0 to TP_ClassHandling.Count - 1 do
        begin
          cm := TObject(TP_ClassHandling.Items[i]) as TClassMode;
          if cm.HClass = IgnClass then
            raise EGGProgrammingError.Create
              ('You cannot add a class to the ignore list that is already on that list: '
              + IgnClass.ClassName + '.');
          if IgnClass.InheritsFrom(cm.HClass) then
          begin
            // This is the place to insert this class
            cm := TClassMode.Create;
            cm.HClass := IgnClass;
            TP_ClassHandling.insert(i, cm);
{$IFDEF DXGETTEXTDEBUG}
            DebugWriteln('Locally, class ' + IgnClass.ClassName +
              ' is being ignored.');
{$ENDIF}
            exit;
          end;
        end;
        cm := TClassMode.Create;
        cm.HClass := IgnClass;
        TP_ClassHandling.Add(cm);
{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln('Locally, class ' + IgnClass.ClassName +
          ' is being ignored.');
{$ENDIF}
      end;

      procedure TGnuGettextInstance.TP_IgnoreClassProperty(IgnClass: TClass;
        propertyname: ComponentNameString);
      var
        cm: TClassMode;
        i: Integer;
      begin
        propertyname := uppercase(propertyname);
        for i := 0 to TP_ClassHandling.Count - 1 do
        begin
          cm := TObject(TP_ClassHandling.Items[i]) as TClassMode;
          if cm.HClass = IgnClass then
          begin
            if Assigned(cm.SpecialHandler) then
              raise EGGProgrammingError.Create
                ('You cannot ignore a class property for a class that has a handler set.');
            cm.PropertiesToIgnore.Add(propertyname);
{$IFDEF DXGETTEXTDEBUG}
            DebugWriteln('Globally, the ' + propertyname + ' property of class '
              + IgnClass.ClassName + ' is being ignored.');
{$ENDIF}
            exit;
          end;
          if IgnClass.InheritsFrom(cm.HClass) then
          begin
            // This is the place to insert this class
            cm := TClassMode.Create;
            cm.HClass := IgnClass;
            cm.PropertiesToIgnore.Add(propertyname);
            TP_ClassHandling.insert(i, cm);
{$IFDEF DXGETTEXTDEBUG}
            DebugWriteln('Locally, the ' + propertyname + ' property of class '
              + IgnClass.ClassName + ' is being ignored.');
{$ENDIF}
            exit;
          end;
        end;
        cm := TClassMode.Create;
        cm.HClass := IgnClass;
        cm.PropertiesToIgnore.Add(propertyname);
        TP_GlobalClassHandling.Add(cm);
{$IFDEF DXGETTEXTDEBUG}
        DebugWriteln('Locally, the ' + propertyname + ' property of class ' +
          IgnClass.ClassName + ' is being ignored.');
{$ENDIF}
      end;

      procedure TGnuGettextInstance.FreeTP_ClassHandlingItems;
      begin
        while TP_ClassHandling.Count <> 0 do
        begin
          TObject(TP_ClassHandling.Items[0]).Free;
          TP_ClassHandling.delete(0);
        end;
      end;

{$IFNDEF UNICODE}
      function TGnuGettextInstance.ansi2wideDTCP(const s: AnsiString)
        : MsgIdString;
{$IFDEF MSWindows}
      var
        Len: Integer;
{$ENDIF}
      begin
{$IFDEF MSWindows}
        if DesignTimeCodePage = CP_ACP then
        begin
          // No design-time codepage specified. Using runtime codepage instead.
{$ENDIF}
          Result := s;
{$IFDEF MSWindows}
        end
        else
        begin
          Len := length(s);
          if Len = 0 then
            Result := ''
          else
          begin
            SetLength(Result, Len);
            Len := MultiByteToWideChar(DesignTimeCodePage, 0, PAnsiChar(s), Len,
              PWideChar(Result), Len);
            if Len = 0 then
              raise EGGAnsi2WideConvError.Create
                ('Cannot convert string to widestring:' + sLinebreak + s);
            SetLength(Result, Len);
          end;
        end;
{$ENDIF}
      end;
{$ENDIF}
{$IFNDEF UNICODE}
      function TGnuGettextInstance.dngettext(const szDomain: DomainString;
        const singular, plural: AnsiString; Number: Integer)
        : TranslatedUnicodeString;
      begin
        Result := dngettext(szDomain, ansi2wideDTCP(singular),
          ansi2wideDTCP(plural), Number);
      end;
{$ENDIF}
    { TClassMode }

      constructor TClassMode.Create;
      begin
        PropertiesToIgnore := TStringList.Create;
        PropertiesToIgnore.Sorted := True;
        PropertiesToIgnore.Duplicates := dupError;
        PropertiesToIgnore.CaseSensitive := false;
      end;

      destructor TClassMode.Destroy;
      begin
        FreeAndNil(PropertiesToIgnore);
        inherited;
      end;

    { TFileLocator }

      function TFileLocator.FindSignaturePos(const signature: RawByteString;
        str: TFileStream): int64;
      // Finds the position of signature in the file.
      const
        bufsize = 100000;
      var
        a: RawByteString;
        b: RawByteString;
        Offset: Integer;
        rd, p: Integer;
      begin
        if signature = '' then
        begin
          Result := 0;
          exit;
        end;

        Offset := 0;
        str.Seek(0, soFromBeginning);

        SetLength(a, bufsize);
        SetLength(b, bufsize);
        str.Read(a[1], bufsize);

        while True do
        begin
          rd := str.Read(b[1], bufsize);
          p := pos(signature, a + b);
          if (p <> 0) then
          begin // do not check p < bufsize+100 here!
            Result := Offset + p - 1;
            exit;
          end;
          if rd <> bufsize then
          begin
            // Prematurely ended without finding anything
            Result := 0;
            exit;
          end;
          a := b;
          Offset := Offset + bufsize;
        end;
        Result := 0;
      end;

    // Changed to "Analyze" with old signature to be compatible to older programs
    // and to the assemble routine in GgtTranslate
    // JR - 2012-09-10
      procedure TFileLocator.Analyze;
      var
        s: AnsiString;
        i: Integer;
        Offset: int64;
        fs: TFileStream;
        fi: TEmbeddedFileInfo;
        Filename: TFileName;
        // change from FilenameString to AnsiString, JR - 2010-04-10
      begin
        // "ggassemble" will search for this GUI and replace the trailing #0s by a pointer
        // to the begin of the embedded mo files
        s := '6637DB2E-62E1-4A60-AC19-C23867046A89'#0#0#0#0#0#0#0#0; // constant
        s := MidStr(s, length(s) - 7, 8);
        Offset := 0;
        for i := 8 downto 1 do
          Offset := Offset shl 8 + ord(s[i]);
        if Offset = 0 then
          exit;
        basedirectory := extractfilepath(ExecutableFilename);
        try
          fs := TFileStream.Create(ExecutableFilename, fmOpenRead or
            fmShareDenyNone);
          try
            while True do
            begin
              fs.Seek(Offset, soFromBeginning);
              Offset := ReadInt64(fs);
              if Offset = 0 then
                exit;
              fi := TEmbeddedFileInfo.Create;
              try
                fi.Offset := ReadInt64(fs);
                fi.Size := ReadInt64(fs);
                SetLength(filename, Offset - fs.Position);
                fs.ReadBuffer(filename[1], Offset - fs.Position);
                filename := trim(filename);
                if PreferExternal and System.SysUtils.FileExists
                  (basedirectory + filename) then
                begin
                  // Disregard the internal version and use the external version instead
                  FreeAndNil(fi);
                end
                else
                  filelist.AddObject(filename, fi);
              except
                FreeAndNil(fi);
                raise;
              end;
            end;
          finally
            FreeAndNil(fs);
          end;
        except
{$IFDEF DXGETTEXTDEBUG}
          raise;
{$ENDIF}
        end;
      end;

    (* procedure TFileLocator.Analyze;
      var
      HeaderSize,
      PrefixSize: Integer;
      dummysig,
      headerpre,
      headerbeg,
      headerend:RawByteString;
      i:integer;
      headerbeginpos,
      headerendpos:integer;
      offset,
      tableoffset:int64;
      fs:TFileStream;
      fi:TEmbeddedFileInfo;
      filename:FilenameString;
      filename8bit:RawByteString;
      const
      // DetectionSignature: used solely to detect gnugettext usage by assemble
      DetectionSignature: array[0..35] of AnsiChar='2E23E563-31FA-4C24-B7B3-90BE720C6B1A';
      // Embedded Header Begin Signature (without dynamic prefix written by assemble)
      BeginHeaderSignature: array[0..35] of AnsiChar='BD7F1BE4-9FCF-4E3A-ABA7-3443D11AB362';
      // Embedded Header End Signature (without dynamic prefix written by assemble)
      EndHeaderSignature: array[0..35] of AnsiChar='1C58841C-D8A0-4457-BF54-D8315D4CF49D';
      // Assemble Prefix (do not put before the Header Signatures!)
      SignaturePrefix: array[0..2] of AnsiChar='DXG'; // written from assemble
      begin
      // Attn: Ensure all Signatures have the same size!
      HeaderSize := High(BeginHeaderSignature) - Low(BeginHeaderSignature) + 1;
      PrefixSize := High(SignaturePrefix) - Low(SignaturePrefix) + 1;

      // dummy usage of DetectionSignature (otherwise not compiled into exe)
      SetLength(dummysig, HeaderSize);
      for i := 0 to HeaderSize-1 do
      dummysig[i+1] := DetectionSignature[i];

      // copy byte by byte (D2009+ compatible)
      SetLength(headerpre, PrefixSize);
      for i:= 0 to PrefixSize-1 do
      headerpre[i+1] := SignaturePrefix[i];

      SetLength(headerbeg, HeaderSize);
      for i:= 0 to HeaderSize-1 do
      headerbeg[i+1] := BeginHeaderSignature[i];

      SetLength(headerend, HeaderSize);
      for i:= 0 to HeaderSize-1 do
      headerend[i+1] := EndHeaderSignature[i];

      BaseDirectory:=ExtractFilePath(ExecutableFilename);
      try
      fs:=TFileStream.Create(ExecutableFilename,fmOpenRead or fmShareDenyNone);
      try
      // try to find new header begin and end signatures
      headerbeginpos := FindSignaturePos(headerpre+headerbeg, fs);
      headerendpos := FindSignaturePos(headerpre+headerend, fs);

      if (headerbeginpos > 0) and (headerendpos > 0) then
      begin
      // adjust positions (to the end of each signature)
      headerbeginpos := headerbeginpos + HeaderSize + PrefixSize;

      // get file table offset (8 byte, stored directly before the end header)
      fs.Seek(headerendpos - 8, soFromBeginning);
      // get relative offset and convert to absolute offset during runtime
      tableoffset := headerbeginpos + ReadInt64(fs);

      // go to beginning of embedded block
      fs.Seek(headerbeginpos, soFromBeginning);

      offset := tableoffset;
      Assert(sizeof(offset)=8);
      while (true) and (fs.Position<headerendpos) do begin
      fs.Seek(offset,soFromBeginning);
      offset:=ReadInt64(fs);
      if offset=0 then
      exit;
      offset:=headerbeginpos+offset;
      fi:=TEmbeddedFileInfo.Create;
      try
      // get embedded file info (adjusting dynamic to real offsets now)
      fi.Offset:=headerbeginpos+ReadInt64(fs);
      fi.Size:=ReadInt64(fs);
      SetLength (filename8bit, offset-fs.position);
      fs.ReadBuffer (filename8bit[1], offset-fs.position);
      filename:=trim(utf8decode(filename8bit));
      if PreferExternal and sysutils.fileexists(basedirectory+filename) then begin
      // Disregard the internal version and use the external version instead
      FreeAndNil (fi);
      end else
      filelist.AddObject(filename,fi);
      except
      FreeAndNil (fi);
      raise;
      end;
      end;
      end;
      finally
      FreeAndNil (fs);
      end;
      except
      {$ifdef DXGETTEXTDEBUG}
      raise;
      {$endif}
      end;
      end; *)

      constructor TFileLocator.Create;
      begin
        MoFilesCS := TMultiReadExclusiveWriteSynchronizer.Create;
        MoFiles := TStringList.Create;
        filelist := TStringList.Create;
{$IFDEF LINUX}
        filelist.Duplicates := dupError;
        filelist.CaseSensitive := True;
{$ENDIF}
        MoFiles.Sorted := True;
        MoFiles.Duplicates := dupError;
        MoFiles.CaseSensitive := false;
{$IFDEF MSWINDOWS}
        filelist.Duplicates := dupError;
        filelist.CaseSensitive := false;
{$ENDIF}
        filelist.Sorted := True;
      end;

      destructor TFileLocator.Destroy;
      begin
        while filelist.Count <> 0 do
        begin
          filelist.Objects[0].Free;
          filelist.delete(0);
        end;
        FreeAndNil(filelist);
        FreeAndNil(MoFiles);
        FreeAndNil(MoFilesCS);
        inherited;
      end;

      function TFileLocator.FileExists(filename: FilenameString): boolean;
      var
        idx: Integer;
      begin
        if LeftStr(filename, length(basedirectory)) = basedirectory then
        begin
          // Cut off basedirectory if the file is located beneath that base directory
          filename := MidStr(filename, length(basedirectory) + 1, maxint);
        end;
        Result := filelist.Find(filename, idx);
      end;

      function TFileLocator.GetMoFile(filename: FilenameString;
        DebugLogger: TDebugLogger): TMoFile;
      var
        fi: TEmbeddedFileInfo;
        idx: Integer;
        idxname: FilenameString;
        Offset, Size: int64;
        realfilename: FilenameString;
      begin
        // Find real filename
        Offset := 0;
        Size := 0;
        realfilename := filename;
        if LeftStr(filename, length(basedirectory)) = basedirectory then
        begin
          filename := MidStr(filename, length(basedirectory) + 1, maxint);
          idx := filelist.IndexOf(filename);
          if idx <> -1 then
          begin
            fi := filelist.Objects[idx] as TEmbeddedFileInfo;
            realfilename := ExecutableFilename;
            Offset := fi.Offset;
            Size := fi.Size;
{$IFDEF DXGETTEXTDEBUG}
            DebugLogger('Instead of ' + filename + ', using ' + realfilename +
              ' from offset ' + IntToStr(Offset) + ', size ' + IntToStr(Size));
{$ENDIF}
          end;
        end;

{$IFDEF DXGETTEXTDEBUG}
        DebugLogger('Reading .mo data from file ''' + filename + '''');
{$ENDIF}
        // Find TMoFile object
        MoFilesCS.BeginWrite;
        try
          idxname := realfilename + ' //\\ ' + IntToStr(Offset);
          if MoFiles.Find(idxname, idx) then
          begin
            Result := MoFiles.Objects[idx] as TMoFile;
          end
          else
          begin
            Result := TMoFile.Create(realfilename, Offset, Size,
              UseMemoryMappedFiles);
            MoFiles.AddObject(idxname, Result);
          end;
          inc(Result.Users);
        finally
          MoFilesCS.EndWrite;
        end;
      end;

      function TFileLocator.ReadInt64(str: TStream): int64;
      begin
        Assert(SizeOf(Result) = 8);
        str.ReadBuffer(Result, 8);
      end;

      procedure TFileLocator.ReleaseMoFile(mofile: TMoFile);
      var
        i: Integer;
      begin
        Assert(mofile <> nil);

        MoFilesCS.BeginWrite;
        try
          dec(mofile.Users);
          if mofile.Users <= 0 then
          begin
            i := MoFiles.Count - 1;
            while i >= 0 do
            begin
              if MoFiles.Objects[i] = mofile then
              begin
                MoFiles.delete(i);
                FreeAndNil(mofile);
                break;
              end;
              dec(i);
            end;
          end;
        finally
          MoFilesCS.EndWrite;
        end;
      end;

    { TTP_Retranslator }

      constructor TTP_Retranslator.Create;
      begin
        list := TList.Create;
      end;

      destructor TTP_Retranslator.Destroy;
      var
        i: Integer;
      begin
        for i := 0 to list.Count - 1 do
          TObject(list.Items[i]).Free;
        FreeAndNil(list);
        inherited;
      end;

      procedure TTP_Retranslator.Execute;
      var
        i: Integer;
        sl: TStrings;
        item: TTP_RetranslatorItem;
        newvalue: TranslatedUnicodeString;
        comp: TGnuGettextComponentMarker;
        ppi: PPropInfo;
      begin
        for i := 0 to list.Count - 1 do
        begin
          item := TObject(list.Items[i]) as TTP_RetranslatorItem;
          if not Assigned(item.obj) then
            continue; // JR 20223-04-18 to avoid protection faults
          if item.obj is TComponent then
          begin
            comp := TComponent(item.obj).FindComponent('GNUgettextMarker')
              as TGnuGettextComponentMarker;
            if Assigned(comp) and (self <> comp.Retranslator) then
            begin
              comp.Retranslator.Execute;
              continue;
            end;
          end;
          if item.obj is TStrings then
          begin
            // Since we don't know the order of items in sl, and don't have
            // the original .Objects[] anywhere, we cannot anticipate anything
            // about the current sl.Strings[] and sl.Objects[] values. We therefore
            // have to discard both values. We can, however, set the original .Strings[]
            // value into the list and retranslate that.
            sl := TStringList.Create;
            try
              sl.Text := item.OldValue;
              Instance.TranslateStrings(sl, textdomain);
              (item.obj as TStrings).BeginUpdate;
              try
                (item.obj as TStrings).Text := sl.Text;
              finally
                (item.obj as TStrings).EndUpdate;
              end;
            finally
              FreeAndNil(sl);
            end;
          end
          else
          begin
            if (textdomain = '') or (textdomain = DefaultTextDomain) then
              newvalue := ComponentGettext(item.OldValue, Instance)
            else
              newvalue := Instance.dgettext(textdomain, item.OldValue);
            ppi := GetPropInfo(item.obj, item.Propname);
            if ppi <> nil then
            begin
              SetWideStrProp(item.obj, ppi, newvalue);
            end
            else
            begin
{$IFDEF DXGETTEXTDEBUG}
              Instance.DebugWriteln
                ('ERROR: On retranslation, property disappeared: ' +
                item.Propname + ' for object of type ' + item.obj.ClassName);
{$ENDIF}
            end;
          end;
        end;
      end;

      procedure TTP_Retranslator.Remember(obj: TObject;
        Propname: ComponentNameString; OldValue: TranslatedUnicodeString);
      var
        item: TTP_RetranslatorItem;
      begin
        item := TTP_RetranslatorItem.Create;
        item.obj := obj;
        item.Propname := Propname;
        item.OldValue := OldValue;
        list.Add(item);
      end;

    { TGnuGettextComponentMarker }

      destructor TGnuGettextComponentMarker.Destroy;
      begin
        FreeAndNil(Retranslator);
        inherited;
      end;

    { THook }

      constructor THook.Create(OldProcedure, NewProcedure: pointer;
        FollowJump: boolean = false);
      { Idea and original code from Igor Siticov }
      { Modified by Jacques Garcia Vazquez and Lars Dybdahl }
      begin
{$IFNDEF CPU386}
{$IFNDEF CPUx64}
        raise Exception.Create
          ('This procedure only works on Intel i386 or x64 compatible processors.');
{$ENDIF}
{$ENDIF}
        oldproc := OldProcedure;
        newproc := NewProcedure;

        Reset(FollowJump);
      end;

      destructor THook.Destroy;
      begin
        Shutdown;
        inherited;
      end;

      procedure THook.Disable;
      begin
        Assert(PatchPosition <> nil,
          'Patch position in THook was nil when Disable was called');
        PatchPosition[0] := Original[0];
        PatchPosition[1] := Original[1];
        PatchPosition[2] := Original[2];
        PatchPosition[3] := Original[3];
        PatchPosition[4] := Original[4];
      end;

      procedure THook.Enable;
      begin
        Assert(PatchPosition <> nil,
          'Patch position in THook was nil when Enable was called');
        PatchPosition[0] := Patch[0];
        PatchPosition[1] := Patch[1];
        PatchPosition[2] := Patch[2];
        PatchPosition[3] := Patch[3];
        PatchPosition[4] := Patch[4];
      end;

      procedure THook.Reset(FollowJump: boolean);
      var
        Offset: Integer;
{$IFDEF LINUX}
        p: pointer;
        pagesize: Integer;
{$ENDIF}
{$IFDEF MSWindows}
        ov: Cardinal;
{$ENDIF}
      begin
        if PatchPosition <> nil then
          Shutdown;

        PatchPosition := oldproc;
        if FollowJump and (Word(oldproc^) = $25FF) then
        begin
          // This finds the correct procedure if a virtual jump has been inserted
          // at the procedure address
          inc(PatchPosition, 2); // skip the jump
          PatchPosition := PAnsiChar(pointer(pointer(PatchPosition)^)^);
        end;
        Offset := Integer(newproc) - Integer(pointer(PatchPosition)) - 5;

        Patch[0] := ansichar($E9);
        Patch[1] := ansichar(Offset and 255);
        Patch[2] := ansichar((Offset shr 8) and 255);
        Patch[3] := ansichar((Offset shr 16) and 255);
        Patch[4] := ansichar((Offset shr 24) and 255);

        Original[0] := PatchPosition[0];
        Original[1] := PatchPosition[1];
        Original[2] := PatchPosition[2];
        Original[3] := PatchPosition[3];
        Original[4] := PatchPosition[4];

{$IFDEF MSWINDOWS}
        if not VirtualProtect(pointer(PatchPosition), 5,
          PAGE_EXECUTE_READWRITE, @ov) then
          RaiseLastOSError;
{$ENDIF}
{$IFDEF LINUX}
        pagesize := sysconf(_SC_PAGE_SIZE);
        p := pointer(PatchPosition);
        p := pointer((Integer(p) + pagesize - 1) and not(pagesize - 1) -
          pagesize);
        if mprotect(p, pagesize, PROT_READ + PROT_WRITE + PROT_EXEC) <> 0 then
          RaiseLastOSError;
{$ENDIF}
      end;

      procedure THook.Shutdown;
      begin
        Disable;
        PatchPosition := nil;
      end;

      procedure HookIntoResourceStrings(enabled: boolean = True;
        SupportPackages: boolean = false);
      begin
        HookLoadResString.Reset(SupportPackages);
        HookLoadStr.Reset(SupportPackages);
        HookFmtLoadStr.Reset(SupportPackages);
        if enabled then
        begin
          HookLoadResString.Enable;
          HookLoadStr.Enable;
          HookFmtLoadStr.Enable;
        end;
      end;

    { TMoFile }

      function TMoFile.autoswap32(i: Cardinal): Cardinal;
      var
        cnv1, cnv2: record case Integer of 0: (arr: array [0 .. 3] of byte);
        1: (int: Cardinal);
      end;

  begin
    if doswap then
    begin
      cnv1.int := i;
      cnv2.arr[0] := cnv1.arr[3];
      cnv2.arr[1] := cnv1.arr[2];
      cnv2.arr[2] := cnv1.arr[1];
      cnv2.arr[3] := cnv1.arr[0];
      Result := cnv2.int;
    end
    else
      Result := i;
  end;

  function TMoFile.CardinalInMem(baseptr: PAnsiChar; Offset: Cardinal)
    : Cardinal;
  var
    pc: ^Cardinal;
  begin
    inc(baseptr, Offset);
    pc := pointer(baseptr);
    Result := pc^;
    if doswap then
      autoswap32(Result);
  end;

  constructor TMoFile.Create(const filename: FilenameString;
    const Offset: int64; Size: int64; const xUseMemoryMappedFiles: boolean);
  var
    i: Cardinal;
    nn: Integer;
    mofile: TFileStream;
  begin
    if SizeOf(i) <> 4 then
      raise EGGProgrammingError.Create
        ('TDomain in gnugettext is written for an architecture that has 32 bit integers.');

{$IFDEF mswindows}
    FUseMemoryMappedFiles := xUseMemoryMappedFiles;
{$ENDIF}
{$IFDEF linux}
    FUseMemoryMappedFiles := false;
{$ENDIF}
    if FUseMemoryMappedFiles then
    begin
      // Map the mo file into memory and let the operating system decide how to cache
      mo := createfile(PChar(filename), GENERIC_READ, FILE_SHARE_READ, nil,
        OPEN_EXISTING, 0, 0);
      if mo = INVALID_HANDLE_VALUE then
        raise EGGIOError.Create('Cannot open file ' + filename);
      momapping := CreateFileMapping(mo, nil, PAGE_READONLY, 0, 0, nil);
      if momapping = 0 then
        raise EGGIOError.Create('Cannot create memory map on file ' + filename);
      momemoryHandle := MapViewOfFile(momapping, FILE_MAP_READ, 0, 0, 0);
      if momemoryHandle = nil then
      begin
        raise EGGIOError.Create('Cannot map file ' + filename +
          ' into memory. Reason: ' + GetLastWinError);
      end;
      momemory := momemoryHandle + Offset;
    end
    else
    begin
      // Read the whole file into memory
      mofile := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
      try
        if (Size = 0) then
          Size := mofile.Size;
        Getmem(momemoryHandle, Size);
        momemory := momemoryHandle;
        mofile.Seek(Offset, soFromBeginning);
        mofile.ReadBuffer(momemory^, Size);
      finally
        FreeAndNil(mofile);
      end;
    end;

    // Check the magic number
    doswap := false;
    i := CardinalInMem(momemory, 0);
    if (i <> $950412DE) and (i <> $DE120495) then
      raise EGGIOError.Create('This file is not a valid GNU gettext mo file: ' +
        filename);
    doswap := (i = $DE120495);

    // Find the positions in the file according to the file format spec
    CardinalInMem(momemory, 4);
    // Read the version number, but don't use it for anything.
    N := CardinalInMem(momemory, 8); // Get string count
    O := CardinalInMem(momemory, 12); // Get offset of original strings
    T := CardinalInMem(momemory, 16); // Get offset of translated strings

    // Calculate start conditions for a binary search
    nn := N;
    startindex := 1;
    while nn <> 0 do
    begin
      nn := nn shr 1;
      startindex := startindex shl 1;
    end;
    startindex := startindex shr 1;
    startstep := startindex shr 1;
  end;

  destructor TMoFile.Destroy;
  begin
    if FUseMemoryMappedFiles then
    begin
      UnMapViewOfFile(momemoryHandle);
      try
        CloseHandle(momapping);
      except
      end;
      try
        CloseHandle(mo);
      except
      end;
    end
    else
    begin
      FreeMem(momemoryHandle);
    end;

    inherited;
  end;

  function TMoFile.gettext(const msgid: RawUtf8String; var found: boolean)
    : RawUtf8String;
  var
    i, step: Cardinal;
    Offset, pos: Cardinal;
    CompareResult: Integer;
    msgidptr, a, b: PAnsiChar;
    abidx: Integer;
    Size, msgidsize: Integer;
  begin
    found := false;
    msgidptr := PAnsiChar(msgid);
    msgidsize := length(msgid);

    // Do binary search
    i := startindex;
    step := startstep;
    while True do
    begin
      // Get string for index i
      pos := O + 8 * (i - 1);
      Offset := CardinalInMem(momemory, pos + 4);
      Size := CardinalInMem(momemory, pos);
      a := msgidptr;
      b := momemory + Offset;
      abidx := Size;
      if msgidsize < abidx then
        abidx := msgidsize;
      CompareResult := 0;
      while abidx <> 0 do
      begin
        CompareResult := Integer(byte(a^)) - Integer(byte(b^));
        if CompareResult <> 0 then
          break;
        dec(abidx);
        inc(a);
        inc(b);
      end;
      if CompareResult = 0 then
        CompareResult := msgidsize - Size;
      if CompareResult = 0 then
      begin // msgid=s
        // Found the msgid
        pos := T + 8 * (i - 1);
        Offset := CardinalInMem(momemory, pos + 4);
        Size := CardinalInMem(momemory, pos);
        SetString(Result, momemory + Offset, Size);
        found := True;
        break;
      end;
      if step = 0 then
      begin
        // Not found
        Result := msgid;
        break;
      end;
      if CompareResult < 0 then
      begin // msgid<s
        if i < 1 + step then
          i := 1
        else
          i := i - step;
        step := step shr 1;
      end
      else
      begin // msgid>s
        i := i + step;
        if i > N then
          i := N;
        step := step shr 1;
      end;
    end;
  end;

var
  param0: string;

initialization

{$IFDEF DXGETTEXTDEBUG}
{$IFDEF MSWINDOWS}
  MessageBox(0,
  'gnugettext.pas debugging is enabled. Turn it off before releasing this piece of software.',
  'Information', MB_OK);
{$ENDIF}
{$IFDEF LINUX}
writeln(stderr,
  'gnugettext.pas debugging is enabled. Turn it off before releasing this piece of software.');
{$ENDIF}
{$ENDIF}
{$IFDEF FPC}
{$IFDEF LINUX}
SetLocale(LC_ALL, '');
SetCWidestringManager;
{$ENDIF LINUX}
{$ENDIF FPC}
// Get DLL/shared object filename
SetLength(ExecutableFilename, 300); // MAX_PATH ?
{$IFDEF MSWINDOWS}
SetLength(ExecutableFilename, GetModuleFileName(HInstance,
  PChar(ExecutableFilename), length(ExecutableFilename)));
{$ENDIF}
{$IFDEF LINUX}
if ModuleIsLib or ModuleIsPackage then
begin
  // This line has not been tested on Linux, yet, but should work.
  SetLength(ExecutableFilename, GetModuleFileName(0, PChar(ExecutableFilename),
    length(ExecutableFilename)));
end
else
  ExecutableFilename := Paramstr(0);
{$ENDIF}
FileLocator := TFileLocator.Create;
FileLocator.Analyze;
ResourceStringDomainList := TStringList.Create;
ResourceStringDomainList.Add(DefaultTextDomain);
ResourceStringDomainListCS := TMultiReadExclusiveWriteSynchronizer.Create;
ComponentDomainList := TStringList.Create;
ComponentDomainList.Add(DefaultTextDomain);
ComponentDomainListCS := TMultiReadExclusiveWriteSynchronizer.Create;
DefaultInstance := TGnuGettextInstance.Create;
{$IFDEF MSWINDOWS}
Win32PlatformIsUnicode := (Win32Platform = VER_PLATFORM_WIN32_NT);
{$ENDIF}

// replace Borlands LoadResString with gettext enabled version:
{$IFDEF UNICODE}
HookLoadResString := THook.Create(@System.LoadResString, @LoadResStringW);
{$ELSE}
HookLoadResString := THook.Create(@System.LoadResString, @LoadResStringA);
{$ENDIF}
HookLoadStr := THook.Create(@System.SysUtils.LoadStr, @SysUtilsLoadStr);
HookFmtLoadStr := THook.Create(@System.SysUtils.FmtLoadStr,
  @SysUtilsFmtLoadStr);
param0 := lowercase(extractfilename(Paramstr(0)));
if (param0 <> 'delphi32.exe') and (param0 <> 'kylix') and (param0 <> 'bds.exe')
then
  HookIntoResourceStrings(AutoCreateHooks, false);
param0 := '';

finalization

FreeAndNil(DefaultInstance);
FreeAndNil(ResourceStringDomainListCS);
FreeAndNil(ResourceStringDomainList);
FreeAndNil(ComponentDomainListCS);
FreeAndNil(ComponentDomainList);
FreeAndNil(HookFmtLoadStr);
FreeAndNil(HookLoadStr);
FreeAndNil(HookLoadResString);
FreeAndNil(FileLocator);

end.
