//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Utils;

(*  Miscellaneous support utilities & classes *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.Windows,
  Winapi.ShellApi,
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.Types,
  System.UITypes,
  System.UIConsts,

  FMX.Dialogs,
  FMX.Graphics,
  FMX.Types,
  FMX.Objects,
  FMX.Consts,
  FMX.Controls,
  FMX.Forms,


  GXS.VectorGeometry,
  GXS.Strings,
  GXS.ApplicationFileIO;

type
  EGLUtilsException = class(Exception);
  TSqrt255Array = array[0..255] of Byte;
  PSqrt255Array = ^TSqrt255Array;

type
  THalfFloat = type Word;
  PHalfFloat = ^THalfFloat;

  TGraphicClass = class of TBitmap; // class of TGraphic in VCL

  TgxTextLayout = (tlTop, tlCenter, tlBottom);

  TgxMouseEvent = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer) of object;

  EGLOSError = EOSError;

  TProjectTargetNameFunc = function(): string;

  // Several define from unit Consts
const
  sAllFilter: string = SMsgDlgAll; ///in VCL -> sAllFilter;
  FONT_CHARS_COUNT = 2024;

var
  IsDesignTime: Boolean = False;
  vProjectTargetName: TProjectTargetNameFunc;

// Copies the values of Source to Dest (converting word values to integer values)
procedure WordToIntegerArray(Source: PWordArray; Dest: PIntegerArray; Count: Cardinal);
// Round ups to the nearest power of two, value must be positive
function RoundUpToPowerOf2(value: Integer): Integer;
// Round down to the nearest power of two, value must be strictly positive
function RoundDownToPowerOf2(value: Integer): Integer;
// Returns True if value is a true power of two
function IsPowerOf2(value: Integer): Boolean;
(* Read a CRLF terminated string from a stream.
   The CRLF is NOT in the returned string. *)
function ReadCRLFString(aStream: TStream): AnsiString;
// Write the string and a CRLF in the stream
procedure WriteCRLFString(aStream: TStream; const aString: AnsiString);
// Similar to SysUtils.TryStrToFloat, but ignores user's locale
function TryStrToFloat(const strValue: string; var val: Extended): Boolean;
// Similar to SysUtils.StrToFloatDef, but ignores user's locale
function StrToFloatDef(const strValue: string; defValue: Extended = 0): Extended;

// Converts a string into color
function StringToColorAdvancedSafe(const Str: string; const Default: TColor): TColor;
// Converts a string into color
function TryStringToColorAdvanced(const Str: string; var OutColor: TColor): Boolean;
// Converts a string into color
function StringToColorAdvanced(const Str: string): TColor;

(* Parses the next integer in the string.
   Initial non-numeric characters are skipper, p is altered, returns 0 if none
   found. '+' and '-' are acknowledged. *)
function ParseInteger(var p: PChar): Integer;
(* Parses the next integer in the string.
   Initial non-numeric characters are skipper, p is altered, returns 0 if none
   found. Both '.' and ',' are accepted as decimal separators. *)
function ParseFloat(var p: PChar): Extended;

// Saves "data" to "filename".
procedure SaveAnsiStringToFile(const fileName: string; const data: AnsiString);
// Returns the content of "filename".
function LoadAnsiStringFromFile(const fileName: string): AnsiString;

// Saves component to a file.
procedure SaveComponentToFile(const Component: TComponent; const FileName: string; const AsText: Boolean = True);
// Loads component from a file.
procedure LoadComponentFromFile(const Component: TComponent; const FileName: string; const AsText: Boolean = True);

(* Returns the size of "filename".
   Returns 0 (zero) is file does not exists. *)
function SizeOfFile(const fileName: string): Int64;

// Returns a pointer to an array containing the results of "255*sqrt(i/255)".
function GetSqrt255Array: PSqrt255Array;

// Pops up a simple dialog with msg and an Ok button.
procedure InformationDlg(const msg: string);
(* Pops up a simple question dialog with msg and yes/no buttons.
   Returns True if answer was "yes". *)
function QuestionDlg(const msg: string): Boolean;
// Posp a simple dialog with a string input.
function InputDlg(const aCaption, aPrompt, aDefault: string): string;
// Pops up a simple save picture dialog.
function SavePictureDialog(var AFileName: string; const ATitle: string = ''): Boolean;
// Pops up a simple open picture dialog.
function OpenPictureDialog(var AFileName: string; const ATitle: string = ''): Boolean;

procedure SetSceneMediaDir();

// CrossPlatform

function GetRectangle(const aLeft, aTop, aRight, aBottom: Integer): TRect;
(* Increases or decreases the width and height of the specified rectangle.
   Adds dx units to the left and right ends of the rectangle and dy units to
   the top and bottom. *)
procedure InflateRectangle(var aRect: TRect; dx, dy: Integer);
procedure IntersectRectangle(var aRect: TRect; const rect2: TRect);
procedure RaiseLastOSError;
(* Number of pixels per logical inch along the screen width for the device.
   Under Win32 awaits a HDC and returns its LOGPIXELSX. *)
function GetDeviceLogicalPixelsX(device: THandle): Integer; ///in VCL -> HDC
// Number of bits per pixel for the current desktop resolution.
function GetCurrentColorDepth: Integer;
// Returns the number of color bits associated to the given pixel format.
function PixelFormatToColorBits(aPixelFormat: TPixelFormat): Integer;
// Returns the bitmap's scanline for the specified row.
function BitmapScanLine(aBitmap: TBitmap; aRow: Integer): Pointer;
// Replace path delimiter to delimiter of the current platform.
procedure FixPathDelimiter(var S: string);
// Remove if possible part of path witch leads to project executable.
function RelativePath(const S: string): string;
(* Returns the current value of the highest-resolution counter.
   If the platform has none, should return a value derived from the highest
   precision time reference available, avoiding, if possible, timers that
   allocate specific system resources. *)
procedure QueryPerformanceCounter(var val: Int64);
(* Returns the frequency of the counter used by QueryPerformanceCounter.
   Return value is in ticks per second (Hz), returns False if no precision
   counter is available. *)
function QueryPerformanceFrequency(var val: Int64): Boolean;
(* Starts a precision timer.
   Returned value should just be considered as 'handle', even if it ain't so.
   Default platform implementation is to use QueryPerformanceCounter and
   QueryPerformanceFrequency, if higher precision references are available,
   they should be used. The timer will and must be stopped/terminated/released
   with StopPrecisionTimer. *)
function StartPrecisionTimer: Int64;
(* Computes time elapsed since timer start.
   Return time lap in seconds. *)
function PrecisionTimerLap(const precisionTimer: Int64): Double;
(* Computes time elapsed since timer start and stop timer.
   Return time lap in seconds. *)
function StopPrecisionTimer(const precisionTimer: Int64): Double;
// Returns time in millisecond from application start.
function AppTime: Double;
function MessageBoxOK(const Text, Caption: string): Integer;
procedure GLLoadBitmapFromInstance(Instance: LongInt; ABitmap: TBitmap; AName: string);
procedure ShowHTMLUrl(Url: string);
//function GLGetTickCount: int64;
procedure SetExeDirectory;
// StrUtils.pas
function AnsiStartsText(const ASubText, AText: string): Boolean;
// Classes.pas
function IsSubComponent(const AComponent: TComponent): Boolean;
procedure MakeSubComponent(const AComponent: TComponent; const Value: Boolean);
function FindUnitName(anObject: TObject): string; overload;
function FindUnitName(aClass: TClass): string; overload;
function FloatToHalf(Float: Single): THalfFloat;
function HalfToFloat(Half: THalfFloat): Single;
function GetValueFromStringsIndex(const AStrings: TStrings; const AIndex: Integer): string;
// Determine if the directory is writable.
function IsDirectoryWriteable(const AName: string): Boolean;
function CharToWideChar(const AChar: AnsiChar): WideChar;


//------------------------------------------------------
implementation
//------------------------------------------------------

var
  vSqrt255: TSqrt255Array;
  vInvPerformanceCounterFrequency: Double;
  vInvPerformanceCounterFrequencyReady: Boolean = False;
  vLastProjectTargetName: string;


procedure WordToIntegerArray(Source: PWordArray; Dest: PIntegerArray; Count: Cardinal);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Dest^[i] := Source^[i];
end;

function RoundUpToPowerOf2(value: Integer): Integer;
begin
  Result := 1;
  while (Result < value) do
    Result := Result shl 1;
end;

function RoundDownToPowerOf2(value: Integer): Integer;
begin
  if value > 0 then
  begin
    Result := 1 shl 30;
    while Result > value do
      Result := Result shr 1;
  end
  else
    Result := 1;
end;

function IsPowerOf2(value: Integer): Boolean;
begin
  Result := (RoundUpToPowerOf2(value) = value);
end;

function ReadCRLFString(aStream: TStream): AnsiString;
var
  c: AnsiChar;
begin
  Result := '';
  while Copy(Result, Length(Result) - 1, 2) <> #13#10 do
  begin
    aStream.Read(c, 1);
    Result := Result + c;
  end;
  Result := Copy(Result, 1, Length(Result) - 2);
end;

procedure WriteCRLFString(aStream: TStream; const aString: AnsiString);
const
  cCRLF: Integer = $0A0D;
begin
  with aStream do
  begin
    Write(aString[1], Length(aString));
    Write(cCRLF, 2);
  end;
end;

function TryStrToFloat(const strValue: string; var val: Extended): Boolean;
var
  i, j, divider, lLen, exponent: Integer;
  c: Char;
  v: Extended;
begin
  if strValue = '' then
  begin
    Result := False;
    Exit;
  end
  else
    v := 0;
  lLen := Length(strValue);
  while (lLen > 0) and (strValue[lLen] = ' ') do
    Dec(lLen);
  divider := lLen + 1;
  exponent := 0;
  for i := 1 to lLen do
  begin
    c := strValue[i];
    case c of
      ' ': if v <> 0 then
        begin
          Result := False;
          Exit;
        end;
      '0'..'9': v := (v * 10) + Integer(c) - Integer('0');
      ',', '.':
        begin
          if (divider > lLen) then
            divider := i + 1
          else
          begin
            Result := False;
            Exit;
          end;
        end;
      '-', '+': if i > 1 then
        begin
          Result := False;
          Exit;
        end;
      'e', 'E':
        begin
          if i + 1 > lLen then
          begin
            Result := False;
            Exit;
          end;
          for j := i + 1 to lLen do
          begin
            c := strValue[j];
            case c of
              '-', '+': if j <> i + 1 then
                begin
                  Result := False;
                  Exit;
                end;
              '0'..'9': exponent := (exponent * 10) + Integer(c) - Integer('0');
            else
              Result := False;
              Exit;
            end;
          end;
          if strValue[i + 1] <> '-' then
            exponent := -exponent;
          exponent := exponent - 1;
          lLen := i;
          if divider > lLen then
            divider := lLen;
          Break;
        end;
    else
      Result := False;
      Exit;
    end;
  end;
  divider := lLen - divider + exponent + 1;
  if strValue[1] = '-' then
  begin
    v := -v;
  end;
  if divider <> 0 then
    v := v * Exp(-divider * Ln(10));
  val := v;
  Result := True;
end;


function StrToFloatDef(const strValue: string; defValue: Extended = 0): Extended;
begin
  if not TryStrToFloat(strValue, Result) then
    result := defValue;
end;

function StringToColorAdvancedSafe(const Str: string; const Default: TColor): TColor;
begin
  if not TryStringToColorAdvanced(Str, Result) then
    Result := Default;
end;

function StringToColorAdvanced(const Str: string): TColor;
begin
  if not TryStringToColorAdvanced(Str, Result) then
    raise EGLUtilsException.CreateResFmt(@strInvalidColor, [Str]);
end;

function TryStringToColorAdvanced(const Str: string; var OutColor: TColor): Boolean;
var
  Code, I: Integer;
  Temp: string;
begin
  Result := True;
  Temp := Str;

  Val(Temp, I, Code); //to see if it is a number
  if Code = 0 then
    OutColor := TColor(I) //Str = $0000FF
  else
  begin
    if not IdentToColor(Temp, Longint(OutColor)) then //Str = clRed
    begin
      if AnsiStartsText('clr', Temp) then //Str = clrRed
      begin
        Delete(Temp, 3, 1);
        if not IdentToColor(Temp, Longint(OutColor)) then
          Result := False;
      end
      else if not IdentToColor('cl' + Temp, Longint(OutColor)) then //Str = Red
        Result := False;
    end;
  end;
end;

function ParseInteger(var p: PChar): Integer;
var
  neg: Boolean;
  c: Char;
begin
  Result := 0;
  if p = nil then
    Exit;
  neg := False;
  // skip non-numerics
  while not CharInSet(p^, [#0, '0'..'9', '+', '-']) do
    Inc(p);
  c := p^;
  if c = '+' then
    Inc(p)
  else if c = '-' then
  begin
    neg := True;
    Inc(p);
  end;
  // Parse numerics
  while True do
  begin
    c := p^;
    if not CharInSet(c, ['0'..'9']) then
      Break;
    Result := Result * 10 + Integer(c) - Integer('0');
    Inc(p);
  end;
  if neg then
    Result := -Result;
end;

function ParseFloat(var p: PChar): Extended;
var
  decimals, expSign, exponent: Integer;
  c: Char;
  neg: Boolean;
begin
  Result := 0;
  if p = nil then
    Exit;
  // skip non-numerics
  while not CharInSet(p^, [#0, '0'..'9', '+', '-']) do
    Inc(p);
  c := p^;
  if c = '+' then
  begin
    neg := False;
    Inc(p);
  end
  else if c = '-' then
  begin
    neg := True;
    Inc(p);
  end
  else
    neg := False;
  // parse numbers
  while CharInSet(p^, ['0'..'9']) do
  begin
    Result := Result * 10 + (Integer(p^) - Integer('0'));
    Inc(p);
  end;
  // parse dot, then decimals, if any
  decimals := 0;
  if (p^ = '.') then
  begin
    Inc(p);
    while CharInSet(p^, ['0'..'9']) do
    begin
      Result := Result * 10 + (Integer(p^) - Integer('0'));
      Inc(p);
      Dec(decimals);
    end;
  end;
  // parse exponent, if any
  if CharInSet(p^, ['e', 'E']) then
  begin
    Inc(p);
    // parse exponent sign
    c := p^;
    if c = '-' then
    begin
      expSign := -1;
      Inc(p);
    end
    else if c = '+' then
    begin
      expSign := 1;
      Inc(p);
    end
    else
      expSign := 1;
    // parse exponent
    exponent := 0;
    while CharInSet(p^, ['0'..'9']) do
    begin
      exponent := exponent * 10 + (Integer(p^) - Integer('0'));
      Inc(p);
    end;
    decimals := decimals + expSign * exponent;
  end;
  if decimals <> 0 then
    Result := Result * Exp(decimals * Ln(10));
  if neg then
    Result := -Result;
end;

procedure SaveAnsiStringToFile(const fileName: string; const data: AnsiString);
var
  n: Cardinal;
  fs: TStream;
begin
  fs := TFileStream.Create(fileName, fmCreate);
  try
    n := Length(data);
    if n > 0 then
      fs.Write(data[1], n);
  finally
    fs.Free;
  end;
end;

function LoadAnsiStringFromFile(const fileName: string): AnsiString;
var
  n: Cardinal;
  fs: TStream;
begin
  if FileExists(fileName) then
  begin
    fs := TFileStream.Create(fileName, fmOpenRead + fmShareDenyNone);
    try
      n := fs.Size;
      SetLength(Result, n);
      if n > 0 then
        fs.Read(Result[1], n);
    finally
      fs.Free;
    end;
  end
  else
    Result := '';
end;

procedure SaveComponentToFile(const Component: TComponent; const FileName: string; const AsText: Boolean);
var
  Stream: TStream;
  MemStream: TMemoryStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    if AsText then
    begin
      MemStream := TMemoryStream.Create;
      try
        MemStream.WriteComponent(Component);
        MemStream.Position := 0;
        ObjectBinaryToText(MemStream, Stream);
      finally
        MemStream.Free;
      end;
    end
    else
      Stream.WriteComponent(Component);
  finally
    Stream.Free;
  end;
end;

procedure LoadComponentFromFile(const Component: TComponent; const FileName: string; const AsText: Boolean = True);
var
  Stream: TStream;
  MemStream: TMemoryStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    if AsText then
    begin
      MemStream := TMemoryStream.Create;
      try
        ObjectTextToBinary(Stream, MemStream);
        MemStream.Position := 0;
        MemStream.ReadComponent(Component);
      finally
        MemStream.Free;
      end;
    end
    else
      Stream.ReadComponent(Component);
  finally
    Stream.Free;
  end;
end;

function SizeOfFile(const fileName: string): Int64;
var
  fs: TStream;
begin
  if FileExists(fileName) then
  begin
    fs := TFileStream.Create(fileName, fmOpenRead + fmShareDenyNone);
    try
      Result := fs.Size;
    finally
      fs.Free;
    end;
  end
  else
    Result := 0;
end;

function GetSqrt255Array: PSqrt255Array;
const
  cOneDiv255 = 1 / 255;
var
  i: Integer;
begin
  if vSqrt255[255] <> 255 then
  begin
    for i := 0 to 255 do
      vSqrt255[i] := Integer(Trunc(255 * Sqrt(i * cOneDiv255)));
  end;
  Result := @vSqrt255;
end;

procedure InformationDlg(const msg: string);
begin
  ShowMessage(msg);
end;

function QuestionDlg(const msg: string): Boolean;
begin
  Result := (MessageDlg(msg, TMsgDlgType.mtConfirmation,
              [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes);
end;

function InputDlg(const aCaption, aPrompt, aDefault: string): string;
begin
  Result := InputBox(aCaption, aPrompt, aDefault);
end;

function SavePictureDialog(var AFileName: string; const ATitle: string = ''): Boolean;
var
  SaveDialog: TSaveDialog; // in VCL -> TSavePictureDialog;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Options := [TOpenOption.ofHideReadOnly, TOpenOption.ofNoReadOnlyReturn];
    if ATitle <> '' then
      SaveDialog.Title := ATitle;
    SaveDialog.FileName := AFileName;
    Result := SaveDialog.Execute;
    if Result then
      AFileName := SaveDialog.FileName;
  finally
    SaveDialog.Free;
  end;
end;

function OpenPictureDialog(var AFileName: string; const ATitle: string = ''): Boolean;
var
  OpenDialog: TOpenDialog;  // in VCL -> TOpenPictureDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Options := [TOpenOption.ofHideReadOnly, TOpenOption.ofNoReadOnlyReturn];
    if ATitle <> '' then
      OpenDialog.Title := ATitle;
    OpenDialog.FileName := AFileName;
    Result := OpenDialog.Execute;
    if Result then
      AFileName := OpenDialog.FileName;
  finally
    OpenDialog.Free;
  end;
end;

function IsSubComponent(const AComponent: TComponent): Boolean;
begin
  Result := (csSubComponent in AComponent.ComponentStyle);
end;

procedure MakeSubComponent(const AComponent: TComponent; const Value: Boolean);
begin
  AComponent.SetSubComponent(Value);
end;

function AnsiStartsText(const ASubText, AText: string): Boolean;
begin
  Result := AnsiStartsText(ASubText, AText);
end;

function MessageBoxOK(const Text, Caption: string): Integer;
begin
  Application.ProcessMessages;
  Result := MB_OK; //<- Instead of Result := Application.MessageBox(PChar(Text), PChar(Caption), MB_OK);
end;

procedure GLLoadBitmapFromInstance(Instance: LongInt; ABitmap: TBitmap; AName: string);
begin
  /// TODO : Cannot assign to a read-only property E2129
  /// ABitmap.Handle := LoadBitmap(Instance, PChar(AName));
end;

function GLGetTickCount: int64;
begin
  result := TThread.GetTickCount;
end;

procedure ShowHTMLUrl(Url: string);
begin
  ShellExecute(0, 'open', PChar(Url), nil, nil, SW_SHOW);
end;

function GetRectangle(const aLeft, aTop, aRight, aBottom: Integer): TRect;
begin
  Result.Left := aLeft;
  Result.Top := aTop;
  Result.Right := aRight;
  Result.Bottom := aBottom;
end;

procedure InflateRectangle(var aRect: TRect; dx, dy: Integer);
begin
  aRect.Left := aRect.Left - dx;
  aRect.Right := aRect.Right + dx;
  if aRect.Right < aRect.Left then
    aRect.Right := aRect.Left;
  aRect.Top := aRect.Top - dy;
  aRect.Bottom := aRect.Bottom + dy;
  if aRect.Bottom < aRect.Top then
    aRect.Bottom := aRect.Top;
end;

procedure IntersectRectangle(var aRect: TRect; const rect2: TRect);
var
  a: Integer;
begin
  if (aRect.Left > rect2.Right) or (aRect.Right < rect2.Left)
    or (aRect.Top > rect2.Bottom) or (aRect.Bottom < rect2.Top) then
  begin
    // no intersection
    a := 0;
    aRect.Left := a;
    aRect.Right := a;
    aRect.Top := a;
    aRect.Bottom := a;
  end
  else
  begin
    if aRect.Left < rect2.Left then
      aRect.Left := rect2.Left;
    if aRect.Right > rect2.Right then
      aRect.Right := rect2.Right;
    if aRect.Top < rect2.Top then
      aRect.Top := rect2.Top;
    if aRect.Bottom > rect2.Bottom then
      aRect.Bottom := rect2.Bottom;
  end;
end;

procedure RaiseLastOSError;
var
  e: EGLOSError;
begin
  e := EGLOSError.Create('OS Error : ' + SysErrorMessage(GetLastError));
  raise e;
end;

type
  TDeviceCapabilities = record
    Xdpi, Ydpi: integer; // Number of pixels per logical inch.
    Depth: integer; // The bit depth.
    NumColors: integer; // Number of entries in the device's color table.
  end;

function GetDeviceCapabilities: TDeviceCapabilities;
var
  Device: HDC;
begin
  Device := GetDC(0);
  try
    result.Xdpi := GetDeviceCaps(Device, LOGPIXELSX);
    result.Ydpi := GetDeviceCaps(Device, LOGPIXELSY);
    result.Depth := GetDeviceCaps(Device, BITSPIXEL);
    result.NumColors := GetDeviceCaps(Device, NUMCOLORS);
  finally
    ReleaseDC(0, Device);
  end;
end;

function GetDeviceLogicalPixelsX(device: THandle): Integer;
begin
  result := GetDeviceCapabilities().Xdpi;
end;

function GetCurrentColorDepth: Integer;
begin
  result := GetDeviceCapabilities().Depth;
end;

function PixelFormatToColorBits(aPixelFormat: TPixelFormat): Integer;
begin
  case aPixelFormat of
    TPixelFormat.None: Result := GetCurrentColorDepth; // use current color depth
    TPixelFormat.BGR5_A1: Result := 1;
    TPixelFormat.BGRA4: Result := 4;
    TPixelFormat.RGBA: Result := 8;
    TPixelFormat.RGBA16: Result := 16;
    TPixelFormat.RGBA32F: Result := 32;
  else
    Result := 24;
  end;
end;

function BitmapScanLine(aBitmap: TBitmap; aRow: Integer): Pointer;
var
  BitmapData : TBitmapData;
begin
  aBitmap.Map(TMapAccess.ReadWrite, BitmapData);
  Result := BitmapData.GetScanline(aRow); //in VCL the Result := aBitmap.ScanLine[aRow];
end;

procedure FixPathDelimiter(var S: string);
var
  I: Integer;
begin
  for I := Length(S) downto 1 do
    if (S[I] = '/') or (S[I] = '\') then
      S[I] := PathDelim;
end;

function RelativePath(const S: string): string;
var
  path: UTF8String;
begin
  Result := S;
  if IsDesignTime then
  begin
    if Assigned(vProjectTargetName) then
    begin
      path :=  vProjectTargetName();
      if Length(path) = 0 then
        path := vLastProjectTargetName
      else
        vLastProjectTargetName := path;
      path := IncludeTrailingPathDelimiter(ExtractFilePath(path));
    end
    else
      exit;
  end
  else
  begin
    path := ExtractFilePath(ParamStr(0));
    path := IncludeTrailingPathDelimiter(path);
  end;
  if Pos(path, S) = 1 then
    Delete(Result, 1, Length(path));
end;

procedure QueryPerformanceCounter(var val: Int64);
begin
  QueryPerformanceCounter(val);
end;

function QueryPerformanceFrequency(var val: Int64): Boolean;
begin
  Result := Boolean(QueryPerformanceFrequency(val));
end;

function StartPrecisionTimer: Int64;
begin
  QueryPerformanceCounter(Result);
end;

function PrecisionTimerLap(const precisionTimer: Int64): Double;
begin
  // we can do this, because we don't really stop anything
  Result := StopPrecisionTimer(precisionTimer);
end;

function StopPrecisionTimer(const precisionTimer: Int64): Double;
var
  cur, freq: Int64;
begin
  QueryPerformanceCounter(cur);
  if not vInvPerformanceCounterFrequencyReady then
  begin
    QueryPerformanceFrequency(freq);
    vInvPerformanceCounterFrequency := 1.0 / freq;
    vInvPerformanceCounterFrequencyReady := True;
  end;
  Result := (cur - precisionTimer) * vInvPerformanceCounterFrequency;
end;

var
  vSStartTime : TDateTime;
  vLastTime: TDateTime;
  vDeltaMilliSecond: TDateTime;

function AppTime: Double;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  with SystemTime do
    Result := (wHour * (MinsPerHour * SecsPerMin * MSecsPerSec) +
             wMinute * (SecsPerMin * MSecsPerSec) +
             wSecond * MSecsPerSec +
             wMilliSeconds) - vSStartTime;
  // Hack to fix time precession
  if Result - vLastTime = 0 then
  begin
    Result := Result + vDeltaMilliSecond;
    vDeltaMilliSecond := vDeltaMilliSecond + 0.1;
  end
  else begin
    vLastTime := Result;
    vDeltaMilliSecond := 0.1;
  end;
end;

function FindUnitName(anObject: TObject): string;
begin
  if Assigned(anObject) then
    Result := anObject.UnitName
  else
    Result := '';
end;

function FindUnitName(aClass: TClass): string;
begin
  if Assigned(aClass) then
    Result := aClass.UnitName
  else
    Result := '';
end;

procedure SetExeDirectory;
var
  path: string;
begin
  if IsDesignTime then
  begin
    if Assigned(vProjectTargetName) then
    begin
      path :=  vProjectTargetName();
      if Length(path) = 0 then
        path := vLastProjectTargetName
      else
        vLastProjectTargetName := path;
      path := IncludeTrailingPathDelimiter(ExtractFilePath(path));
      SetCurrentDir(path);
    end;
  end
  else
  begin
    path := ExtractFilePath(ParamStr(0));
    path := IncludeTrailingPathDelimiter(path);
    SetCurrentDir(path);
  end;
end;

function HalfToFloat(Half: THalfFloat): Single;
var
  Dst, Sign, Mantissa: LongWord;
  Exp: LongInt;
begin
  // extract sign, exponent, and mantissa from half number
  Sign := Half shr 15;
  Exp := (Half and $7C00) shr 10;
  Mantissa := Half and 1023;

  if (Exp > 0) and (Exp < 31) then
  begin
    // common normalized number
    Exp := Exp + (127 - 15);
    Mantissa := Mantissa shl 13;
    Dst := (Sign shl 31) or (LongWord(Exp) shl 23) or Mantissa;
    // Result := Power(-1, Sign) * Power(2, Exp - 15) * (1 + Mantissa / 1024);
  end
  else if (Exp = 0) and (Mantissa = 0) then
  begin
    // zero - preserve sign
    Dst := Sign shl 31;
  end
  else if (Exp = 0) and (Mantissa <> 0) then
  begin
    // denormalized number - renormalize it
    while (Mantissa and $00000400) = 0 do
    begin
      Mantissa := Mantissa shl 1;
      Dec(Exp);
    end;
    Inc(Exp);
    Mantissa := Mantissa and not $00000400;
    // now assemble normalized number
    Exp := Exp + (127 - 15);
    Mantissa := Mantissa shl 13;
    Dst := (Sign shl 31) or (LongWord(Exp) shl 23) or Mantissa;
    // Result := Power(-1, Sign) * Power(2, -14) * (Mantissa / 1024);
  end
  else if (Exp = 31) and (Mantissa = 0) then
  begin
    // +/- infinity
    Dst := (Sign shl 31) or $7F800000;
  end
  else //if (Exp = 31) and (Mantisa <> 0) then
  begin
    // not a number - preserve sign and mantissa
    Dst := (Sign shl 31) or $7F800000 or (Mantissa shl 13);
  end;

  // reinterpret LongWord as Single
  Result := PSingle(@Dst)^;
end;

function FloatToHalf(Float: Single): THalfFloat;
var
  Src: LongWord;
  Sign, Exp, Mantissa: LongInt;
begin
  Src := PLongWord(@Float)^;
  // extract sign, exponent, and mantissa from Single number
  Sign := Src shr 31;
  Exp := LongInt((Src and $7F800000) shr 23) - 127 + 15;
  Mantissa := Src and $007FFFFF;

  if (Exp > 0) and (Exp < 30) then
  begin
    // simple case - round the significand and combine it with the sign and exponent
    Result := (Sign shl 15) or (Exp shl 10) or ((Mantissa + $00001000) shr 13);
  end
  else if Src = 0 then
  begin
    // input float is zero - return zero
    Result := 0;
  end
  else
  begin
    // difficult case - lengthy conversion
    if Exp <= 0 then
    begin
      if Exp < -10 then
      begin
        // input float's value is less than HalfMin, return zero
        Result := 0;
      end
      else
      begin
        // Float is a normalized Single whose magnitude is less than HalfNormMin.
        // We convert it to denormalized half.
        Mantissa := (Mantissa or $00800000) shr (1 - Exp);
        // round to nearest
        if (Mantissa and $00001000) > 0 then
          Mantissa := Mantissa + $00002000;
        // assemble Sign and Mantissa (Exp is zero to get denotmalized number)
        Result := (Sign shl 15) or (Mantissa shr 13);
      end;
    end
    else if Exp = 255 - 127 + 15 then
    begin
      if Mantissa = 0 then
      begin
        // input float is infinity, create infinity half with original sign
        Result := (Sign shl 15) or $7C00;
      end
      else
      begin
        // input float is NaN, create half NaN with original sign and mantissa
        Result := (Sign shl 15) or $7C00 or (Mantissa shr 13);
      end;
    end
    else
    begin
      // Exp is > 0 so input float is normalized Single
      // round to nearest
      if (Mantissa and $00001000) > 0 then
      begin
        Mantissa := Mantissa + $00002000;
        if (Mantissa and $00800000) > 0 then
        begin
          Mantissa := 0;
          Exp := Exp + 1;
        end;
      end;

      if Exp > 30 then
      begin
        // exponent overflow - return infinity half
        Result := (Sign shl 15) or $7C00;
      end
      else
        // assemble normalized half
        Result := (Sign shl 15) or (Exp shl 10) or (Mantissa shr 13);
    end;
  end;
end;

function GetValueFromStringsIndex(const AStrings: TStrings; const AIndex: Integer): string;
begin
  Result := AStrings.ValueFromIndex[AIndex];
end;

function IsDirectoryWriteable(const AName: string): Boolean;
var
  LFileName: String;
  LHandle: THandle;
begin
  LFileName := IncludeTrailingPathDelimiter(AName) + 'chk.tmp';
  LHandle := CreateFile(PChar(LFileName), GENERIC_READ or GENERIC_WRITE, 0, nil,
    CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0);
  Result := LHandle <> INVALID_HANDLE_VALUE;
  if Result then
    CloseHandle(LHandle);
end;


function CharToWideChar(const AChar: AnsiChar): WideChar;
var
  lResult: PWideChar;
begin
  GetMem(lResult, 2);
  MultiByteToWideChar(CP_ACP, 0, @AChar, 1, lResult, 2);
  Result := lResult^;
  FreeMem(lResult, 2);
end;

procedure SetSceneMediaDir();
var
  path: String;
  p: Integer;
begin
   path := ParamStr(0);
   path := LowerCase(ExtractFilePath(path));
   p := Pos('demos', path);
   Delete(path, p+5, Length(path));
   path := IncludeTrailingPathDelimiter(path) + 'media';
   SetCurrentDir(path);
end;

//-------------------------------------------
initialization
//-------------------------------------------

  vSStartTime := AppTime;

end.

