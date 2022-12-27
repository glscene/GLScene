//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLS.FileVfsPAK;

(*
  Support-code for loading files from Quake II PAK Files.
  When instance is created all LoadFromFile methods using
  GLS.ApplicationFileIO mechanism will be pointed into PAK file.
  You can change current PAK file by ActivePak variable.

*)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.Contnrs,
  System.SysUtils,

  GLS.Strings,
  GLS.ApplicationFileIO;

const
  SIGN = 'PACK'; // Signature for uncompressed - raw pak.
  SIGN_COMPRESSED = 'PACZ'; // Signature for compressed pak.

type

  TZCompressedMode = (Good, Fast, Auto, None);

  TPakHeader = record
    Signature: array [0 .. 3] of AnsiChar;
    DirOffset: integer;
    DirLength: integer;
  end;

  TFileSection = record
    FileName: array [0 .. 119] of AnsiChar;
    FilePos: integer;
    FileLength: integer;
  end;

  TGLVfsPAK = class(TComponent)
  private
    FPakFiles: TStringList;
    FHeader: TPakHeader;
    FHeaderList: array of TPakHeader;
    FStream: TFileStream;
    FStreamList: TObjectList;
    FFiles: TStrings;
    FFilesLists: TObjectList;
    FFileName: string;
    FCompressionLevel: TZCompressedMode;
    FCompressed: Boolean;
    function GetFileCount: integer;
    procedure MakeFileList;
    function GetStreamNumber: integer;
    procedure SetStreamNumber(i: integer);
  public
    property PakFiles: TStringList read FPakFiles;
    property Files: TStrings read FFiles;
    property ActivePakNum: integer read GetStreamNumber write SetStreamNumber;
    property FileCount: integer Read GetFileCount;
    property PakFileName: string Read FFileName;
    property Compressed: Boolean read FCompressed;
    property CompressionLevel: TZCompressedMode read FCompressionLevel;
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; const CbrMode: TZCompressedMode);
      reintroduce; overload;
    destructor Destroy; override;
    // for Mode value search Delphi Help for "File open mode constants"
    procedure LoadFromFile(const FileName: string; Mode: word);
    procedure ClearPakFiles;
    function FileExists(const FileName: string): Boolean;
    function GetFile(index: integer): TStream; overload;
    function GetFile(const FileName: string): TStream; overload;
    function GetFileSize(index: integer): integer; overload;
    function GetFileSize(const FileName: string): integer; overload;
    procedure AddFromStream(const FileName, Path: string; F: TStream);
    procedure AddFromFile(const FileName, Path: string);
    procedure AddEmptyFile(const FileName, Path: string);
    procedure RemoveFile(index: integer); overload;
    procedure RemoveFile(const FileName: string); overload;
    procedure Extract(index: integer; const NewName: string); overload;
    procedure Extract(const FileName, NewName: string); overload;
  end;

function PAKCreateFileStream(const FileName: string; Mode: word): TStream;
function PAKFileStreamExists(const FileName: string): Boolean;

var
  ActiveVfsPAK: TGLVfsPAK;

// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------

var
  Dir: TFileSection;

function BackToSlash(const s: string): string;
var
  i: integer;
begin
  SetLength(Result, Length(s));
  for i := 1 to Length(s) do
    if s[i] = '\' then
      Result[i] := '/'
    else
      Result[i] := s[i];
end;

function PAKCreateFileStream(const FileName: string; Mode: word): TStream;
var
  i: integer;
begin
  with ActiveVfsPAK do
    for i := FStreamList.Count - 1 downto 0 do
    begin
      FFiles := TStrings(FFilesLists[i]);
      if FileExists(BackToSlash(FileName)) then
      begin
        FHeader := FHeaderList[i];
        FStream := TFileStream(FStreamList[i]);
        Result := GetFile(BackToSlash(FileName));
        Exit;
      end
      else
      begin
        if FileExists(FileName) then
        begin
          Result := TFileStream.Create(FileName, fmOpenReadWrite or
            fmShareDenyWrite);
          Exit;
        end
        else
        begin
          Result := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
          Exit;
        end;
      end;
    end;
  if FileExists(FileName) then
  begin
    Result := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite);
    Exit;
  end
  else
  begin
    Result := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
    Exit;
  end;
  Result.Free;
end;

function PAKFileStreamExists(const FileName: string): Boolean;
var
  i: integer;
begin
  with ActiveVfsPAK do
    for i := 0 to FStreamList.Count - 1 do
    begin
      FFiles := TStrings(FFilesLists[i]);
      if FileExists(BackToSlash(FileName)) then
      begin
        Result := True;
        Exit;
      end;
    end;
  Result := FileExists(FileName);
end;

// --------------------------
// TGLVfsPAK
// --------------------------

function TGLVfsPAK.GetStreamNumber: integer;
begin
  Result := FStreamList.IndexOf(FStream);
end;

procedure TGLVfsPAK.SetStreamNumber(i: integer);
begin
  FStream := TFileStream(FStreamList[i]);
end;

constructor TGLVfsPAK.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPakFiles := TStringList.Create;
  FStreamList := TObjectList.Create(True);
  FFilesLists := TObjectList.Create(True);
  ActiveVfsPAK := Self;
  vAFIOCreateFileStream := PAKCreateFileStream;
  vAFIOFileStreamExists := PAKFileStreamExists;
  FCompressionLevel := None;
  FCompressed := False;
end;

constructor TGLVfsPAK.Create(AOwner: TComponent;
  const CbrMode: TZCompressedMode);
begin
  Self.Create(AOwner);
  FCompressionLevel := None;
  FCompressed := FCompressionLevel <> None;
end;

destructor TGLVfsPAK.Destroy;
begin
  vAFIOCreateFileStream := nil;
  vAFIOFileStreamExists := nil;
  SetLength(FHeaderList, 0);
  FPakFiles.Free;
  // Objects are automatically freed by TObjectList
  FStreamList.Free;
  FFilesLists.Free;
  ActiveVfsPAK := nil;
  inherited Destroy;
end;

function TGLVfsPAK.GetFileCount: integer;
begin
  Result := FHeader.DirLength div SizeOf(TFileSection);
end;

procedure TGLVfsPAK.MakeFileList;
var
  i: integer;
begin
  FStream.Seek(FHeader.DirOffset, soFromBeginning);
  FFiles.Clear;
  for i := 0 to FileCount - 1 do
  begin
    FStream.ReadBuffer(Dir, SizeOf(TFileSection));
    FFiles.Add(string(Dir.FileName));
  end;
end;

procedure TGLVfsPAK.LoadFromFile(const FileName: string; Mode: word);
var
  l: integer;
begin
  FFileName := FileName;
  FPakFiles.Clear;
  FPakFiles.Add(FileName);
  FFiles := TStringList.Create;
  FStream := TFileStream.Create(FileName, Mode);
  if FStream.Size = 0 then
  begin
    if FCompressed then
      FHeader.Signature := SIGN_COMPRESSED
    else
      FHeader.Signature := SIGN;
    FHeader.DirOffset := SizeOf(TPakHeader);
    FHeader.DirLength := 0;
    if FHeader.Signature = SIGN_COMPRESSED then
    begin
      FStream.Free;
      raise Exception.Create
        (FileName +
        ' - This is a compressed PAK file. This version of software does not support Compressed Pak files.');
      Exit;
    end;
    FStream.WriteBuffer(FHeader, SizeOf(TPakHeader));
    FStream.Position := 0;
  end;
  FStream.ReadBuffer(FHeader, SizeOf(TPakHeader));
  if (FHeader.Signature <> SIGN) and (FHeader.Signature <> SIGN_COMPRESSED) then
  begin
    FStream.Free;
    raise Exception.Create(FileName + ' - This is not PAK file');
    Exit;
  end;

  // Set the compression flag property.
  FCompressed := FHeader.Signature = SIGN_COMPRESSED;
  if FCompressed then
  begin
    FStream.Free;
    raise Exception.Create
      (FileName +
      ' - This is a compressed PAK file. This version of software does not support Compressed Pak files.');
    Exit;
  end;
  if FileCount <> 0 then
    MakeFileList;
  l := Length(FHeaderList);
  SetLength(FHeaderList, l + 1);
  FHeaderList[l] := FHeader;
  FFilesLists.Add(FFiles);
  FStreamList.Add(FStream);
end;

procedure TGLVfsPAK.ClearPakFiles;
begin
  SetLength(FHeaderList, 0);
  FPakFiles.Clear;
  // Objects are automatically freed by TObjectList
  FStreamList.Clear;
  FFilesLists.Clear;
  ActiveVfsPAK := nil;
end;

function TGLVfsPAK.GetFile(index: integer): TStream;
begin
  FStream.Seek(FHeader.DirOffset + SizeOf(TFileSection) * index,
    soFromBeginning);
  FStream.Read(Dir, SizeOf(TFileSection));
  FStream.Seek(Dir.FilePos, soFromBeginning);
  Result := TMemoryStream.Create;
  Result.CopyFrom(FStream, Dir.FileLength);
  Result.Position := 0;
end;

function TGLVfsPAK.FileExists(const FileName: string): Boolean;
begin
  Result := (FFiles.IndexOf(FileName) > -1);
end;

function TGLVfsPAK.GetFile(const FileName: string): TStream;
begin
  Result := nil;
  if Self.FileExists(FileName) then
    Result := GetFile(FFiles.IndexOf(FileName));
end;

function TGLVfsPAK.GetFileSize(index: integer): integer;
begin
  FStream.Seek(FHeader.DirOffset + SizeOf(TFileSection) * index,
    soFromBeginning);
  FStream.Read(Dir, SizeOf(Dir));
  Result := Dir.FileLength;
end;

function TGLVfsPAK.GetFileSize(const FileName: string): integer;
begin
  Result := -1;
  if Self.FileExists(FileName) then
    Result := GetFileSize(FFiles.IndexOf(FileName));
end;

{$WARNINGS OFF}

procedure TGLVfsPAK.AddFromStream(const FileName, Path: string; F: TStream);
var
  Temp: TMemoryStream;
begin
  FStream.Position := FHeader.DirOffset;
  if FHeader.DirLength > 0 then
  begin
    Temp := TMemoryStream.Create;
    Temp.CopyFrom(FStream, FHeader.DirLength);
    Temp.Position := 0;
    FStream.Position := FHeader.DirOffset;
  end;
  Dir.FilePos := FHeader.DirOffset;

  Dir.FileLength := F.Size;
  FStream.CopyFrom(F, 0);
  FHeader.DirOffset := FStream.Position;
  if FHeader.DirLength > 0 then
  begin
    FStream.CopyFrom(Temp, 0);
    Temp.Free;
  end;
  StrPCopy(Dir.FileName, Path + ExtractFileName(FileName));
  FStream.WriteBuffer(Dir, SizeOf(TFileSection));
  FHeader.DirLength := FHeader.DirLength + SizeOf(TFileSection);
  FStream.Position := 0;
  FStream.WriteBuffer(FHeader, SizeOf(TPakHeader));
  FFiles.Add(Dir.FileName);
end;

{$WARNINGS ON}

procedure TGLVfsPAK.AddFromFile(const FileName, Path: string);
var
  F: TFileStream;
begin
  if not FileExists(FileName) then
    Exit;
  F := TFileStream.Create(FileName, fmOpenRead);
  try
    AddFromStream(FileName, Path, F);
  finally
    F.Free;
  end;
end;

procedure TGLVfsPAK.AddEmptyFile(const FileName, Path: string);
var
  F: TMemoryStream;
begin
  F := TMemoryStream.Create;
  try
    AddFromStream(FileName, Path, F);
  finally
    F.Free;
  end;
end;

procedure TGLVfsPAK.RemoveFile(index: integer);
var
  Temp: TMemoryStream;
  i: integer;
  F: TFileSection;
begin
  Temp := TMemoryStream.Create;
  FStream.Seek(FHeader.DirOffset + SizeOf(TFileSection) * index,
    soFromBeginning);
  FStream.ReadBuffer(Dir, SizeOf(TFileSection));
  FStream.Seek(Dir.FilePos + Dir.FileLength, soFromBeginning);
  Temp.CopyFrom(FStream, FStream.Size - FStream.Position);
  FStream.Position := Dir.FilePos;
  FStream.CopyFrom(Temp, 0);
  FHeader.DirOffset := FHeader.DirOffset - Dir.FileLength;
  Temp.Clear;
  for i := 0 to FileCount - 1 do
    if i > index then
    begin
      FStream.Seek(FHeader.DirOffset + SizeOf(TFileSection) * i,
        soFromBeginning);
      FStream.ReadBuffer(F, SizeOf(TFileSection));
      FStream.Position := FStream.Position - SizeOf(TFileSection);
      F.FilePos := F.FilePos - Dir.FileLength;
      FStream.WriteBuffer(F, SizeOf(TFileSection));
    end;

  i := FHeader.DirOffset + SizeOf(TFileSection) * index;
  FStream.Position := i + SizeOf(TFileSection);
  if FStream.Position < FStream.Size then
  begin
    Temp.CopyFrom(FStream, FStream.Size - FStream.Position);
    FStream.Position := i;
    FStream.CopyFrom(Temp, 0);
  end;
  Temp.Free;
  FHeader.DirLength := FHeader.DirLength - SizeOf(TFileSection);
  FStream.Position := 0;
  FStream.WriteBuffer(FHeader, SizeOf(TPakHeader));
  FStream.Size := FStream.Size - Dir.FileLength - SizeOf(TFileSection);
  MakeFileList;
end;

procedure TGLVfsPAK.RemoveFile(const FileName: string);
begin
  if Self.FileExists(FileName) then
    RemoveFile(FFiles.IndexOf(FileName));
end;

procedure TGLVfsPAK.Extract(index: integer; const NewName: string);
var
  s: TFileStream;
begin
  if NewName = '' then
    Exit;
  if (index < 0) or (index >= FileCount) then
    Exit;
  s := TFileStream.Create(NewName, fmCreate);
  s.CopyFrom(GetFile(index), 0);
  s.Free;
end;

procedure TGLVfsPAK.Extract(const FileName, NewName: string);
begin
  if Self.FileExists(FileName) then
    Extract(FFiles.IndexOf(FileName), NewName);
end;

end.
