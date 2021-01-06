//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.FilePAK;

(* Methods for PAK Archiving *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
  GLS.ArchiveManager;

const
   SIGN = 'PACK';

type

   TPakHeader = record
      Signature: array[0..3] of AnsiChar;
      DirOffset: integer;
      DirLength: integer;
   end;

   TFileSection = record
      FileName: array[0..119] of AnsiChar;
      FilePos: integer;
      FileLength: integer;
   end;

  TPAKArchive=class(TGLBaseArchive)
    private
      FHeader: TPakHeader;
      FStream: TFileStream;
      function GetContentCount: integer;
      procedure MakeContentList;
    protected
      Procedure SetCompressionLevel(aValue: TCompressionLevel); override;
    public
     property ContentCount: integer Read GetContentCount;

      procedure LoadFromFile(const FileName: string); override;
      procedure Clear; override;

      function ContentExists(ContentName: string): boolean; override;

      function GetContent(Stream: TStream; index: integer): TStream;  override;
      function GetContent(index: integer): TStream;  override;
      function GetContent(ContentName: string): TStream;  override;

      function GetContentSize(index: integer): integer; override;
      function GetContentSize(ContentName: string): integer; override;

      procedure AddFromStream(ContentName, Path: string; FS: TStream); override;
      procedure AddFromFile(FileName, Path: string); override;

      procedure RemoveContent(index: integer); override;
      procedure RemoveContent(ContentName: string); override;

      procedure Extract(index: integer; NewName: string); override;
      procedure Extract(ContentName, NewName: string); override;
  end;

//-----------------------------------------------------------
implementation
//-----------------------------------------------------------

var
   Dir: TFileSection;

//-------------------------------
// TPAKArchive 
//-------------------------------

function TPAKArchive.GetContentCount: integer;
begin
   Result := FHeader.DirLength div SizeOf(TFileSection);
end;

procedure TPAKArchive.MakeContentList;
var
   I: integer;
begin
   FStream.Seek(FHeader.DirOffset, soFromBeginning);
   FContentList.Clear;
   for i := 0 to ContentCount - 1 do
   begin
      FStream.ReadBuffer(Dir, SizeOf(TFileSection));
      FContentList.Add(string(Dir.FileName));
   end;
end;

procedure TPAKArchive.SetCompressionLevel(aValue: TCompressionLevel);
begin
  aValue := clNone;
  inherited SetCompressionLevel(aValue);
end;

procedure TPAKArchive.LoadFromFile(const FileName: string);
begin
   FFileName := FileName;
    FStream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite);
    //?????????? ?????
    If (FStream = nil) then exit;


    if FStream.Size = 0 then
   begin
     FHeader.Signature := SIGN;
     FHeader.DirOffset := SizeOf(TPakHeader);
     FHeader.DirLength := 0;
     FStream.WriteBuffer(FHeader, SizeOf(TPakHeader));
     FStream.Position := 0;
   end;

   FStream.ReadBuffer(FHeader, SizeOf(TPakHeader));
   if (FHeader.Signature <> SIGN)  then
   begin
      FreeAndNil(FStream); // nil it too to avoid own Clear() giving AV
      raise Exception.Create(FileName+' - This is not PAK file');
      Exit;
   end;
   if ContentCount <> 0 then
      MakeContentList;
end;

procedure TPAKArchive.Clear;
begin
   If FStream <> nil then FStream.Free;
   FContentList.Clear;
end;

function TPAKArchive.ContentExists(ContentName: string): boolean;
begin
   Result := (FContentList.IndexOf(ContentName) > -1);
end;

function TPAKArchive.GetContent(Stream: TStream; index: integer): TStream;
begin
      FStream.Seek(FHeader.DirOffset + SizeOf(TFileSection) * index, soFromBeginning);
      FStream.Read(Dir, SizeOf(TFileSection));
      FStream.Seek(Dir.FilePos, soFromBeginning);
      Result := Stream;
      Result.CopyFrom(FStream, Dir.FileLength);
      Result.Position := 0;
end;

function TPAKArchive.GetContent(index: integer): TStream;
begin
   Result:=GetContent(TMemoryStream.Create, index);
end;

function TPAKArchive.GetContent(ContentName: string): TStream;
begin
   Result := nil;
   if ContentExists(ContentName) then
      Result := GetContent(FContentList.IndexOf(ContentName));
end;

function TPAKArchive.GetContentSize(index: integer): integer;
begin
   FStream.Seek(FHeader.DirOffset + SizeOf(TFileSection) * index, soFromBeginning);
   FStream.Read(Dir, SizeOf(Dir));
   Result := Dir.FileLength;
end;

function TPAKArchive.GetContentSize(ContentName: string): integer;
begin
   Result := -1;
   if ContentExists(ContentName) then
      Result := GetContentSize(FContentList.IndexOf(ContentName));
end;

procedure TPAKArchive.AddFromStream(ContentName, Path: string; FS: TStream);
var
   Temp: TMemoryStream;
begin
      //?????????? ?????
   If (FStream = nil) or ContentExists(ContentName) then exit;
   Temp := nil;
   FStream.Position := FHeader.DirOffset;
   if FHeader.DirLength > 0 then
   begin
      Temp := TMemoryStream.Create;
      Temp.CopyFrom(FStream, FHeader.DirLength);
      Temp.Position    := 0;
      FStream.Position := FHeader.DirOffset;
   end;
   Dir.FilePos    := FHeader.DirOffset;
   Dir.FileLength := FS.Size;
   FStream.CopyFrom(FS, 0);

   FHeader.DirOffset := FStream.Position;
   if FHeader.DirLength > 0 then
   begin
      FStream.CopyFrom(Temp, 0);
      Temp.Free;
   end;
   StrPCopy(Dir.FileName, AnsiString(Path + ExtractFileName(ContentName)));
   FStream.WriteBuffer(Dir, SizeOf(TFileSection));
   FHeader.DirLength := FHeader.DirLength + SizeOf(TFileSection);
   FStream.Position  := 0;
   FStream.WriteBuffer(FHeader, SizeOf(TPakHeader));
   FContentList.Add(string(Dir.FileName));
end;

procedure TPAKArchive.AddFromFile(FileName, Path: string);
var
   FS: TFileStream;
begin
   if not FileExists(FileName) then
      exit;
   FS := TFileStream.Create(FileName, fmOpenRead);
   try
      AddFromStream(FileName, Path, FS);
   finally
      FS.Free;
   end;
end;

procedure TPAKArchive.RemoveContent(index: integer);
var
   Temp: TMemoryStream;
   i:    integer;
   f:    TFileSection;
begin
   Temp := TMemoryStream.Create;
   FStream.Seek(FHeader.DirOffset + SizeOf(TFileSection) * index, soFromBeginning);
   FStream.ReadBuffer(Dir, SizeOf(TFileSection));
   FStream.Seek(Dir.FilePos + Dir.FileLength, soFromBeginning);
   Temp.CopyFrom(FStream, FStream.Size - FStream.Position);
   FStream.Position := Dir.FilePos;
   FStream.CopyFrom(Temp, 0);
   FHeader.DirOffset := FHeader.DirOffset - dir.FileLength;
   Temp.Clear;
   for i := 0 to ContentCount - 1 do
      if i > index then
      begin
         FStream.Seek(FHeader.DirOffset + SizeOf(TFileSection) * i, soFromBeginning);
         FStream.ReadBuffer(f, SizeOf(TFileSection));
         FStream.Position := FStream.Position - SizeOf(TFileSection);
         f.FilePos := f.FilePos - dir.FileLength;
         FStream.WriteBuffer(f, SizeOf(TFileSection));
      end;

   i := FHeader.DirOffset + SizeOf(TFileSection) * index;
   FStream.Position := Cardinal(i + SizeOf(TFileSection));
   if FStream.Position < FStream.Size then
   begin
      Temp.CopyFrom(FStream, FStream.Size - FStream.Position);
      FStream.Position := i;
      FStream.CopyFrom(Temp, 0);
   end;
   Temp.Free;
   FHeader.DirLength := FHeader.DirLength - SizeOf(TFileSection);
   FStream.Position  := 0;
   FStream.WriteBuffer(FHeader, SizeOf(TPakHeader));
   FStream.Size := FStream.Size - dir.FileLength - SizeOf(TFileSection);
   MakeContentList;
end;

procedure TPAKArchive.RemoveContent(ContentName: string);
begin
   if ContentExists(ContentName) then
      RemoveContent(FContentList.IndexOf(ContentName));
end;

procedure TPAKArchive.Extract(index: integer; NewName: string);
var
   vExtractFileStream: TFileStream;
   vTmpStream: Tstream;
begin
   if NewName = '' then
      Exit;
   if (index < 0) or (index >= ContentCount) then
      exit;
   vExtractFileStream := TFileStream.Create(NewName, fmCreate);
   vTmpStream := GetContent(index);
   vExtractFileStream.CopyFrom(vTmpStream, 0);
   vTmpStream.Free;
   vExtractFileStream.Free;
end;

procedure TPAKArchive.Extract(ContentName, NewName: string);
begin
   if ContentExists(ContentName) then
      Extract(FContentList.IndexOf(ContentName), NewName);
end;

//----------------------------------------
initialization
//----------------------------------------

RegisterArchiveFormat('pak','PAK File',TPAKArchive);

end.
