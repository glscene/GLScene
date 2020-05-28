// DXPConfig
{
   FPC Globals for DXPExpert.<p>

   Licensed under MPL (http://www.mozilla.org/MPL/)

   Copyright 2003 - Eric Grange
}
unit DXPConfig;

interface

uses Classes;

type

   // TDXPConfig
   //
   TDXPConfig = class (TObject)
      protected
         { Protected Declarations }
         procedure WriteString(aStream : TStream; const aString : String);
         function ReadString(aStream : TStream) : String;

      public
         { Public Declarations }
         constructor Create; virtual;

         procedure SaveToStream(aStream : TStream); virtual; abstract;
         procedure LoadFromStream(aStream : TStream); virtual; abstract;

         procedure SaveToFile(const fileName : String);
         procedure LoadFromFile(const fileName : String);
   end;

// -----------------------------------------------------------------
// -----------------------------------------------------------------
// -----------------------------------------------------------------
implementation
// -----------------------------------------------------------------
// -----------------------------------------------------------------
// -----------------------------------------------------------------

uses SysUtils;

// Create
//
constructor TDXPConfig.Create;
begin
   inherited Create;
end;

// SaveToFile
//
procedure TDXPConfig.SaveToFile(const fileName : String);
var
   fs : TFileStream;
begin
   fs:=TFileStream.Create(fileName, fmCreate);
   try
      WriteString(fs, '#');
      WriteString(fs, '# DXP Config File');
      WriteString(fs, '#');
      WriteString(fs, '');
      SaveToStream(fs);
   finally
      fs.Free;
   end;
end;

// LoadFromFile
//
procedure TDXPConfig.LoadFromFile(const fileName : String);
var
   fs : TFileStream;
begin
   fs:=TFileStream.Create(fileName, fmOpenRead+fmShareDenyWrite);
   try
      ReadString(fs);
      ReadString(fs);
      ReadString(fs);
      ReadString(fs);
      LoadFromStream(fs);
   finally
      fs.Free;
   end;
end;

// WriteString
//
procedure TDXPConfig.WriteString(aStream : TStream; const aString : String);
const
   cCRLF : array [0..1] of Byte = ($0D, $0A);
begin
   if aString<>'' then
      aStream.Write(aString[1], Length(aString));
   aStream.Write(cCRLF, 2);
end;

// ReadString
//
function TDXPConfig.ReadString(aStream : TStream) : String;
var
   buf : Char;
begin
   // ugly, I know, feel free to improve
   Result:='';
   while aStream.Position<aStream.Size do begin
      aStream.Read(buf, 1);
      Result:=Result+buf;
      if Copy(Result, Length(Result)-1, 2)=#13#10 then begin
         SetLength(Result, Length(Result)-2);
         Break;
      end;
   end;
end;

end.

