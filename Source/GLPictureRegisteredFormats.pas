//
// This unit is part of the GLScene Engine, http://glscene.org
//
(*  Hacks to access the list of TPicture registered TGraphic formats *)
unit GLPictureRegisteredFormats;

interface

{$I GLScene.inc}

uses
  System.Classes,
  Vcl.Graphics;

{$DEFINE PRF_HACK_PASSES}

{Returns the TGraphicClass associated to the extension, if any. 
   Accepts anExtension with or without the '.' }
function GraphicClassForExtension(const anExtension: string): TGraphicClass;

{Adds to the passed TStrings the list of registered formats. 
   Convention is "extension=description" for the string, the Objects hold
   the corresponding TGraphicClass (extensions do not include the '.'). }
procedure HackTPictureRegisteredFormats(destList: TStrings);

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

function GraphicClassForExtension(const anExtension: string): TGraphicClass;
var
  i: integer;
  sl: TStringList;
  buf: string;
begin
  Result := nil;
  if anExtension = '' then
    Exit;
  if anExtension[1] = '.' then
    buf := Copy(anExtension, 2, MaxInt)
  else
    buf := anExtension;
  sl := TStringList.Create;
  try
    HackTPictureRegisteredFormats(sl);
    i := sl.IndexOfName(buf);
    if i >= 0 then
      Result := TGraphicClass(sl.Objects[i]);
  finally
    sl.Free;
  end;
end;

type
  PFileFormat = ^TFileFormat;

  TFileFormat = record
    GraphicClass: TGraphicClass;
    Extension: string;
    Description: string;
    DescResID: integer;
  end;

{$ifopt R+}
  {$define HackTPictureRegisteredFormats_Disable_RangeCheck}
  {$R-}
{$endif}
procedure HackTPictureRegisteredFormats(destList: TStrings);
var
  pRegisterFileFormat, pCallGetFileFormat, pGetFileFormats, pFileFormats: PAnsiChar;
  iCall: cardinal;
  i: integer;
  list: TList;
  fileFormat: PFileFormat;
begin
  {$MESSAGE WARN 'HackTPictureRegisteredFormats will crash when Graphics.pas is compiled with the 'Use Debug DCUs' option'}

  pRegisterFileFormat := PAnsiChar(@TPicture.RegisterFileFormat);
  if pRegisterFileFormat[0] = #$FF then // in case of BPL redirector
    pRegisterFileFormat := PAnsiChar(PCardinal(PCardinal(@pRegisterFileFormat[2])^)^);
  pCallGetFileFormat := @pRegisterFileFormat[16];
  iCall := PCardinal(pCallGetFileFormat)^;
  pGetFileFormats := @pCallGetFileFormat[iCall + 4];
  pFileFormats := PAnsiChar(PCardinal(@pGetFileFormats[2])^);
  list := TList(PCardinal(pFileFormats)^);

  if list <> nil then
  begin
    for i := 0 to list.Count - 1 do
    begin
      fileFormat := PFileFormat(list[i]);
      destList.AddObject(fileFormat.Extension + '=' + fileFormat.Description,
        TObject(fileFormat.GraphicClass));
    end;
  end;
end;

{$ifdef HackTPictureRegisteredFormats_Disable_RangeCheck}
  {$R+}
  {$undef HackTPictureRegisteredFormats_Disable_RangeCheck}
{$endif}

end.


