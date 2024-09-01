//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.TextureImageEditors;

(* Standard texture image editors for standard texture image classes *)

interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  System.SysUtils,

  GXS.Texture,
  GXS.ProcTextures,
  GXS.Utils;

type

  TgxTextureImageEditor = class(TObject)
  public
    (* Request to edit a textureImage.
      Returns True if changes have been made.
      This method may be invoked from the IDE or at run-time. *)
    class function Edit(aTexImage: TgxTextureImage): Boolean; virtual; abstract;
  end;

  TgxTextureImageEditorClass = class of TgxTextureImageEditor;

  TgxBlankTIE = class(TgxTextureImageEditor)
  public
    class function Edit(aTexImage: TgxTextureImage): Boolean; override;
  end;

  TgxPersistentTIE = class(TgxTextureImageEditor)
  public
    class function Edit(aTexImage: TgxTextureImage): Boolean; override;
  end;

  TgxPicFileTIE = class(TgxTextureImageEditor)
  public
    class function Edit(aTexImage: TgxTextureImage): Boolean; override;
  end;

  TgxProcTextureNoiseTIE = class(TgxTextureImageEditor)
  public
    class function Edit(aTexImage: TgxTextureImage): Boolean; override;
  end;

  // Invokes the editor for the given TgxTextureImage
function EditTextureImage(aTexImage: TgxTextureImage): Boolean;
procedure RegisterTextureImageEditor(aTexImageClass: TgxTextureImageClass;
  texImageEditor: TgxTextureImageEditorClass);
procedure UnRegisterTextureImageEditor(texImageEditor
  : TgxTextureImageEditorClass);

// ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------

var
  vTIEClass, vTIEEditor: TList;

function EditTextureImage(aTexImage: TgxTextureImage): Boolean;
var
  i: Integer;
  editor: TgxTextureImageEditorClass;
begin
  if Assigned(vTIEClass) then
  begin
    i := vTIEClass.IndexOf(Pointer(aTexImage.ClassType));
    if i >= 0 then
    begin
      editor := TgxTextureImageEditorClass(vTIEEditor[i]);
      Result := editor.Edit(aTexImage);
      Exit;
    end;
  end;
  InformationDlg(aTexImage.ClassName + ': editing not supported.');
  Result := False;
end;

procedure RegisterTextureImageEditor(aTexImageClass: TgxTextureImageClass;
  texImageEditor: TgxTextureImageEditorClass);
begin
  if not Assigned(vTIEClass) then
  begin
    vTIEClass := TList.Create;
    vTIEEditor := TList.Create;
  end;
  vTIEClass.Add(Pointer(aTexImageClass));
  vTIEEditor.Add(texImageEditor);
end;

procedure UnRegisterTextureImageEditor(texImageEditor
  : TgxTextureImageEditorClass);
var
  i: Integer;
begin
  if Assigned(vTIEClass) then
  begin
    i := vTIEEditor.IndexOf(texImageEditor);
    if i >= 0 then
    begin
      vTIEClass.Delete(i);
      vTIEEditor.Delete(i);
    end;
  end;
end;

// ------------------
// ------------------ TgxBlankTIE ------------------
// ------------------

class function TgxBlankTIE.Edit(aTexImage: TgxTextureImage): Boolean;
var
  p1, p2: Integer;
  buf, part: String;
  texImage: TgxBlankImage;
begin
  texImage := (aTexImage as TgxBlankImage);
  if texImage.Depth = 0 then
    buf := InputDlg('Blank Image', 'Enter size',
      Format('%d x %d', [texImage.Width, texImage.Height]))
  else
    buf := InputDlg('Blank Image', 'Enter size',
      Format('%d x %d x %d', [texImage.Width, texImage.Height,
      texImage.Depth]));

  p1 := Pos('x', buf);
  if p1 > 0 then
  begin
    texImage.Width := StrToIntDef(Trim(Copy(buf, 1, p1 - 1)), 256);
    part := Copy(buf, p1 + 1, MaxInt);
    p2 := Pos('x', part);
    if p2 > 0 then
    begin
      texImage.Height := StrToIntDef(Trim(Copy(part, 1, p2 - 1)), 256);
      texImage.Depth := StrToIntDef(Trim(Copy(part, p2 + 1, MaxInt)), 1)
    end
    else
    begin
      texImage.Height := StrToIntDef(Trim(Copy(buf, p1 + 1, MaxInt)), 256);
      texImage.Depth := 0;
    end;
    Result := True;
  end
  else
  begin
    InformationDlg('Invalid size');
    Result := False;
  end;
end;

// ------------------
// ------------------ TgxPersistentTIE ------------------
// ------------------

class function TgxPersistentTIE.Edit(aTexImage: TgxTextureImage): Boolean;
var
  fName: String;
begin
  fName := '';
  Result := OpenPictureDialog(fName);
  if Result then
  begin
    aTexImage.LoadFromFile(fName);
    aTexImage.NotifyChange(aTexImage);
  end;
end;

// ------------------
// ------------------ TgxPicFileTIE ------------------
// ------------------

class function TgxPicFileTIE.Edit(aTexImage: TgxTextureImage): Boolean;
var
  newName: String;
  texImage: TgxPicFileImage;
begin
  { TODO : A better TgxPicFileImage.Edit is needed... }
  texImage := (aTexImage as TgxPicFileImage);
  newName := InputDlg('PicFile Image', 'Enter filename',
    texImage.PictureFileName);
  Result := (texImage.PictureFileName <> newName);
  if Result then
    texImage.PictureFileName := newName
end;

class function TgxProcTextureNoiseTIE.Edit(aTexImage: TgxTextureImage): Boolean;
var
  p: Integer;
  buf: String;
begin
  with aTexImage as TgxProcTextureNoise do
  begin
    buf := InputDlg(TgxProcTextureNoise.FriendlyName, 'Enter size',
      Format('%d x %d', [Width, Height]));
    p := Pos('x', buf);
    if p > 0 then
    begin
      Width := StrToIntDef(Trim(Copy(buf, 1, p - 1)), 256);
      Height := StrToIntDef(Trim(Copy(buf, p + 1, MaxInt)), 256);
      buf := InputDlg(TgxProcTextureNoise.FriendlyName, 'Minimum Cut',
        IntToStr(MinCut));
      MinCut := StrToIntDef(buf, 0);
      buf := InputDlg(TgxProcTextureNoise.FriendlyName, 'Noise Sharpness',
        FloatToStr(NoiseSharpness));
      NoiseSharpness := GXS.Utils.StrToFloatDef(buf, 0.9);
      buf := InputDlg(TgxProcTextureNoise.FriendlyName, 'Random Seed',
        IntToStr(NoiseRandSeed));
      NoiseRandSeed := StrToIntDef(buf, 0);
      RandSeed := NoiseRandSeed;
      buf := InputDlg(TgxProcTextureNoise.FriendlyName,
        'Generate Seamless Texture (0,1)', IntToStr(Ord(Seamless)));
      Seamless := (buf <> '0');
      Result := True;
      Invalidate;
    end
    else
    begin
      InformationDlg('Invalid size');
      Result := False;
    end;
  end;
end;

// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------

RegisterTextureImageEditor(TgxBlankImage, TgxBlankTIE);
RegisterTextureImageEditor(TgxPersistentImage, TgxPersistentTIE);
RegisterTextureImageEditor(TgxPicFileImage, TgxPicFileTIE);
RegisterTextureImageEditor(TgxProcTextureNoise, TgxProcTextureNoiseTIE);

finalization

UnRegisterTextureImageEditor(TgxBlankTIE);
UnRegisterTextureImageEditor(TgxPersistentTIE);
UnRegisterTextureImageEditor(TgxPicFileTIE);
UnRegisterTextureImageEditor(TgxProcTextureNoiseTIE);

FreeAndNil(vTIEClass);
FreeAndNil(vTIEEditor);

end.
