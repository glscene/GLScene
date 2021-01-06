//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.TextureImageEditors;

(* Standard texture image editors for standard texture image classes. *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,

  GLS.Texture,
  GLS.ProcTextures;

type

  TGLTextureImageEditor = class(TObject)
  public
    (* Request to edit a textureImage.
      Returns True if changes have been made.
      This method may be invoked from the IDE or at run-time. *)
    class function Edit(aTexImage: TGLTextureImage): Boolean; virtual;
  end;

  TGLTextureImageEditorClass = class of TGLTextureImageEditor;

  TGLBlankTIE = class(TGLTextureImageEditor)
  public
    class function Edit(aTexImage: TGLTextureImage): Boolean; override;
  end;

  TGLPersistentTIE = class(TGLTextureImageEditor)
  public
    class function Edit(aTexImage: TGLTextureImage): Boolean; override;
  end;

  TGLPicFileTIE = class(TGLTextureImageEditor)
  public
    class function Edit(aTexImage: TGLTextureImage): Boolean; override;
  end;

  TGLProcTextureNoiseTIE = class(TGLTextureImageEditor)
  public
    class function Edit(aTexImage: TGLTextureImage): Boolean; override;
  end;

// Invokes the editor for the given TGLTextureImage
function EditGLTextureImage(aTexImage: TGLTextureImage): Boolean;
procedure RegisterGLTextureImageEditor(aTexImageClass: TGLTextureImageClass; 
  texImageEditor: TGLTextureImageEditorClass);
procedure UnRegisterGLTextureImageEditor(texImageEditor: TGLTextureImageEditorClass);

// ------------------------------------------------------------------------------
implementation
// ------------------------------------------------------------------------------

uses
  GLS.Utils;

var
  vTIEClass, vTIEEditor: TList;

class function TGLTextureImageEditor.Edit(ATexImage: TGLTextureImage): Boolean;
begin
  Result := True;
end;

function EditGLTextureImage(ATexImage: TGLTextureImage): Boolean;
var
  i: Integer;
  editor: TGLTextureImageEditorClass;
begin
  if Assigned(vTIEClass) then
  begin
    i := vTIEClass.IndexOf(Pointer(aTexImage.ClassType));
    if i >= 0 then
    begin
      editor := TGLTextureImageEditorClass(vTIEEditor[i]);
      Result := editor.Edit(aTexImage);
      Exit;
    end;
  end;
  InformationDlg(aTexImage.ClassName + ': editing not supported.');
  Result := False;
end;

procedure RegisterGLTextureImageEditor(ATexImageClass: TGLTextureImageClass; 
  texImageEditor: TGLTextureImageEditorClass);
begin
  if not Assigned(vTIEClass) then
  begin
    vTIEClass := TList.Create;
    vTIEEditor := TList.Create;
  end;
  vTIEClass.Add(Pointer(aTexImageClass));
  vTIEEditor.Add(texImageEditor);
end;

procedure UnRegisterGLTextureImageEditor(TexImageEditor: TGLTextureImageEditorClass);
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
// ------------------ TGLBlankTIE ------------------
// ------------------

class function TGLBlankTIE.Edit(ATexImage: TGLTextureImage): Boolean;
var
  p1, p2: Integer;
  buf, part: String;
  texImage: TGLBlankImage;
begin
  texImage := (aTexImage as TGLBlankImage);
  if texImage.Depth = 0 then
    buf := InputDlg('Blank Image', 'Enter size', Format('%d x %d', [texImage.Width, texImage.Height]))
  else
    buf := InputDlg('Blank Image', 'Enter size', Format('%d x %d x %d', [texImage.Width, texImage.Height, texImage.Depth]));

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
// ------------------ TGLPersistentTIE ------------------
// ------------------

class function TGLPersistentTIE.Edit(aTexImage: TGLTextureImage): Boolean;
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
// ------------------ TGLPicFileTIE ------------------
// ------------------

class function TGLPicFileTIE.Edit(aTexImage: TGLTextureImage): Boolean;
var
  newName: String;
  texImage: TGLPicFileImage;
begin
  { TODO : A better TGLPicFileImage.Edit is needed... }
  texImage := (aTexImage as TGLPicFileImage);
  newName := InputDlg('PicFile Image', 'Enter filename', texImage.PictureFileName);
  Result := (texImage.PictureFileName <> newName);
  if Result then
    texImage.PictureFileName := newName
end;

class function TGLProcTextureNoiseTIE.Edit(aTexImage: TGLTextureImage): Boolean;
var
  p: Integer;
  buf: String;
begin
  with aTexImage as TGLProcTextureNoise do
  begin
    buf := InputDlg(TGLProcTextureNoise.FriendlyName, 'Enter size', Format('%d x %d', [Width, Height]));
    p := Pos('x', buf);
    if p > 0 then
    begin
      Width := StrToIntDef(Trim(Copy(buf, 1, p - 1)), 256);
      Height := StrToIntDef(Trim(Copy(buf, p + 1, MaxInt)), 256);
      buf := InputDlg(TGLProcTextureNoise.FriendlyName, 'Minimum Cut', IntToStr(MinCut));
      MinCut := StrToIntDef(buf, 0);
      buf := InputDlg(TGLProcTextureNoise.FriendlyName, 'Noise Sharpness', FloatToStr(NoiseSharpness));
      NoiseSharpness := StrToFloatDef(buf, 0.9);
      buf := InputDlg(TGLProcTextureNoise.FriendlyName, 'Random Seed', IntToStr(NoiseRandSeed));
      NoiseRandSeed := StrToIntDef(buf, 0);
      RandSeed := NoiseRandSeed;
      buf := InputDlg(TGLProcTextureNoise.FriendlyName, 'Generate Seamless Texture (0,1)', IntToStr(Ord(Seamless)));
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

RegisterGLTextureImageEditor(TGLBlankImage, TGLBlankTIE);
RegisterGLTextureImageEditor(TGLPersistentImage, TGLPersistentTIE);
RegisterGLTextureImageEditor(TGLPicFileImage, TGLPicFileTIE);
RegisterGLTextureImageEditor(TGLProcTextureNoise, TGLProcTextureNoiseTIE);

finalization

UnRegisterGLTextureImageEditor(TGLBlankTIE);
UnRegisterGLTextureImageEditor(TGLPersistentTIE);
UnRegisterGLTextureImageEditor(TGLPicFileTIE);
UnRegisterGLTextureImageEditor(TGLProcTextureNoiseTIE);

FreeAndNil(vTIEClass);
FreeAndNil(vTIEEditor);

end.
