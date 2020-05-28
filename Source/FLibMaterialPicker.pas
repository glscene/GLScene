//
// This unit is part of the GLScene Engine, http://glscene.org
//
{
 Allows choosing a material in a material library 
}
unit FLibMaterialPicker;

interface

{$I GLScene.inc}

uses
  System.Classes, 
  VCL.Forms,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Controls,
  GLWin32Viewer,
  GLMaterial,
  FRMaterialPreview;

type
  TGLLibMaterialPicker = class(TForm)
    LBMaterials: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    BBOk: TBitBtn;
    BBCancel: TBitBtn;
    MPPreview: TRMaterialPreview;
    procedure LBMaterialsClick(Sender: TObject);
    procedure LBMaterialsKeyPress(Sender: TObject; var Key: Char);
    procedure LBMaterialsDblClick(Sender: TObject);
  private
  public
    function Execute(var materialName: TGLLibMaterialName;
      materialLibrary: TGLAbstractMaterialLibrary): Boolean;
  end;

function GLLibMaterialPicker: TGLLibMaterialPicker;
procedure ReleaseLibMaterialPicker;

//-------------------------------------------------
implementation
//-------------------------------------------------

{$R *.dfm}

var
  vGLLibMaterialPicker: TGLLibMaterialPicker;

function GLLibMaterialPicker: TGLLibMaterialPicker;
begin
  if not Assigned(vGLLibMaterialPicker) then
    vGLLibMaterialPicker := TGLLibMaterialPicker.Create(nil);
  Result := vGLLibMaterialPicker;
end;

procedure ReleaseLibMaterialPicker;
begin
  if Assigned(vGLLibMaterialPicker) then
  begin
    vGLLibMaterialPicker.Free;
    vGLLibMaterialPicker := nil;
  end;
end;


function TGLLibMaterialPicker.Execute(var materialName: TGLLibMaterialName;
  materialLibrary: TGLAbstractMaterialLibrary): Boolean;
begin
  with LBMaterials do
  begin
    materialLibrary.SetNamesToTStrings(LBMaterials.Items);
    ItemIndex := Items.IndexOf(materialName);
    if (ItemIndex < 0) and (Items.Count > 0) then
      ItemIndex := 0;
    BBOk.Enabled := (Items.Count > 0);
  end;
  LBMaterialsClick(Self);
  Result := (ShowModal = mrOk);
  if Result then
  begin
    with LBMaterials do
      if ItemIndex >= 0 then
        materialName := Items[ItemIndex]
      else
        materialName := '';
  end;
end;

procedure TGLLibMaterialPicker.LBMaterialsClick(Sender: TObject);
begin
  with LBMaterials do
    if ItemIndex >= 0 then
      MPPreview.LibMaterial := TGLAbstractLibMaterial(Items.Objects[ItemIndex]);
end;

procedure TGLLibMaterialPicker.LBMaterialsKeyPress(Sender: TObject;
  var Key: Char);
begin
  LBMaterialsClick(Sender);
end;

procedure TGLLibMaterialPicker.LBMaterialsDblClick(Sender: TObject);
begin
  BBOk.Click;
end;

//-----------------------------------------------------------------
initialization
//-----------------------------------------------------------------

finalization
  ReleaseLibMaterialPicker;

end.

