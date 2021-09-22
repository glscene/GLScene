//
// The graphics rendering engine GLScene http://glscene.org
//
unit FmLibMaterialPicker;

(* Allows choosing a material in a material library *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  VCL.Forms,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Controls,
  GLS.SceneViewer,
  GLS.Material,
  FRMaterialPreview;

type
  TGLLibMaterialPickerForm = class(TForm)
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

function GLLibMaterialPickerForm: TGLLibMaterialPickerForm;
procedure ReleaseLibMaterialPickerForm;

//-------------------------------------------------
implementation
//-------------------------------------------------

{$R *.dfm}

var
  vGLLibMaterialPickerForm: TGLLibMaterialPickerForm;

function GLLibMaterialPickerForm: TGLLibMaterialPickerForm;
begin
  if not Assigned(vGLLibMaterialPickerForm) then
    vGLLibMaterialPickerForm := TGLLibMaterialPickerForm.Create(nil);
  Result := vGLLibMaterialPickerForm;
end;

procedure ReleaseLibMaterialPickerForm;
begin
  if Assigned(vGLLibMaterialPickerForm) then
  begin
    vGLLibMaterialPickerForm.Free;
    vGLLibMaterialPickerForm := nil;
  end;
end;


function TGLLibMaterialPickerForm.Execute(var materialName: TGLLibMaterialName;
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

procedure TGLLibMaterialPickerForm.LBMaterialsClick(Sender: TObject);
begin
  with LBMaterials do
    if ItemIndex >= 0 then
      MPPreview.LibMaterial := TGLAbstractLibMaterial(Items.Objects[ItemIndex]);
end;

procedure TGLLibMaterialPickerForm.LBMaterialsKeyPress(Sender: TObject;
  var Key: Char);
begin
  LBMaterialsClick(Sender);
end;

procedure TGLLibMaterialPickerForm.LBMaterialsDblClick(Sender: TObject);
begin
  BBOk.Click;
end;

//-----------------------------------------------------------------
initialization
//-----------------------------------------------------------------

finalization

 ReleaseLibMaterialPickerForm;

end.

