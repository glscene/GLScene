//
// The graphics rendering engine GLScene http://glscene.org
//
unit FmMaterialEditor;

(*  Editor window for a material (with preview) *)

interface

{$I GLScene.inc}

uses
  Winapi.Windows,
  System.Classes,
  System.TypInfo,
  VCL.Forms,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.Controls,
  VCL.Buttons,
  GLS.SceneViewer,
  GLS.State,
  GLS.Material,
  GLS.Texture,
  FRMaterialPreview,
  FRColorEditor,
  FRFaceEditor,
  FRTextureEdit;

type
  TGLMaterialEditorForm = class(TForm)
    PageControl1: TPageControl;
    TSFront: TTabSheet;
    TSBack: TTabSheet;
    TSTexture: TTabSheet;
    FEFront: TRFaceEditor;
    FEBack: TRFaceEditor;
    GroupBox1: TGroupBox;
    MPPreview: TRMaterialPreview;
    BBOk: TBitBtn;
    BBCancel: TBitBtn;
    RTextureEdit: TRTextureEdit;
    CBBlending: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    CBPolygonMode: TComboBox;
    procedure OnMaterialChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    function Execute(AMaterial: TGLMaterial): Boolean;
  end;

function GLMaterialEditorForm: TGLMaterialEditorForm;
procedure ReleaseMaterialEditorForm;

//----------------------------------------------
implementation
//----------------------------------------------

{$R *.dfm}

var
  vGLMaterialEditorForm: TGLMaterialEditorForm;

function GLMaterialEditorForm: TGLMaterialEditorForm;
begin
  if not Assigned(vGLMaterialEditorForm) then
    vGLMaterialEditorForm := TGLMaterialEditorForm.Create(nil);
  Result := vGLMaterialEditorForm;
end;

procedure ReleaseMaterialEditorForm;
begin
  if Assigned(vGLMaterialEditorForm) then
  begin
    vGLMaterialEditorForm.Free;
    vGLMaterialEditorForm := nil;
  end;
end;


constructor TGLMaterialEditorForm.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  for i := 0 to Integer(High(TGLBlendingMode)) do
    CBBlending.Items.Add(GetEnumName(TypeInfo(TGLBlendingMode), i));
  for i := 0 to Integer(High(TGLPolygonMode)) do
    CBPolygonMode.Items.Add(GetEnumName(TypeInfo(TGLPolygonMode), i));

  FEFront.OnChange := OnMaterialChanged;
  FEBack.OnChange := OnMaterialChanged;
  RTextureEdit.OnChange := OnMaterialChanged;
end;

function TGLMaterialEditorForm.Execute(AMaterial: TGLMaterial): Boolean;
begin
  with AMaterial.GetActualPrimaryMaterial do
  begin
    FEFront.FaceProperties := FrontProperties;
    FEBack.FaceProperties := BackProperties;
    RTextureEdit.Texture := Texture;
    CBPolygonMode.ItemIndex:=Integer(PolygonMode);
    CBBlending.ItemIndex := Integer(BlendingMode);
  end;
  MPPreview.Material := AMaterial;
  Result := (ShowModal = mrOk);
  if Result then
    with AMaterial.GetActualPrimaryMaterial do
    begin
      FrontProperties := FEFront.FaceProperties;
      BackProperties := FEBack.FaceProperties;
      Texture := RTextureEdit.Texture;
      BlendingMode := TGLBlendingMode(CBBlending.ItemIndex);
      PolygonMode := TGLPolygonMode(CBPolygonMode.ItemIndex);
    end;
end;

procedure TGLMaterialEditorForm.OnMaterialChanged(Sender: TObject);
begin
  with MPPreview.Material do
  begin
    FrontProperties := FEFront.FaceProperties;
    BackProperties := FEBack.FaceProperties;
    Texture := RTextureEdit.Texture;
    BlendingMode := TGLBlendingMode(CBBlending.ItemIndex);
    PolygonMode := TGLPolygonMode(CBPolygonMode.ItemIndex);
  end;
  MPPreview.GLSceneViewer.Invalidate;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

finalization

  ReleaseMaterialEditorForm;

end.

