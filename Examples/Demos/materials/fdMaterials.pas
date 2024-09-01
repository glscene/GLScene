unit fdMaterials;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,

  fCubemapD,
  fCustomquadD,
  fDynTextureD,
  fDynCubemapD,
  fMatScriptD,
  fMirrorD,
  fMultiMaterialD,
  fMultiPassD,
  fMultiTextureD,
  fObjMaterialD,
  fProcCloudsD,
  fTexAnimD,
  fTexCombineD,
  fTexFormatD,
  fTransparAdvD,
  fTransparencyD;

type
  TFormMaterials = class(TForm)
    tvMaterials: TTreeView;
    procedure tvMaterialsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;

var
  FormMaterials: TFormMaterials;

implementation //--------------------------------------------------------------

{$R *.dfm}

procedure TFormMaterials.FormShow(Sender: TObject);
begin
  tvMaterials.Select(tvMaterials.Items[0]);  // goto to column 0
  tvMaterials.Items[0].DropHighlighted := True;
  tvMaterialsClick(Self);
end;

procedure TFormMaterials.tvMaterialsClick(Sender: TObject);
begin
  tvMaterials.Items[0].DropHighlighted := False;
  case tvMaterials.Selected.Index of
    0:
      begin // Cubemap
        FormCubemap := TFormCubemap.Create(FormMaterials);
        FormCubemap.Parent := FormMaterials;
        FormCubemap.Align := alClient;
        FormCubemap.BorderStyle := bsNone;
        FormCubemap.Show;
      end;
    1:
      begin // Customquad
        FormCustomquad := TFormCustomquad.Create(FormMaterials);
        FormCustomquad.Parent := FormMaterials;
        FormCustomquad.Align := alClient;
        FormCustomquad.BorderStyle := bsNone;
        FormCustomquad.Show;
      end;
    2:
      begin // FormDynamictexture
        FormDynamicTexture := TFormDynamicTexture.Create(FormMaterials);
        FormDynamicTexture.Parent := FormMaterials;
        FormDynamicTexture.Align := alClient;
        FormDynamicTexture.BorderStyle := bsNone;
        FormDynamicTexture.Show;
      end;
    3:
      begin // DynCubemap
        FormDynCubemap := TFormDynCubemap.Create(FormMaterials);
        FormDynCubemap.Parent := FormMaterials;
        FormDynCubemap.Align := alClient;
        FormDynCubemap.BorderStyle := bsNone;
        FormDynCubemap.Show;
      end;
    4:
      begin
        // MatScript
        FormMatScript := TFormMatScript.Create(FormMaterials);
        FormMatScript.Parent := FormMaterials;
        FormMatScript.Align := alClient;
        FormMatScript.BorderStyle := bsNone;
        FormMatScript.Show;
      end;
    5:
      begin
        // Mirror
        FormMirror := TFormMirror.Create(FormMaterials);
        FormMirror.Parent := FormMaterials;
        FormMirror.Align := alClient;
        FormMirror.BorderStyle := bsNone;
        FormMirror.Show;
      end;
    6:
      begin
        // MultiMat
        FormMultiMat := TFormMultiMat.Create(FormMaterials);
        FormMultiMat.Parent := FormMaterials;
        FormMultiMat.Align := alClient;
        FormMultiMat.BorderStyle := bsNone;
        FormMultiMat.Show;
      end;
    7:
      begin
        // MultiPass
        FormMultiPass := TFormMultiPass.Create(FormMaterials);
        FormMultiPass.Parent := FormMaterials;
        FormMultiPass.Align := alClient;
        FormMultiPass.BorderStyle := bsNone;
        FormMultiPass.Show;
      end;
    8:
      begin
        // MultiTexture
        FormMultiTexture := TFormMultiTexture.Create(FormMaterials);
        FormMultiTexture.Parent := FormMaterials;
        FormMultiTexture.Align := alClient;
        FormMultiTexture.BorderStyle := bsNone;
        FormMultiTexture.Show;
      end;
    9:
      begin
        // ObjMaterial
        FormMO := TFormMO.Create(FormMaterials);
        FormMO.Parent := FormMaterials;
        FormMO.Align := alClient;
        FormMO.BorderStyle := bsNone;
        FormMO.Show;
      end;
    10:
      begin
        // Clouds
        FormClouds := TFormClouds.Create(FormMaterials);
        FormClouds.Parent := FormMaterials;
        FormClouds.Align := alClient;
        FormClouds.BorderStyle := bsNone;
        FormClouds.Show;
      end;
    11:
      begin
        // TexAnim
        FormTexAnim := TFormTexAnim.Create(FormMaterials);
        FormTexAnim.Parent := FormMaterials;
        FormTexAnim.Align := alClient;
        FormTexAnim.BorderStyle := bsNone;
        FormTexAnim.Show;
      end;
    12:
      begin
        // TexCombine
        FormTexCombine := TFormTexCombine.Create(FormMaterials);
        FormTexCombine.Parent := FormMaterials;
        FormTexCombine.Align := alClient;
        FormTexCombine.BorderStyle := bsNone;
        FormTexCombine.Show;
      end;
    13:
      begin
        // TexFormat
        FormTexFormat := TFormTexFormat.Create(FormMaterials);
        FormTexFormat.Parent := FormMaterials;
        FormTexFormat.Align := alClient;
        FormTexFormat.BorderStyle := bsNone;
        FormTexFormat.Show;
      end;
    14:
      begin
        // TransparAdv
        FormTransparAdv := TFormTransparAdv.Create(FormMaterials);
        FormTransparAdv.Parent := FormMaterials;
        FormTransparAdv.Align := alClient;
        FormTransparAdv.BorderStyle := bsNone;
        FormTransparAdv.Show;
      end;
    15:
      begin
        // Transparency
        FormTransparency := TFormTransparency.Create(FormMaterials);
        FormTransparency.Parent := FormMaterials;
        FormTransparency.Align := alClient;
        FormTransparency.BorderStyle := bsNone;
        FormTransparency.Show;
      end;
  end;
end;

end.
