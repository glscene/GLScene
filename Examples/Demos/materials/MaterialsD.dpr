program MaterialsD;

uses
  Vcl.Forms,
  fdMaterials in 'fdMaterials.pas' {FormMaterials},
  fCubemapD in 'cubemap\fCubemapD.pas' {FormCubeMap},
  fCustomQuadD in 'customquad\fCustomQuadD.pas' {FormCustomQuad},
  fDynTextureD in 'dynamictexture\fDynTextureD.pas' {FormDynamicTexture},
  fDynCubemapD in 'dyncubemap\fDynCubemapD.pas' {FormDynCubeMap},
  fMatScriptD in 'MaterialScript\fMatScriptD.pas' {FormMatScript},
  fMirrorD in 'mirror\fMirrorD.pas' {FormMirror},
  fMultiMaterialD in 'multimaterial\fMultiMaterialD.pas' {FormMultiMat},
  fMultiPassD in 'multipass\fMultiPassD.pas' {FormMultiPass},
  fMultiTextureD in 'multitexture\fMultiTextureD.pas' {FormMultiTexture},
  fObjMaterialD in 'objmaterial\fObjMaterialD.pas' {FormMO},
  fProcCloudsD in 'proceduralclouds\fProcCloudsD.pas' {FormClouds},
  fTexAnimD in 'texanim\fTexAnimD.pas' {FormTexAnim},
  fTexCombineD in 'texcombine\fTexCombineD.pas' {FormTexCombine},
  fTexFormatD in 'texformat\fTexFormatD.pas' {FormTexFormat},
  fTransparAdvD in 'TransparAdv\fTransparAdvD.pas' {FormTransparAdv},
  fTransparencyD in 'transparency\fTransparencyD.pas' {FormTransparency};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMaterials, FormMaterials);
  Application.CreateForm(TFormCubeMap, FormCubeMap);
  Application.CreateForm(TFormCustomQuad, FormCustomQuad);
  Application.CreateForm(TFormDynamicTexture, FormDynamicTexture);
  Application.CreateForm(TFormDynCubeMap, FormDynCubeMap);
  Application.CreateForm(TFormMatScript, FormMatScript);
  Application.CreateForm(TFormMirror, FormMirror);
  Application.CreateForm(TFormMultiMat, FormMultiMat);
  Application.CreateForm(TFormMultiPass, FormMultiPass);
  Application.CreateForm(TFormMultiTexture, FormMultiTexture);
  Application.CreateForm(TFormMO, FormMO);
  Application.CreateForm(TFormClouds, FormClouds);
  Application.CreateForm(TFormTexAnim, FormTexAnim);
  Application.CreateForm(TFormTexCombine, FormTexCombine);
  Application.CreateForm(TFormTexFormat, FormTexFormat);
  Application.CreateForm(TFormTransparAdv, FormTransparAdv);
  Application.CreateForm(TFormTransparency, FormTransparency);
  Application.Run;
end.
