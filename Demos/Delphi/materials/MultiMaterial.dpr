{: Basic GLMultiMaterialShader example.

   The GLMultiMaterialShader applies a pass for each material in
   the assigned MaterialLibrary. This example shows how to apply
   three blended textures to an object.

   A fourth texture is used in the specular pass to map the area
   affected by the chrome-like shine.
}
program MultiMaterial;

uses
  Forms,
  MultiMaterialFm in 'MultiMaterialFm.pas' {FormMultiMat};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMultiMat, FormMultiMat);
  Application.Run;
end.
