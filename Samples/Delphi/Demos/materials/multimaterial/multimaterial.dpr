{: Basic GLMultiMaterialShader example.

   The GLMultiMaterialShader applies a pass for each material in
   the assigned MaterialLibrary. This example shows how to apply
   three blended textures to an object.

   A fourth texture is used in the specular pass to map the area
   affected by the chrome-like shine.
}
program multimaterial;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
