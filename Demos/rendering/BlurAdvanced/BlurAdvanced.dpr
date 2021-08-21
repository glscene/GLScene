// This demo is part of the GLSCene project
// Advanced GLBlur demo
// by Marcus Oblak
program BlurAdvanced;

uses
  Forms,
  fBlurAdvanced in 'fBlurAdvanced.pas' {FormBlurAdvanced};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormBlurAdvanced, FormBlurAdvanced);
  Application.Run;
end.
