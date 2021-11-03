// This demo is part of the GLSCene project
// Advanced GLBlur demo
// by Marcus Oblak
program BlurAdvancedD;

uses
  Forms,
  fBlurAdvancedD in 'fBlurAdvancedD.pas' {FormBlurAdvanced};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormBlurAdvanced, FormBlurAdvanced);
  Application.Run;
end.
