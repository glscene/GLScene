//
// This unit is part of the GLScene Engine, http://glscene.org
//
{
  GLSLShaderLab Demo : Demo that show how to use some GLSLShader.
  If you want to use your own model, take care that models need to have UV Coordinates

  History :
  01/12/15 - J.Delauney - Creation

}
program ShadersLab;

uses
  Vcl.Forms,
  ShaderLabFm in 'ShaderLabFm.pas' {FormShaderLab};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormShaderLab, FormShaderLab);
  Application.Run;
end.
