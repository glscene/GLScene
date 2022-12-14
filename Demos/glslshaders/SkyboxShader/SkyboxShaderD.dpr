program SkyboxShaderD;

uses
  Forms,
  fSkyboxShaderD in 'fSkyboxShaderD.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSkyboxShader, FormSkyboxShader);
  Application.Run;
end.
