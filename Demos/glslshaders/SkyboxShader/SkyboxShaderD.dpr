program SkyboxShaderD;

uses
  Forms,
  fSkyboxShaderD in 'fSkyboxShaderD.pas' {FormSkyboxShader};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSkyboxShader, FormSkyboxShader);
  Application.Run;
end.
