program SkyboxShader;

uses
  Forms,
  SkyboxShaderFm in 'SkyboxShaderFm.pas' {FormSkyboxShader};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSkyboxShader, FormSkyboxShader);
  Application.Run;
end.
