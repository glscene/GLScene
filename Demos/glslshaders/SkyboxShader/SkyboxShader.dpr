program SkyboxShader;

uses
  Forms,
  fSkyboxShader in 'fSkyboxShader.pas' {FormSkyboxShader};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSkyboxShader, FormSkyboxShader);
  Application.Run;
end.
