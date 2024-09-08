program Cubemap;

uses
  System.StartUpCopy,
  FMX.Forms,
  fdCubemap in 'fdCubemap.pas' {FormCubemap};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormCubemap, FormCubemap);
  Application.Run;
end.
