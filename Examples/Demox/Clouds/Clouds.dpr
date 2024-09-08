program Clouds;

uses
  System.StartUpCopy,
  FMX.Forms,
  fdClouds in 'fdClouds.pas' {FormClouds};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormClouds, FormClouds);
  Application.Run;
end.
