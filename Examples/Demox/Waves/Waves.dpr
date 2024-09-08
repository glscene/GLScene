program Waves;

uses
  System.StartUpCopy,
  FMX.Forms,
  fdWaves in 'fdWaves.pas' {FormWaves};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormWaves, FormWaves);
  Application.Run;
end.
