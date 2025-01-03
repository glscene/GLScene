program Graphics32xDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  fxGR32 in 'fxGR32.pas' {fxFMXGR32};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfxFMXGR32, fxFMXGR32);
  Application.Run;
end.
