program FastFourierD;

uses
  Forms,
  uCPUFFT in 'uCPUFFT.pas',
  fFourier_D in 'fFourier_D.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.Run;
end.
