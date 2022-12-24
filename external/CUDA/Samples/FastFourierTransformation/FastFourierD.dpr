program FastFourierD;

uses
  Forms,
  uCPUFFT in 'uCPUFFT.pas',
  fFourier_D in 'fFourier_D.pas' {FormFFT};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TFormFFT, FormFFT);
  Application.Run;
end.
