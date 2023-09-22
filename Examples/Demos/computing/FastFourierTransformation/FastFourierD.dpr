program FastFourierD;

uses
  Forms,
  uCPUFFT in 'uCPUFFT.pas',
  fFastFourierD in 'fFastFourierD.pas' {Form1};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
