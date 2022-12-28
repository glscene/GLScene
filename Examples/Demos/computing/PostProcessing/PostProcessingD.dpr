program PostProcessingD;

uses
  Vcl.Forms,
  fPostProcessingD in 'fPostProcessingD.pas';

{$R *.res}

begin
  //ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Run;
end.
