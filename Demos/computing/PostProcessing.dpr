program PostProcessing;

uses
  Forms,
  PostProcessingFm in 'PostProcessingFm.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Run;
end.
