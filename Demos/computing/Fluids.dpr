program Fluids;

uses
  Forms,
  FluidsFm in 'FluidsFm.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Run;
end.
