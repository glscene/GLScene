program VertexGeneration;

uses
  Forms,
  VertexGenerationFm in 'VertexGenerationFm.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Run;
end.
