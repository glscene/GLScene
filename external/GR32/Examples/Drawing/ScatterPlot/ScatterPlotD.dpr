program ScatterPlotD;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fMainD in 'fMainD.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFmScatterPlot, FmScatterPlot);
  Application.Run;
end.

