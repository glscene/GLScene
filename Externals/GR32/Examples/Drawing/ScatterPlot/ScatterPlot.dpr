program ScatterPlot;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fScatterPlot in 'fScatterPlot.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFmScatterPlot, FmScatterPlot);
  Application.Run;
end.

