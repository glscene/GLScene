program BenchD;

uses
  Vcl.Forms,
  fBenchD in 'fBenchD.pas' {frmBench},
  fCanvasD in 'canvas\fCanvasD.pas' {FormCanvas},
  fMegaCubeD in 'megacube\fMegaCubeD.pas' {FormMegacube},
  fMegaglassD in 'megaglasscube\fMegaglassD.pas' {FormMegaglasscube},
  fSmokingD in 'smoking\fSmokingD.pas' {FormSmoking},
  fVolcanoD in 'volcano\fVolcanoD.pas' {FormVolcano},
  fWhirlD in 'whirlwind\fWhirlD.pas' {FormWhirl};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmBench, frmBench);
  Application.Run;
end.
