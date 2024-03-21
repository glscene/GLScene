program BenchD;

uses
  Vcl.Forms,
  fdBench in 'fdBench.pas' {FormBench},
  fMegaCubeD in 'megacube\fMegaCubeD.pas' {FormMegacube},
  fMegaglassD in 'megaglasscube\fMegaglassD.pas' {FormMegaglasscube},
  fSmokingD in 'smoking\fSmokingD.pas' {FormSmoking},
  fVolcanoD in 'volcano\fVolcanoD.pas' {FormVolcano},
  fWhirlD in 'whirlwind\fWhirlD.pas' {FormWhirl},
  fCanvasD in 'canvas\fCanvasD.pas' {FormCanvas};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormBench, FormBench);
  Application.CreateForm(TFormCanvas, FormCanvas);
  Application.Run;
end.
