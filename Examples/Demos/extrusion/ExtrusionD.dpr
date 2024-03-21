program ExtrusionD;

uses
  Vcl.Forms,
  fdExtrusion in 'fdExtrusion.pas' {FormExtrusion},
  fBendingD in 'bendingcyl\fBendingD.pas' {FormBendingCyl},
  fCutoutStarD in 'cutoutstar\fCutoutStarD.pas' {FormCutoutStar},
  fNutsnBoltsD in 'nutsnbolts\fNutsnBoltsD.pas' {FormNutsnBolts},
  fPawnD in 'pawn\fPawnD.pas' {FormPawn},
  fTentaclesD in 'tentacles\fTentaclesD.pas' {FormTentacles};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormExtrusion, FormExtrusion);
  Application.Run;
end.
