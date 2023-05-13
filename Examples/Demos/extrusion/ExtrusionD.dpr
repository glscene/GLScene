program ExtrusionD;

uses
  Vcl.Forms,
  fExtrusionD in 'fExtrusionD.pas' {frmExtrusionD},
  fBendingD in 'bendingcyl\fBendingD.pas' {FormBendingCyl},
  fCutoutStarD in 'cutoutstar\fCutoutStarD.pas' {FormCutoutStar},
  fNutsnBoltsD in 'nutsnbolts\fNutsnBoltsD.pas' {FormNutsnBolts},
  fPawnD in 'pawn\fPawnD.pas' {FormPawn},
  fTentaclesD in 'tentacles\fTentaclesD.pas' {FormTentacles};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmExtrusionD, frmExtrusionD);
  Application.Run;
end.
