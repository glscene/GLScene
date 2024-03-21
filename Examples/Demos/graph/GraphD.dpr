program GraphD;

uses
  Vcl.Forms,
  fdGraph in 'fdGraph.pas' {FormGraph},
  fFxyD in 'fxy\fFxyD.pas' {FormFxy},
  fHeightFieldD in 'heightfield\fHeightFieldD.pas' {FormHeightField},
  fPointsD in 'points\fPointsD.pas' {FormPoints},
  fProjectionD in 'projection\fProjectionD.pas' {FormProjection},
  fSplinesD in 'splines\fSplinesD.pas' {FormSplines};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormGraph, FormGraph);
  Application.Run;
end.
