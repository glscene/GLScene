(*
  WaterPlane demo
*)

program WaterPlaneD;

uses
  Forms,
  fWaterPlaneD in 'fWaterPlaneD.pas' {FormWaterPlane};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormWaterPlane, FormWaterPlane);
  Application.Run;
end.
