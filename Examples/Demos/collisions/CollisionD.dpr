program CollisionD;

uses
  Vcl.Forms,
  fCollisionD in 'fCollisionD.pas' {frmCollisions};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmCollisions, frmCollisions);
  Application.Run;
end.
