program CollisionsD;

uses
  Vcl.Forms,
  fdCollision in 'fdCollision.pas' {FormCollisions};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormCollisions, FormCollisions);
  Application.Run;
end.
