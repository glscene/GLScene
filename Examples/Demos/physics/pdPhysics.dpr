program pdPhysics;

uses
  Vcl.Forms,
  fdPhysics in 'fdPhysics.pas' {FormPhysics};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormPhysics, FormPhysics);
  Application.Run;
end.
