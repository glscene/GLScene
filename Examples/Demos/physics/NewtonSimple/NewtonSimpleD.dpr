(* Newton Game Dynamics Physics Engine demo.

  This is the simplest example of how to create dynamic body suported by NGD.
  To execute the simulation we need to indicate to the physics engine the time
  elapsed for update in GLCadencer1Progress.
  The floor is static, so it can't move.

  Updated for GLScene by Franck Papouin 31/01/11
*)
program NewtonSimpleD;

uses
  Forms,
  fNewtonSimpleD in 'fNewtonSimpleD.pas' {FormNewtonSimple};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormNewtonSimple, FormNewtonSimple);
  Application.Run;
end.
