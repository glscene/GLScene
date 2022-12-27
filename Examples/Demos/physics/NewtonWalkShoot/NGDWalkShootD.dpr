(* NGD Physics Engine demo.

  This example show how to move with keyboard.
  Press X key to spawn New model.
  Press C key to Respawn Body.
  Press Middle Mouse to grab.
  Press MouseLeft to shoot.
  Hold MouseRight to look

  Body has two Upvectors joint, one in Y one in X.
  04/02/11 - FP - Created by Franck Papouin
*)
program NGDWalkShootD;

uses
  Forms,
  fNGDWalkShootD in 'fNGDWalkShootD.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormNewtonWalkShoot, FormNewtonWalkShoot);
  Application.Run;
end.
