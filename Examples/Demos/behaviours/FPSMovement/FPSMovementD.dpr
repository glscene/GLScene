(*
  SphereSweepAndSlide - Initial work by Dan Bartlett
  Shows how to use the FPS Movement behaviour
----------------------------------------
  Controls:
    W,A,S,D: Movement
    Mouse: Movement
    I,J,K,L,O,P: Movement (2nd sphere)
    F2, F3: First person, Third person
    F5: Toggle wireframe
    Space: Move upwards
    Esc: Quit
*)
program FPSMovementD;

uses
  Vcl.Forms,
  fFPSMovementD in 'fFPSMovementD.pas' {FormFPSMovement};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormFPSMovement, FormFPSMovement);
  Application.Run;
end.
