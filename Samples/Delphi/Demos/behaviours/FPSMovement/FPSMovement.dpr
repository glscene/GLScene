//******************************************************************************
//  SphereSweepAndSlide - Initial work by Dan Bartlett
//  Shows how to use the FPS Movement behaviour
//----------------------------------------
//  Controls:
//    W,A,S,D: Movement
//    Mouse: Movement
//    I,J,K,L,O,P: Movement (2nd sphere)
//    F2, F3: First person, Third person
//    F5: Toggle wireframe
//    Space: Move upwards
//    Esc: Quit
//******************************************************************************
program FPSMovement;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
