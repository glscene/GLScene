{ : Newton Game Dynamics Physics Engine demo.

  This exemple show how to move with keyboard.
  There is already a NewtonWalkAndCarry demo but I wanted to do with another way.
  Press X key to spawn New model.
  Press C key to Respawn Body.
  Press Middle Mouse to grab.
  Press MouseLeft to shoot.
  Hold MouseRight to look

  Body has two Upvectors joint, one in Y one in X.

  <b>History : </b><font size=-1><ul>
  <li>04/02/11 - FP - Created by Franck Papouin
  </ul>
}
program NewtonWalkCarryShoot;

uses
  //ExceptionLog,
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
