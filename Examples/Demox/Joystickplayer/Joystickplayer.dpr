program Joystickplayer;











{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  fdJoistickplayer in 'fdJoistickplayer.pas' {FormJoistick};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormJoistick, FormJoistick);
  Application.Run;
end.
