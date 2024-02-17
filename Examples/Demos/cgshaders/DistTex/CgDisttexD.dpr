(*
  This little demo shows how to use the GL_TEXTURE_RECT_NV extension for creating
  effects applied on the screen (filters, grayscaling etc.).

  I tried it on a HUD sprite first but for some reason, its coordinates are messed
  up. HUD sprites are using somewhat different calcultions than a regular object
  so I guess the vertex program handles the coordinates wrong. Didn't have time
  to really look at it so I did a cheesy solution, just putting a plane 'exactly'
  in front of the camera. Of course, that doesn't work really well when moving/rotating
  the camera.

  Anyway, it looks like if you were looking into 3D space but in fact there's nothing
  but a flat plane in front of your nose with a snapshot of the scene on it. The scene
  itself is behind that plane.
*)
program CgDisttexD;

uses
  Forms,
  fDisttexD in 'fDisttexD.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
