{ : Moving objects with the mouse.

  In this demo you can move the two cubes around by picking and dragging
  them. This showcases the use of ScreenVectorIntersectXxxx functions.

  You can also use the numeric keypad to move/zoom the camera and the arrow
  to move the selected object around.

  (Based on Rado Stoyanov's test project)
}
program ObjMove;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
