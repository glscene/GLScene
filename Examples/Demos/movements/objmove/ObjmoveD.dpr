(*
  Moving objects with the mouse.

  In this demo you can move the cubes around by picking and dragging
  them. This showcases the use of ScreenVectorIntersectXxxx functions.

  You can also use the numeric keypad to move/zoom the camera and the arrow
  to move the selected object around.

  // Based on Rado Stoyanov's test project
*)
program ObjmoveD;

uses
  Forms,
  fObjmoveD in 'fObjmoveD.pas' {FormObjmove};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormObjmove, FormObjmove);
  Application.Run;
end.
