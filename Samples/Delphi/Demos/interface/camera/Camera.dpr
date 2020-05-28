{: This sample illustrates basic user-driven camera movements.

	I'm using the GLScene built-in camera movement methods. The camera object is
	a child of its target dummy cube (this means that the camera is translated
	when its target is translate, which is good for flyover/scrolling movements).

	Movements in this sample are done by moving the mouse with a button
	pressed, left button will translate the dummy cube (and the camera),
	right button will rotate the camera around the target, shift+right will
   rotate the object in camera's axis.<br>
	Mouse Wheel allows zooming in/out.<br>
	'7', '9' rotate around the X vector (in red, absolute).<br>
	'4', '6' rotate around the Y vector (in green, absolute).<br>
	'1', '3' rotate around the Z vector (in blue, absolute).<br>
}
program Camera;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
