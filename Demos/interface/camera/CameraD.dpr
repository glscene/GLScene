{: This sample illustrates basic user-driven camera movements.

	I'm using the GLScene built-in camera movement methods. The camera object is
	a child of its target dummy cube (this means that the camera is translated
	when its target is translate, which is good for flyover/scrolling movements).

	Movements in this sample are done by moving the mouse with a button
	pressed, left button will translate the dummy cube (and the camera),
	right button will rotate the camera around the target, shift+right will
   rotate the object in camera's axis.
	Mouse Wheel allows zooming in/out.
	'7', '9' rotate around the X vector (in red, absolute).
	'4', '6' rotate around the Y vector (in green, absolute).
	'1', '3' rotate around the Z vector (in blue, absolute).
}
program CameraD;

uses
  Forms,
  fCameraD in 'fCameraD.pas' {FormCamera};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormCamera, FormCamera);
  Application.Run;
end.
