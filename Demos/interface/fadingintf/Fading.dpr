{: Fading interface sample.

	This is a smoother (but more CPU and framerate intensive) approach to
	picking objects : when the pointer hovers on an object, it smoothly becomes
	red, and when it moves away it progressively turns back to grey.

	It is implemented here using a shared field, "currentPick" (by shared,
	I mean it's a form field used in more than one event) indicating the
	object the mouse is currently hovering, a classic timer and the "Progress"
	chain of events.
   When a mouse move is detected, it activates a timer, and when this timer
   is fired, the picking is performed. The "direct" approach would perform
   picking in the mousemove event, however, if the picking takes more time
   to be completed than the next mousemove event takes time to arrive,
   the interface will seem to "freeze".

   The other timer is used to provide basic color animation, limited to about
   20 FPS (resolution of the timer isn't high enough to allow much higher
   framerates). Check other samples for better framerate independance techniques
   (and use the TGLCadencer !).

	Note that all objects (sphere, torus...) share the same event.
}
program Fading;

uses
  Forms,
  FadingFm in 'FadingFm.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
