{: This demo is a remake of good old pong game...

	Aim of the game is to prevent the ball from bouncing out of the board,
	each time the ball bumps on your pad you score a frag (er... point ;).
	Move the pad with your mouse.

   The demo makes use of stencil-based shadow volumes.
}
program PongD;

uses
  Forms,
  fPongD in 'fPongD.pas' {FormPong};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormPong, FormPong);
  Application.Run;
end.
