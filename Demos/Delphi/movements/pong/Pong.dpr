{: This demo is a remake of good old pong game...

	Aim of the game is to prevent the ball from bouncing out of the board,
	each time the ball bumps on your pad you score a frag (er... point ;).<br>
	Move the pad with your mouse.

   The demo makes use of stencil-based shadow volumes.
}
program Pong;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
