(* This Form demonstrates basic "manual" movements.

	Positions are computed directly using Sin/Cos functions.

	A cadencer is used to "play" the animation, it is not used as a time-controler,
  but just as a way to push the animation as fast as possible. See further
  samples on framerate independance to see how it can be better used.

	Note : when using 3Dfx OPENGL and a Voodoo3 on Win9x in 24bits resolution,
	the driver always uses internal double-buffering (since it can only render
	in 16bits), and keeping the requesting double-buffering in the TGLSceneViewer
	actually results in a "quadruple-buffering"...
*)
program ManualD;

uses
  Forms,
  fManualD in 'fManualD.pas' {FormManual};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormManual, FormManual);
  Application.Run;
end.
