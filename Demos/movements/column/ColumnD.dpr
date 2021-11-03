(* This form showcases runtime object creation and framerate independant motion.

	We start with an almost empty scene. The dummy cube is used as a convenient
	way to orient the camera (using its TargetObject property). Planes are
	programmatically added to the scene in FormCreate and spinned in the
	GLCadencer1Progress event.

	Framerate independance motion is obtained by using a clock reference (in
	this sample, it is given by the TGLCadencer, which uses the high performance
   precision counter as reference). You can check it by resizing the window :
   whatever the framerate, the spin speed is the same.
	In this sample, it is extremely simply done, but with more complex scenes
	and movements the same rule applies : for framerate independant motion, you
	need a clock measurement.

   Using the TGLCadencer is the standard way for animating under GLScene, it
   offers various option and can drive animation automatically or just act as
   a manual trigger. Basic use just involves dropping a cadencer and adjusting
   its "Scene" property.

	Note that measured framerates are 1 sec averages, a TTimer is used to refresh
   and reset FPS counter.
*)
program ColumnD;

uses
  Forms,
  fColumnD in 'fColumnD.pas' {FormColumn};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormColumn, FormColumn);
  Application.Run;
end.
