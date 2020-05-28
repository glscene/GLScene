{: This Form demonstrates basic "hierarchical" movements.

	Our visible (cube) objects are children of TGLDummyCube object and we move
	them through rotations of the DummyCubes.<br>
	Movements in this demo differs from the "manual" demo : "earth" now spins,
	"moon" has an inclined orbit and spins too.

	You may use any GLScene object when building hierarchical scenes, but it
	is recommended to use TGLDummyCube for structural (regroupment only) objects,
	since dummycube are just "virtual" at run-time and cost no OpenGL setup or
	rendering time.
}
program Hierarchy;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
