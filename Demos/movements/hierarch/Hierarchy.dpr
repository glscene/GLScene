(* This Form demonstrates basic "hierarchical" movements.

	Our visible (cube) objects are children of TGLDummyCube object and we move
	them through rotations of the DummyCubes.
	Movements in this demo differs from the "manual" demo : "earth" now spins,
	"moon" has an inclined orbit and spins too.

	You may use any GLScene object when building hierarchical scenes, but it
	is recommended to use TGLDummyCube for structural (regroupment only) objects,
	since dummycube are just "virtual" at run-time and cost no OpenGL setup or
	rendering time.
*)
program Hierarchy;

uses
  Forms,
  HierarchyFm in 'HierarchyFm.pas' {FormHierarchy};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormHierarchy, FormHierarchy);
  Application.Run;
end.
