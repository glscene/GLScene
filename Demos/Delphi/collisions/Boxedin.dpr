{: Basic Octree/Collision demo.
   Robert Hayes, March 2002.
   This demo uses the new Octree class to quickly detect triangle-level collisions with a sphere.
   See Octree.SphereIntersect() for detailed comments.
}
program Boxedin;

uses
  Forms,
  BoxedinFm in 'BoxedinFm.pas' {FormBoxedin};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormBoxedin, FormBoxedin);
  Application.Run;
end.
