{: Basic Octree/Collision demo.
   Robert Hayes, March 2002.
   This demo uses the new Octree class to quickly detect triangle-level collisions with a sphere.
   See Octree.SphereIntersect() for detailed comments.
}
program BoxedinD;

uses
  Forms,
  fBoxedinD in 'fBoxedinD.pas' {FormBoxedin};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormBoxedin, FormBoxedin);
  Application.Run;
end.
