{: Basic Octree/Collision demo.
   Robert Hayes, March 2002.
   This demo uses the new Octree class to quickly detect triangle-level collisions with a sphere.
   See Octree.SphereIntersect() for detailed comments.
}
program boxedin;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
