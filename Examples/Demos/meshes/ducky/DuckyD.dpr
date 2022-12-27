{: Loading NURBS into a GLScene FreeForm/Actor object

   A very simple parametric model of a duck, comprised of 3 NURBS
   surfaces. The Nurbs format is essentially the NurbsSurface geometry
   type used in VRML. One limitation at the moment is the Control points
   must each be on a separate line. Inverted surfaces are handled with
   the ccw FALSE statement in the .nurbs file (duck3.nurbs uses this
   setting).

   Use the resolution slider to increase or decrease the models triangle
   count dynamically.
}
program DuckyD;

uses
  Forms,
  fDuckyD in 'fDuckyD.pas' {FormDucky};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormDucky, FormDucky);
  Application.Run;
end.
