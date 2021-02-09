{: Generates a 3D mesh from a height-field function.

   The left viewer shows a triangle-based mesh, the right viewer uses a
   triangle-strip-based mesh, both mesh are based on the same height-field and
   have the same resolution.

   This sample wants to demonstrate a few things :<ul>
   <li>how to define a mesh (triangle & triangle strip)
   <li>what the default normal calculations looks like
   <li>TriangleStrips are faster, but slightly more complex to use
   </ul>

   In triangle mode, the normals are computed on a triangle basis, hence the
   facetted look, for triangle-strip, they are computed for each vertex based
   on the triangle it completes (smooth along strip-direction).

   The reader may make the "good" looking version (ie. smooth aspect in all
   direction) by calculating the proper normal from the formula instead of
   using standard normal calculations.

   Sample framerates (K6-400 + Sofware OpenGL), 5000 triangles (cResolution=25) :
   -  mmTriangles : 9.6 FPS
   -  mmTriangleStrip : 17.2 FPS
   Sample framerates (K7-500 + GeForce 256), 20000 triangles (cResolution=50) :
   -  mmTriangles : 53 FPS
   -  mmTriangleStrip : 202 FPS
}
program Formula;

uses
  Forms,
  FormulaFm in 'FormulaFm.pas' {FormFormula};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormFormula, FormFormula);
  Application.Run;
end.
