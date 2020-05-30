{: Demo/sample/testbed for RayCastIntersect.

   The RayCastIntersect aims at determining an as precise as possible collision
   detection between a ray and and object. With the intersection point is also
   returned the normal (which can be used for things like bouncing).

   In this sample, this mechanism is used to implement a two-cents-worth
   raytracer, simply by throwing rays for each point in a raster image.
   That is what raytracers do, but they go beyond throwing simple rays ;)
   The intersection's normal and intersected  object's material are then used
   to calculate a basic lighting.

   To calculate the raytraced/raycasted image, just hit the "cast" button.

   Note: the quadric solver used for calculating torus intersection lacks
         precision and may demonstrate small holes...
}
program raycast;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
