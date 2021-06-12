{: Octree raycast/mesh sample.
   Demonstrating how to find the intersection point between eye-screen ray
   and a high poly mesh with an Octree property.

   To see the performance difference:
  -move mouse around on the scene with octree disabled (default)
   -check the "octree enabled" box.  Note the frame-rate difference.

   .
}
program Octreedemo;

uses
  Forms,
  OctreedemoFm in 'OctreedemoFm.pas' {FormOctreedemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormOctreedemo, FormOctreedemo);
  Application.Run;
end.
