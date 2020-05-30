{: A very simple demo of the TGLPipe object, to show what it can do.

   The TGLPipe objects extrudes a circle along a trajectory (given by its node).
   You can specify a radius factor for each node and use spline smoothing.

   Here we only use 3 control points, the top ones moves horizontally, and the
   middle one can be made fat/slim.

   The current implementation is very limited when it comes to 3D pipes, as there
   is no "smooth" rotation interpolator, therefore, ou will have best results
   if your trajectory stays in the X/Y (local) plane.
}
program bendingcyl;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
