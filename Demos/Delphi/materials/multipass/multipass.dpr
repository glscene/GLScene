{: Simple TGLShader based multipass demo.

   This demo uses a custom TGLShader subclass to implement the classic
   multipass hidden lines rendering technique on a torus: first pass renders
   model with filled triangles, second pass does the wireframe.

   You'll also note the glPolygonOffset call, it displaces fragments depths
   value a little "farther away" so that surface fill depth values do not
   interact with the rendering of the lines (comment out the call and you'll
   see).<br>
   The axis and sphere allow you to see the limit of that simple technique:
   it actually "paints" between the lines, so you cannot use it to make
   transparent wireframed objects with hidden lines - if that thought ever
   blossomed in your mind ;)

   Additionnal objects around the show a glow/toon edges effect achieved in two
   passes too: the 1st pass activate lines and gives them a width, the second
   is used to fill the surface (and clear the lines that aren't on edges).
   (TOutLineShader thanks to Delauney Jerome, jdelauney@free.fr)
}
program multipass;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
