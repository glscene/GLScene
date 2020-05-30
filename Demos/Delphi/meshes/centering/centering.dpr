{: Illustrates the effects of AutoCentering for FreeForms.

   The same mesh is loaded three times and centered with different options
   (by default, the polyhedron is not centered in its mesh).
}
program centering;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
