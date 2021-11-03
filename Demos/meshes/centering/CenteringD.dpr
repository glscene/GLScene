{: Illustrates the effects of AutoCentering for FreeForms.

   The same mesh is loaded three times and centered with different options
   (by default, the polyhedron is not centered in its mesh).
}
program CenteringD;

uses
  Forms,
  fCenteringD in 'fCenteringD.pas' {FormCentering};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormCentering, FormCentering);
  Application.Run;
end.
