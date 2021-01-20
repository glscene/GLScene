{: TGLMultiPolygon Sample, contributed by Uwe Raabe.

   Note: this sample has been partly obsoleted/superseded by the TGLExtrusionSolid
   (by Uwe Raabe), which allows building such solids directly.
}
program ExPolygon;

uses
  Forms,
  ExPolygonFm in 'ExPolygonFm.pas' {FormExPolygon};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormExPolygon, FormExPolygon);
  Application.Run;
end.
