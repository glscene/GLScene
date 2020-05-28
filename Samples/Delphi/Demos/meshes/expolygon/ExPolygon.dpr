{: TGLMultiPolygon Sample, contributed by Uwe Raabe.

   Note: this sample has been partly obsoleted/superseded by the TGLExtrusionSolid
   (by Uwe Raabe), which allows building such solids directly.
}
program ExPolygon;

uses
  Forms,
  ExPolygon1 in 'ExPolygon1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
