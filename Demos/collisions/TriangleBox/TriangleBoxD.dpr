{
  TriangleBoxIntersect Demo

  History:
  29/01/07 - DaStr - Initial version (by dikoe Kenguru)
}
program TriangleBoxD;

uses
  Forms,
  fTriangleBoxD in 'fTriangleBoxD.pas' {FormTriangleBox};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTriangleBox, FormTriangleBox);
  Application.Run;
end.
