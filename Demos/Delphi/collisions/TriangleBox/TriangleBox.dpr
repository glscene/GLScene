{
  TriangleBoxIntersect Demo

  History:
  29/01/07 - DaStr - Initial version (by dikoe Kenguru)
}
program TriangleBox;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
