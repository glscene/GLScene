{
  SphereBoxIntersect Demo
   History:
  29/01/07 - DaStr - Initial version (created by dikoe Kenguru)
}
program BoxSphere;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
