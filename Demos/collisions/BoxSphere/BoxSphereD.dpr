{
  SphereBoxIntersect Demo
   History:
  29/01/07 - DaStr - Initial version (created by dikoe Kenguru)
}
program BoxSphereD;

uses
  Forms,
  fBoxSphereD in 'fBoxSphereD.pas' {FormBoxSphere};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormBoxSphere, FormBoxSphere);
  Application.Run;
end.
