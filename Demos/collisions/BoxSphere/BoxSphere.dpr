{
  SphereBoxIntersect Demo
   History:
  29/01/07 - DaStr - Initial version (created by dikoe Kenguru)
}
program BoxSphere;

uses
  Forms,
  BoxSphereFm in 'BoxSphereFm.pas' {FormBoxSphere};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormBoxSphere, FormBoxSphere);
  Application.Run;
end.
