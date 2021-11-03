{: RayCastBoxIntersect example demo

  History:
  22/01/07 - DaStr - Initial version (by dikoe Kenguru)
 }
program RayBoxD;

uses
  Forms,
  fRayBoxD in 'fRayBoxD.pas' {FormRayBox};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormRayBox, FormRayBox);
  Application.Run;
end.
