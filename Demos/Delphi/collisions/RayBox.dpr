{: RayCastBoxIntersect example demo

  History:
  22/01/07 - DaStr - Initial version (by dikoe Kenguru)
 }
program RayBox;

uses
  Forms,
  RayBoxFm in 'RayBoxFm.pas' {FormRayBox};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormRayBox, FormRayBox);
  Application.Run;
end.
