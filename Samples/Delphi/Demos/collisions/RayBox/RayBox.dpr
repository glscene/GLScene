{: RayCastBoxIntersect example demo 

  History:
  22/01/07 - DaStr - Initial version (by dikoe Kenguru)
 }
program RayBox;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
