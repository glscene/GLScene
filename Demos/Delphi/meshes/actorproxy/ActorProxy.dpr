// THIS DEMO IS PART OF THE GLSCENE PROJECT
{
  Version History:
    30.01.2008 - mrqzzz - Initial version.
    06.02.2008 - mrqzzz - Added  "RayCastIntersect" for Actorproxy in demo.
    15.03.2008 - DaStr  - Updated RayCastIntersect stuff because of changes in
                          the TGLActorProxy component.

}
program ActorProxy;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
