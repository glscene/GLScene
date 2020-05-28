{
  GLAtmosphere Demo.

  Note: object "Not_a_planet" is used to test atmosphere transparency issues.

  Version history:
    26/01/10 - Yar - Added GLColor to uses
    20/08/07 - DaStr - Now demo correctly displays FPS
    15/08/07 - LC - Added GLBehaviours to "uses" section to
                    prevent run-time error.
    03/04/07 - DaStr - Cleaned up "uses" section
    02/03/07 - DaStr - Fixed LensFlare object sorting issue
                       Updated GLSimpleNavigation component
    07/02/07 - DaStr - Initial version

}
program Atmosphere;

uses
  Forms,
  uMainForm in 'uMainForm.pas' {MainForm};

{$R *.res}
  
begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
