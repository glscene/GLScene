{:
  A Demo that shows how the new TGLPostEffect component works.

  Version history:
    16/08/07 - DaStr - Added pepBlur preset.
    09/03/07 - DaStr - Added pepNightVision preset.
    07/03/07 - DaStr - Updated according to changes in the GLPostEffects unit.
    02/03/07 - DaStr - Initial version (based on demo by Grim).

}
program posteffect;

uses
  Forms,
  uMainForm in 'uMainForm.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
