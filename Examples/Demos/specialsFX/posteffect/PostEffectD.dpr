(*
  A Demo that shows how the new TGLPostEffect component works.

  History:
    16/08/07 - DaStr - Added pepBlur preset and pepNightVision preset.
    07/03/07 - DaStr - Updated according to changes in the GLPostEffects unit.
    02/03/07 - DaStr - Initial version (based on demo by Grim).
*)
program PostEffectD;

uses
  Forms,
  fPostEffectD in 'fPostEffectD.pas' {FormPostEffect};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPostEffect, FormPostEffect);
  Application.Run;
end.
