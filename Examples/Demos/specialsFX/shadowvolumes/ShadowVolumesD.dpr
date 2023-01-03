(*
   Shadow volumes demo.

   History :
      29/11/03 - MF - Items now self shadow, and a new cylinder was added.
        Both changes are intended to demonstrate the problems of darkening.
      17/05/03 - EG - Creation (based on code from Mattias Fagerlund)
*)
program ShadowVolumesD;

uses
  Forms,
  fShadowVolumesD in 'fShadowVolumesD.pas' {FormShadowVolumes};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormShadowVolumes, FormShadowVolumes);
  Application.Run;
end.
