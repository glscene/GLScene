program GammaBlur;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fGammaBlur in 'fGammaBlur.pas' {FormGammaBlur};

begin
  Application.Initialize;
  AApplication.CreateForm(TFormGammaBlur, FormGammaBlur);
  pplication.Run;
end.
