program ShadertoyD;

uses
  Forms,
  fShadertoyD in 'fShadertoyD.pas' {FormEiffie};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormEiffie, FormEiffie);
  Application.Run;
end.
