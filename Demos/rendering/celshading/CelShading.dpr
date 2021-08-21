program CelShading;

uses
  Forms,
  fCelShading in 'fCelShading.pas' {FormCelShading};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormCelShading, FormCelShading);
  Application.Run;
end.
