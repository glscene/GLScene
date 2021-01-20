program CelShading;

uses
  Forms,
  CelShadingFm in 'CelShadingFm.pas' {FormCelShading};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormCelShading, FormCelShading);
  Application.Run;
end.
