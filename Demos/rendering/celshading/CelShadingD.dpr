program CelShadingD;

uses
  Forms,
  fCelShadingD in 'fCelShadingD.pas' {FormCelShading};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormCelShading, FormCelShading);
  Application.Run;
end.
