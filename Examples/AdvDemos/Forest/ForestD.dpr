program ForestD;

uses
  Forms,
  fForestD in 'fForestD.pas' {FormForest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormForest, FormForest);
  Application.Run;
end.
