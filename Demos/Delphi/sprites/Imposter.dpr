program Imposter;

uses
  Forms,
  ImposterFm in 'ImposterFm.pas' {FormImposter};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormImposter, FormImposter);
  Application.Run;
end.
