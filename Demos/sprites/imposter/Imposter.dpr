program Imposter;

uses
  Forms,
  fImposter in 'fImposter.pas' {FormImposter};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormImposter, FormImposter);
  Application.Run;
end.
