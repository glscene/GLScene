program ImposterD;

uses
  Forms,
  fImposterD in 'fImposterD.pas' {FormImposter};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormImposter, FormImposter);
  Application.Run;
end.
