program OdeMachine;

uses
  Forms,
  fOdeMachine in 'fOdeMachine.pas' {FormOdeMachine};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormOdeMachine, FormOdeMachine);
  Application.Run;
end.
