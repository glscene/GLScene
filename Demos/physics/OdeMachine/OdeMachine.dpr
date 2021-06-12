program OdeMachine;

uses
  Forms,
  OdeMachineFm in 'OdeMachineFm.pas' {FormOdeMachine};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormOdeMachine, FormOdeMachine);
  Application.Run;
end.
