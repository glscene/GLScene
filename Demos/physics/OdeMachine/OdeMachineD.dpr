program OdeMachineD;

uses
  Forms,
  fOdeMachineD in 'fOdeMachineD.pas' {FormOdeMachine};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormOdeMachine, FormOdeMachine);
  Application.Run;
end.
