program BehavioursD;

uses
  Vcl.Forms,
  fdBehaviours in 'fdBehaviours.pas' {FormBehaviours};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormBehaviours, FormBehaviours);
  Application.Run;
end.
