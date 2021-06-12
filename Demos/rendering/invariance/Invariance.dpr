program Invariance;

uses
  Forms,
  InvarianceFm in 'InvarianceFm.pas' {FormInvariance};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormInvariance, FormInvariance);
  Application.Run;
end.
