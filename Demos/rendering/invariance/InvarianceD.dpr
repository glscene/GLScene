program InvarianceD;

uses
  Forms,
  fInvarianceD in 'fInvarianceD.pas' {FormInvariance};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormInvariance, FormInvariance);
  Application.Run;
end.
