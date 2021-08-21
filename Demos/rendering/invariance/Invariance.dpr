program Invariance;

uses
  Forms,
  fInvariance in 'fInvariance.pas' {FormInvariance};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormInvariance, FormInvariance);
  Application.Run;
end.
