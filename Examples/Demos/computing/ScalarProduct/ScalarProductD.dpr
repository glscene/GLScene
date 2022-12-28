program ScalarProductD;

uses
  Forms,
  fScalarProductD in 'fScalarProductD.pas' {FormSP};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormSP, FormSP);
  Application.Run;
end.
