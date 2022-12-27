program OdeSimpleD;

uses
  Forms,
  fOdeSimpleD in 'fOdeSimpleD.pas' {FormOdeSimple};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormOdeSimple, FormOdeSimple);
  Application.Run;
end.
