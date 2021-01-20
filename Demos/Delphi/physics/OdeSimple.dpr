program OdeSimple;

uses
  Forms,
  OdeSimpleFm in 'OdeSimpleFm.pas' {FormOdeSimple};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormOdeSimple, FormOdeSimple);
  Application.Run;
end.
