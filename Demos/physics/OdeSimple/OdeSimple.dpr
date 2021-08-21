program OdeSimple;

uses
  Forms,
  fOdeSimple in 'fOdeSimple.pas' {FormOdeSimple};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormOdeSimple, FormOdeSimple);
  Application.Run;
end.
