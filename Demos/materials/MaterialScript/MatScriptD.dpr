program MatScriptD;

uses
  Forms,
  fMatScriptD in 'fMatScriptD.pas' {FormMatScript};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMatScript, FormMatScript);
  Application.Run;
end.
