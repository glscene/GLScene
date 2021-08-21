program MatScript;

uses
  Forms,
  fMatScript in 'fMatScript.pas' {FormMatScript};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMatScript, FormMatScript);
  Application.Run;
end.
