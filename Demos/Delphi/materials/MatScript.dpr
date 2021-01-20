program MatScript;

uses
  Forms,
  MatscriptFm in 'MatscriptFm.pas' {FormMatScript};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMatScript, FormMatScript);
  Application.Run;
end.
