program BlendVsMerge;
{$R 'Media.res' 'Media.rc'}
uses
  Forms,
  fMainD in 'fMainD.pas' {MainForm};

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
