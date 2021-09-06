program Curves;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fCurves in 'fCurves.pas';

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
