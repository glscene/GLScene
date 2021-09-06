program TextVPR;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fTextVPR in 'fTextVPR.pas';

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
