program GLSViewerD;

uses
  Forms,
  fGLForm in 'Source\fGLForm.pas' {GLForm},
  fGLDialog in 'Source\fGLDialog.pas' {GLDialog},
  fMain in 'Source\fMain.pas' {MainForm},
  uGlobals in 'Source\uGlobals.pas',
  uSettings in 'Source\uSettings.pas',
  dGLSViewer in 'Source\dGLSViewer.pas' {dmGLSViewer: TDataModule},
  fGLAbout in 'Source\fGLAbout.pas' {GLAbout},
  fGLOptions in 'Source\fGLOptions.pas' {GLOptions},
  GnuGettext in 'Source\GnuGettext.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'GLSViewer';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TdmGLSViewer, dmGLSViewer);
  Application.Run;
end.
