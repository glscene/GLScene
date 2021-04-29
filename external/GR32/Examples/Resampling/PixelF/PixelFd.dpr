program PixelFd;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fMainD in 'fMainD.pas';

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
