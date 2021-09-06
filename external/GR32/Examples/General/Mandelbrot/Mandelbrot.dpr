program Mandelbrot;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fMandelbrot in 'fMandelbrot.pas';

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
