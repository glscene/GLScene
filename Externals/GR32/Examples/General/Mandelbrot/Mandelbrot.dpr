program Mandelbrot;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fMandelbrot in 'fMandelbrot.pas' {MainForm};

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
