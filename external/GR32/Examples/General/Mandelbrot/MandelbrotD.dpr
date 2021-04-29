program MandelbrotD;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fMandelD in 'fMandelD.pas';

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
