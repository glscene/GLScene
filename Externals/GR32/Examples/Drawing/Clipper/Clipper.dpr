program Clipper;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fClipper in 'fClipper.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFrmClipper, FrmClipper);
  Application.Run;
end.
