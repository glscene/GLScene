program ClipperD;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fMainD in 'fMainD.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFrmClipper, FrmClipper);
  Application.Run;
end.
