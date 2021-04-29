program LineStipplingD;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fMainD in 'fMainD.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFormLineStippling, FormLineStippling);
  Application.Run;
end.
