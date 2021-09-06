program LineStippling;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fLineStippling in 'fLineStippling.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFormLineStippling, FormLineStippling);
  Application.Run;
end.
