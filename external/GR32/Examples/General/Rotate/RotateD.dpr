program RotateD;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fMainD in 'fMainD.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFormRotateExample, FormRotateExample);
  Application.Run;
end.
