program Rotate;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fRotate in 'fRotate.pas' {FormRotateExample};

begin
  Application.Initialize;
  Application.CreateForm(TFormRotateExample, FormRotateExample);
  Application.CreateForm(TFormRotateExample, FormRotateExample);
  Application.Run;
end.
