program CubicSpline;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FormBezier};

begin
  Application.Initialize;
  Application.CreateForm(TFormBezier, FormBezier);
  Application.CreateForm(TFormBezier, FormBezier);
  Application.Run;
end.

