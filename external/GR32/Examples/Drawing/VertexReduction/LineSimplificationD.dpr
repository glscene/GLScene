program LineSimplificationD;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fMainD in 'fMainD.pas' {FrmLineSimplification};

begin
  Application.Initialize;
  Application.CreateForm(TFrmLineSimplification, FrmLineSimplification);
  Application.Run;
end.

