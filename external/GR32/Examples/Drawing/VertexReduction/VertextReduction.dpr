program VertextReduction;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fVertextReduction in 'fVertextReduction.pas' {FrmLineSimplification};

begin
  Application.Initialize;
  Application.CreateForm(TFrmLineSimplification, FrmLineSimplification);
  Application.Run;
end.

