program Resamplers;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FrmResamplersExample};

begin
  Application.Initialize;
  Application.CreateForm(TFrmResamplersExample, FrmResamplersExample);
  Application.CreateForm(TFrmResamplersExample, FrmResamplersExample);
  Application.Run;
end.
