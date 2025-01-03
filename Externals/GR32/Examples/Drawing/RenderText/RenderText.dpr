program RenderText;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  fRenderText in 'fRenderText.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFormRenderText, FormRenderText);
  Application.Run;
end.
