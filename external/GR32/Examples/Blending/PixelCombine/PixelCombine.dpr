program PixelCombine;

{$R 'Media.res' 'Media.rc'}

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FormPixelCombine};

begin
  Application.Initialize;
  Application.CreateForm(TFormPixelCombine, FormPixelCombine);
  Application.CreateForm(TFormPixelCombine, FormPixelCombine);
  Application.Run;
end.
