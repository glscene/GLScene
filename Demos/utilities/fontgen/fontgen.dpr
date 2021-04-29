program fontgen;

uses
  Forms,
  fFontGen in 'fFontGen.pas' {FormFontGen};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormFontGen, FormFontGen);
  Application.Run;
end.
