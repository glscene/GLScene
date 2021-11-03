program FontgenD;

uses
  Forms,
  fFontgenD in 'fFontgenD.pas' {FormFontGen};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormFontGen, FormFontGen);
  Application.Run;
end.
