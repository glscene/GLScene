program TransparAdvD;

uses
  Forms,
  fTransparAdvD in 'fTransparAdvD.pas' {FormTransparAdv};

{$R *.res}

begin
  Application.Initialize;
{$IFDEF DELPHI2009UP}
  Application.MainFormOnTaskbar := True;
{$ENDIF}
  Application.CreateForm(TFormTransparAdv, FormTransparAdv);
  Application.Run;
end.
