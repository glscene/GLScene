program TransparencyAdvanced;

uses
  Forms,
  uDemo in 'uDemo.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
{$IFDEF DELPHI2009UP}
  Application.MainFormOnTaskbar := True;
{$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
