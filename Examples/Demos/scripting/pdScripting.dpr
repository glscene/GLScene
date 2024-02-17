program pdScripting;

uses
  Vcl.Forms,
  fdScripting in 'fdScripting.pas' {FormScripting};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormScripting, FormScripting);
  Application.Run;
end.
