program ObjectMatsD;

uses
  Vcl.Forms,
  fObjectMatsD in 'fObjectMatsD.pas' {FormMO};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMO, FormMO);
  Application.Run;
end.
