program pdMovements;

uses
  Vcl.Forms,
  fdMovements in 'fdMovements.pas' {FormMovements};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMovements, FormMovements);
  Application.Run;
end.
