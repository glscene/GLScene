// The ThorFX special effect basic sample by René Lindsay. 10/3/2001
// The code is largely based on the FireFX code.

program Thor;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
