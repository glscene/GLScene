program FFT_Demo;

uses
  Forms,
  UnitMain in 'UnitMain.pas' {Form1},
  CPUFFT in 'CPUFFT.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
