program BenchD;

uses
  Vcl.Forms,
  fBenchD in 'fBenchD.pas' {FormBenchD};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormBenchD, FormBenchD);
  Application.Run;
end.
