program FluidsD;

uses
  Forms,
  fFluidsD in 'fFluidsD.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
