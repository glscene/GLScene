program PostProcessingD;

uses
  Vcl.Forms,
  fPostProcessingD in 'fPostProcessingD.pas' {FormPP};

{$R *.res}

begin
  //ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormPP, FormPP);
  Application.Run;
end.
