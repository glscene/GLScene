program Viewports;

uses
  System.StartUpCopy,
  FMX.Forms,
  fdViewports in 'fdViewports.pas' {FormViewports};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormViewports, FormViewports);
  Application.Run;
end.
