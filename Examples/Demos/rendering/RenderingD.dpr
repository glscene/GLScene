program RenderingD;

uses
  Vcl.Forms,
  fdRendering in 'fdRendering.pas' {FormRendering};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormRendering, FormRendering);
  Application.Run;
end.
