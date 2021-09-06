program SvgPath;

uses
  Forms,
  fSvgPath in 'fSvgPath.pas' {FrmSvgPathRenderer};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmSvgPathRenderer, FrmSvgPathRenderer);
  Application.Run;
end.
