program GLSLshadersD;

uses
  Vcl.Forms,
  fdGLSLshaders in 'fdGLSLshaders.pas' {FormGLSLshaders};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormGLSLshaders, FormGLSLshaders);
  Application.Run;
end.
