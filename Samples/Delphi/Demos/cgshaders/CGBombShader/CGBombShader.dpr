{: GL CgBomb Shader Demo.

  A demo that demostrates how to use the TGLCgBombShader component.

  Version history:
    22/12/12 - PW - Changed the path to media directory
    05/04/07 - DaStr - Initial version. Creation

}
program CGBombShader;

uses
  Forms,
  uMainForm in 'uMainForm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
