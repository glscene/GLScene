{
  Simple Cg Shader Demo
  Tested with Cg 1.1
  Try to get an ATI feel by editing the vertex shader code. ;)
  Last update: 20/01/04
  Nelson Chu
}
program CgSimple;



uses
  Forms,
  CgSimpleFm in 'CgSimpleFm.pas' {FormCgSimple};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormCgSimple, FormCgSimple);
  Application.Run;
end.
