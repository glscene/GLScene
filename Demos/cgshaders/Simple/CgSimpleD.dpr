{
  Simple Cg Shader Demo
  Tested with Cg 1.1
  Try to get an ATI feel by editing the vertex shader code. ;)
  Last update: 20/01/04
  Nelson Chu
}
program CgSimpleD;



uses
  Forms,
  fCgSimpleD in 'fCgSimpleD.pas' {FormCgSimple};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormCgSimple, FormCgSimple);
  Application.Run;
end.
