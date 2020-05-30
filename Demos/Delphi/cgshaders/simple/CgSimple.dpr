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
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
