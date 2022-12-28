{ Simple Cg Shader Cubemap Demo (incomplete)}
program CgReflect;


uses
  Forms,
  fReflect in 'fReflect.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
