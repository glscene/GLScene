{
  Cg Multi-Texturing Demo
  Shows how to do texture coordinate shifting with a VP and blending with a FP.
  Last update: 09/02/04
  Nelson Chu
}

program CgTexture;



uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
