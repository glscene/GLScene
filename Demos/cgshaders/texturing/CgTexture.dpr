{
  Cg Multi-Texturing Demo
  Shows how to do texture coordinate shifting with a VP and blending with a FP.
  Last update: 09/02/04
  Nelson Chu
}

program CgTexture;



uses
  Forms,
  fCgTexture in 'fCgTexture.pas' {FormCgTexture};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormCgTexture, FormCgTexture);
  Application.Run;
end.
