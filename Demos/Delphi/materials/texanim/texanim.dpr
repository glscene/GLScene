{: This samples shows how to use a material library for texture animation.

   Texture animation is best handled via material switching (from a material
   library). Directly updating the texture image can get slow because of the
   overhead induces by preparing and uploading the texture to the 3D board.

   In this sample, we prepare a set of textures in a material library,
   then simply switch between them in a cadencer's "Progress" event.
}
program texanim;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
