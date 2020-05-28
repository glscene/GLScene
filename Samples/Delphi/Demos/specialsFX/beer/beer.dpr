{: Beer demo.

   The scene is defined in the DFM, the code only takes care of loading
   meshes, textures and reacting to mouse clicks.<br>
   A PerlinPFX is used for the foam, a simpler PolyPFX for the bubbles.
   The glass effect uses a texture in sphere mapping mode, the grouping
   of faces in the 3DS model does the rest.

   Original idea by Vitomir Savic.
}
program beer;

uses
  Forms,
  unit1 in 'unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
