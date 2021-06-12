{: Beer demo.

   The scene is defined in the DFM, the code only takes care of loading
   meshes, textures and reacting to mouse clicks.
   A PerlinPFX is used for the foam, a simpler PolyPFX for the bubbles.
   The glass effect uses a texture in sphere mapping mode, the grouping
   of faces in the 3DS model does the rest.

   Original idea by Vitomir Savic.
}
program Beer;

uses
  Forms,
  BeerFm in 'BeerFm.pas' {FormBeer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormBeer, FormBeer);
  Application.Run;
end.
