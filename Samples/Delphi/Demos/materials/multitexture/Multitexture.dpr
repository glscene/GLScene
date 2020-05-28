{: This sample mixes two textures by using multitexturing.

   Multitexturing requires at least two materials in the material library:<ul>
   <li>a base material: determines the basic properties, incl. blending, colors
      and lighting-related properties
   <li>a second material: determines the second texture (color and other
      properties are ignored)
   </ul>

   This structure allows reuse of second textures among a variety of materials,
   this is particularly usefull for details maps, which are usually just "noise"
   to be applied on different base textures. You can also use it to reuse a basic
   standard lighting map throughout many objects, thus reducing texture memory
   needs (many shadows can be derived from a few deformed basic maps).

   The texture matrix (scale, offset) are adjusted independantly for the two
   textures, in this sample, the TrackBar adjusts an isotropic scaling.

   When multi-texturing, never forget that both texture modes (decal, modulate etc.)
   are honoured. For instance, if you "Decal" a non-transparent second texture,
   the base texture will be completely replaced!
}
program Multitexture;

uses
  Forms,
  Main in 'Main.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
