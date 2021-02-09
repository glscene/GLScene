{: Showcases some of the standard texture formats & compression.

   TextureFormat and Compression are directly controled by the similarly named
   properties of a TGLTexture. You can also control this globally though
   the "default" values.

   For texture compression to work... it must be supported by your 3D card,
   so older boards may not see any change when applying texture compression
   (the setting is ignored by GLScene if unsupported).
   Alternatively, some OpenGL ICD will use only one or two compression formats,
   and you may not see any difference between the standard/fastest/nicest
   compression modes.

   Texture compression is fast, saves memory and helps maintaining a good
   fillrate (by reduceing texture trashing and required bandwidth), so,
   as long as the compression artifacts do not show up too much on your
   textures, use it!
}
program TexFormat;

uses
  Forms,
  TexFormatFm in 'TexFormatFm.pas' {FormTexFormat};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTexFormat, FormTexFormat);
  Application.Run;
end.
