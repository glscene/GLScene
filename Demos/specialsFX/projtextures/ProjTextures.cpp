/*: Projected Textures example.

   Controls:
	 Left Mouse Button: Rotate
	 Right Mouse Button: Adjust Distance
	 'S': Change styles

   The TGLProjectedTextures object can be used to simulate
   projected lights or other special effects. To use it, add
   it to the scene and set all objects that should receive
   the projected textures as children. Then, add a number
   of TGLTextureEmitter and load the texture that should
   be projected into it. Add this emitter to the "emitters"
   list of the projtex and that's it.

   There are two styles of projection: original and inverse.
   On the original method (developed by Tom Nuyens of
   www.delphi3d.net) the scene is rendered and the textures
   are projected into it. This is useful to simulate all
   kinds of special effects from bullet holes, to shadow
   maps (using tmBlend texture mode).

   On the inverse method, first all emitters are rendered,
   creating an "illumination mask". Then the scene is blended
   to this mask, so that only lit pixels are drawn. This can
   be used to create a projected-texture only illumination system.

*/
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEFORM("Unit1.cpp", Form1);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TForm1), &Form1);
		Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
