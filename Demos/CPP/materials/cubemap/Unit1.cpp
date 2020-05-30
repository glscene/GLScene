//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLTeapot"
#pragma link "GLWin32Viewer"
#pragma link "GLTexture"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1BeforeRender(TObject *Sender)
{
  CubmapSupported = !GL_ARB_texture_cube_map;
  GLSceneViewer1->BeforeRender = NULL;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  // Cube map warning message
  // If you don't check and turn off cube maps yourself in your apps when
  // cube maps aren't supported, GLScene will just turn off texturing
  // (ie. no error generated, just a different output)
  if (!CubmapSupported)
  {
	ShowMessage("Your graphics board does not support cube maps...");
	exit;
  }
  // Our cube map images are here
  SetGLSceneMediaDir();
	// We need a CubeMapImage, which unlike the "regular Images" stores
	// multiple images.

  //with Teapot1->Material->Texture->
  Teapot1->Material->Texture->ImageClassName =
		__classid(TGLCubeMapImage)->ClassName();
  TGLCubeMapImage *Image = (TGLCubeMapImage *) Teapot1->Material->Texture->Image;

  // Load all 6 texture map components of the cube map
  // The 'PX', 'NX', etc. refer to 'positive X', 'negative X', etc.
  // and follow the RenderMan specs/conventions
  Image->Picture[CmtPX]->LoadFromFile("cm_left.jpg");
  Image->Picture[CmtNX]->LoadFromFile("cm_right.jpg");
  Image->Picture[CmtPY]->LoadFromFile("cm_top.jpg");
  Image->Picture[CmtNY]->LoadFromFile("cm_bottom.jpg");
  Image->Picture[CmtPZ]->LoadFromFile("cm_back.jpg");
  Image->Picture[CmtNZ]->LoadFromFile("cm_front.jpg");
	// Select reflection cube map environment mapping
	// This is the mode you'll most commonly use with cube maps, normal cube
	// map generation is also supported (used for diffuse environment lighting)
  Teapot1->Material->Texture->MappingMode = tmmCubeMapReflection;
	// That's all folks, let us see the thing!
  Teapot1->Material->Texture->Disabled = false;
  Button1->Visible = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
   if (Shift.Contains(ssLeft))
	  GLCamera1->MoveAroundTarget(my-Y, mx-X);
   else if (Shift.Contains(ssRight))
	  GLCamera1->RotateTarget(my-Y, mx-X, 0);
   mx=X; my=Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
  GLCamera1->
   AdjustDistanceToTarget(Power(1.1, (WheelDelta / 120.0)));
}
//---------------------------------------------------------------------------
