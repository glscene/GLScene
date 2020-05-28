//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGeomObjects"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLSkydome"
#pragma link "GLTeapot"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}

//---------------------------------------------------------------------------
void __fastcall TForm1::GenerateCubeMap()
{
   // Don't do anything if cube maps aren't supported
   if (!CubmapSupported) {
	  if (!CubeMapWarnDone)
		 ShowMessage("Your graphics hardware does not support cube maps...");
	  CubeMapWarnDone = true;
	  exit;
   }
   // Here we generate the new cube map, from CubeMapCamera (a child of the
   // teapot in the scene hierarchy)
   // hide the teapot while rendering the cube map
	  Teapot1->Visible = false;
	  // render cube map to the teapot's texture
	  GLMemoryViewer1->RenderCubeMapTextures(Teapot1->Material->Texture);
	  // teapot visible again
	  Teapot1->Material->Texture->Disabled = false;
	  Teapot1->Visible = true;

}

//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
   if (CBDynamic->Checked) {
	  // make things move
	  Teapot1->Position->Y = 2*Sin(newTime);
	  Torus1->RollAngle = newTime*15;
	  // generate the cube map
	  GenerateCubeMap();
   }
   GLSceneViewer1->Invalidate();

}
//---------------------------------------------------------------------------

// Standard issue mouse movements

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
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  LabelFPS->Caption = GLSceneViewer1->FramesPerSecondText();
  GLSceneViewer1->ResetPerformanceMonitor();
}

//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1BeforeRender(TObject *Sender)
{
  CubmapSupported = !GL_ARB_texture_cube_map;
  GLSceneViewer1->BeforeRender = NULL;
}
//---------------------------------------------------------------------------
