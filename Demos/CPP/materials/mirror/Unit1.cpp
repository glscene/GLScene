//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLWin32Viewer"
#pragma link "GLScene"
#pragma link "GLCadencer"
#pragma link "GLMirror"
#pragma link "GLObjects"
#pragma link "GLGeomObjects"
#pragma link "GLExtrusion"
#pragma link "GLMultiPolygon"
#pragma link "GLTeapot"
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma resource "*.dfm"
TForm1 *Form1;
int mx, my;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
}

//---------------------------------------------------------------------------

void __fastcall TForm1::CBOpaqueClick(TObject * Sender)
{
  if(CBOpaque->Checked)
    GLMirror1->MirrorOptions << moOpaque;
  else
    GLMirror1->MirrorOptions >> moOpaque;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::CBStencilClick(TObject * Sender)
{
  if(CBStencil->Checked)
    GLMirror1->MirrorOptions << moUseStencil;
  else
    GLMirror1->MirrorOptions >> moUseStencil;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::CBClearZClick(TObject * Sender)
{
  if(CBClearZ->Checked)
    GLMirror1->MirrorOptions << moClearZBuffer;
  else
    GLMirror1->MirrorOptions >> moClearZBuffer;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::CBPlaneClipClick(TObject * Sender)
{
  if(CBPlaneClip->Checked)
    GLMirror1->MirrorOptions << moMirrorPlaneClip;
  else
    GLMirror1->MirrorOptions >> moMirrorPlaneClip;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject * Sender)
{
  LabelFPS->Caption = GLSceneViewer1->FramesPerSecondText(1);
  GLSceneViewer1->ResetPerformanceMonitor();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject * Sender,
                                            const double deltaTime,
                                            const double newTime)
{
  GLSceneViewer1->Invalidate();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::FormResize(TObject * Sender)
{
  if(GLSceneViewer1->Width > GLSceneViewer1->Height)
    GLCamera1->SceneScale = (float)GLSceneViewer1->Height / 300;
  else
    GLCamera1->SceneScale = (float)GLSceneViewer1->Width / 360;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseDown(TObject * Sender,
                                                TMouseButton Button,
                                                TShiftState Shift, int X, int Y)
{
  mx = X;
  my = Y;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject * Sender,
                                                TShiftState Shift, int X, int Y)
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

