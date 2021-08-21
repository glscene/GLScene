//---------------------------------------------------------------------------
/*: Simple TGLShader based multipass demo.

   This demo uses a custom TGLShader subclass to implement the classic
   multipass hidden lines rendering technique on a torus: first pass renders
   model with filled triangles, second pass does the wireframe.

   You'll also note the glPolygonOffset call, it displaces fragments depths
   value a little "farther away" so that surface fill depth values do not
   interact with the rendering of the lines (comment out the call and you'll
   see).<br>
   The axis and sphere allow you to see the limit of that simple technique:
   it actually "paints" between the lines, so you cannot use it to make
   transparent wireframed objects with hidden lines - if that thought ever
   blossomed in your mind ;)

   Additionnal objects around the show a glow/toon edges effect achieved in two
   passes too: the 1st pass activate lines and gives them a width, the second
   is used to fill the surface (and clear the lines that aren't on edges).
   (TOutLineShader thanks to Delauney Jerome, jdelauney@free.fr)
*/
#include <vcl.h>
#pragma hdrstop

#include "fMultipassC.h"
#include <assert.h>

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.GeomObjects"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.Texture"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Material"
#include "GLS.OpenGLTokens.hpp"

#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
__fastcall THiddenLineShader::THiddenLineShader(TComponent* AOwner)
        : TGLShader(AOwner)
{
}
//---------------------------------------------------------------------------
__fastcall THiddenLineShader::~THiddenLineShader(void)
{
}  
//---------------------------------------------------------------------------
void __fastcall THiddenLineShader::DoApply(TGLRenderContextInfo &rci, System::TObject* Sender)
{
  // new object getting rendered, 1st pass
  PassCount = 1;

  // backup state
  glPushAttrib(GL_ENABLE_BIT);
  // disable lighting, this is a solid fill
  glDisable(GL_LIGHTING);
  rci.GLStates->PolygonMode = GL_FRONT_AND_BACK, GL_FILL;
  // use background color
  glColor3fv(&BackgroundColor.X);
  // enable and adjust polygon offset
  glEnable(GL_POLYGON_OFFSET_FILL);
  glPolygonOffset(1, 2);
}
//---------------------------------------------------------------------------
bool __fastcall THiddenLineShader::DoUnApply(TGLRenderContextInfo &rci)
{
  bool result;

  switch (PassCount)
  {
    case 1 : {
       // 1st pass completed, we setup for the second
       PassCount = 2;

       // switch to wireframe and its color
       rci.GLStates->PolygonMode = GL_FRONT_AND_BACK, GL_LINE;
       glColor3fv(&LineColor.X);
       // disable polygon offset
       glDisable(GL_POLYGON_OFFSET_LINE);

       result = true;
       break;
    }
    case 2 : {
       // restore state
       glPopAttrib();

       // we're done
       result = false;
       break;
    }
    default : {
      // doesn't hurt to be cautious
      assert(false);
      result = false;
    }
  }
  return result;
}
//---------------------------------------------------------------------------
__fastcall TOutLineShader::TOutLineShader(TComponent* AOwner)
         : TGLShader(AOwner)
{
}
//---------------------------------------------------------------------------
__fastcall TOutLineShader::~TOutLineShader(void)
{
}
//---------------------------------------------------------------------------
void __fastcall TOutLineShader::DoApply(TGLRenderContextInfo &rci, System::TObject* Sender)
{
   PassCount = 1;
   glPushAttrib(GL_ENABLE_BIT);
   glDisable(GL_LIGHTING);

   if (OutlineSmooth)
   {
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
      glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
      glEnable(GL_LINE_SMOOTH);
   }
   else glDisable(GL_LINE_SMOOTH);

   glGetFloatv(GL_LINE_WIDTH,&OldlineWidth);
   glLineWidth(OutlineWidth);
   glPolygonMode(GL_BACK, GL_LINE);
   glCullFace(GL_FRONT);
   glDepthFunc(GL_LEQUAL);
   glColor3fv(&LineColor.X);
}
//---------------------------------------------------------------------------
bool __fastcall TOutLineShader::DoUnApply(TGLRenderContextInfo &rci)
{
  bool result;

  switch (PassCount)
  {
    case 1 : {
       PassCount=2;
       if (Lighting)
         glEnable(GL_LIGHTING);
       else glColor3fv(&BackgroundColor.X);
       glDepthFunc(GL_LESS);
       glCullFace(GL_BACK);
       glPolygonMode(GL_BACK, GL_FILL);

       result=true;
       break;
    }
    case 2 : {
       glPopAttrib();
       glLineWidth(OldlineWidth);
       result=false;
       break;
    }
    default : {
      assert(false);
      result=false;
    }
  }
  return result;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BUBindClick(TObject *Sender)
{
  THiddenLineShader *shader1;
  TOutLineShader *shader2 ,*shader3;

  BUBind->Enabled=False;

  // instantiate our shaders

  shader1 = new THiddenLineShader(this);
  shader1->BackgroundColor=ConvertWinColor(GLSceneViewer1->Buffer->BackgroundColor,0);
  shader1->LineColor=clrBlue;

  shader2 = new TOutLineShader(this);

  shader2->BackgroundColor=ConvertWinColor(GLSceneViewer1->Buffer->BackgroundColor,0);
  shader2->OutlineSmooth=true;
  shader2->OutlineWidth=2;
  shader2->Lighting=false;
  shader2->LineColor=clrBlack;


  shader3 = new TOutLineShader(this);
  shader3->BackgroundColor=ConvertWinColor(GLSceneViewer1->Buffer->BackgroundColor,0);
  shader3->OutlineSmooth=false;
  shader3->OutlineWidth=4;
  shader3->Lighting=true;
  shader3->LineColor=clrRed;

  // binds the shaders to the materials
  GLMaterialLibrary1->Materials->Items[0]->Shader=shader1;
  GLMaterialLibrary1->Materials->Items[1]->Shader=shader2;
  GLMaterialLibrary1->Materials->Items[2]->Shader=shader3;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
   mx=X; my=Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender,
      TShiftState Shift, int X, int Y)
{
   if (Shift.Contains(ssLeft))
      GLCamera1->MoveAroundTarget(my-Y, mx-X);
   else if (Shift.Contains(ssRight))
      GLCamera1->RotateTarget(my-Y, mx-X, 0);
   mx=X; my=Y;
}
//---------------------------------------------------------------------------
