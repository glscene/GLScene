//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "GLWin32Viewer.hpp"
#include "GLScene.hpp"
#include "GLCadencer.hpp"
#include "GLMirror.hpp"
#include "GLObjects.hpp"
#include "GLGeomObjects.hpp"
#include "GLExtrusion.hpp"
#include "GLMultiPolygon.hpp"
#include "GLTeapot.hpp"
#include "GLBaseClasses.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
//---------------------------------------------------------------------------
class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TPanel * Panel1;
  TGLSceneViewer *GLSceneViewer1;
  TGLScene *GLScene1;
  TCheckBox *CBOpaque;
  TCheckBox *CBStencil;
  TCheckBox *CBClearZ;
  TCheckBox *CBPlaneClip;
  TGLCadencer *GLCadencer1;
  TTimer *Timer1;
  TGLCamera *GLCamera1;
  TGLLightSource *GLLightSource1;
  TGLMirror *GLMirror1;
  TGLDummyCube *ReflectingObjects;
  TGLDummyCube *NonReflectingObjects;
  TGLCylinder *GLCylinder1;
  TGLCylinder *Cylinder2;
  TGLTorus *Torus1;
  TGLExtrusionSolid *Cadre;
  TGLSphere *GLSphere1;
  TGLCylinder *GLCylinder2;
  TGLTeapot *GLTeapot1;
	TLabel *LabelFPS;
  void __fastcall CBOpaqueClick(TObject * Sender);
  void __fastcall CBStencilClick(TObject * Sender);
  void __fastcall CBClearZClick(TObject * Sender);
  void __fastcall CBPlaneClipClick(TObject * Sender);
  void __fastcall Timer1Timer(TObject * Sender);
  void __fastcall GLCadencer1Progress(TObject * Sender, const double deltaTime,
                                      const double newTime);
  void __fastcall FormResize(TObject * Sender);
  void __fastcall GLSceneViewer1MouseDown(TObject * Sender, TMouseButton Button,
                                          TShiftState Shift, int X, int Y);
  void __fastcall GLSceneViewer1MouseMove(TObject * Sender, TShiftState Shift,
										  int X, int Y);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);
private:                       // User declarations
public:                        // User declarations
    __fastcall TForm1(TComponent * Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif

