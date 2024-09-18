//---------------------------------------------------------------------------

#ifndef fMirrorCH
#define fMirrorCH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "GLS.SceneViewer.hpp"
#include "GLS.Scene.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Mirror.hpp"
#include "GLS.Objects.hpp"
#include "GLS.GeomObjects.hpp"
#include "GLS.Extrusion.hpp"
#include "GLS.MultiPolygon.hpp"
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"

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

