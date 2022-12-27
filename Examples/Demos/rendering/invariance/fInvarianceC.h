//---------------------------------------------------------------------------

#ifndef fInvarianceCH
#define fInvarianceCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Imaging.jpeg.hpp>

#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.GeomObjects.hpp"
#include "GLS.Material.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.Utils.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLDummyCube *DCCamera;
	TGLCamera *GLCamera;
	TGLPlane *PLGround;
	TGLCube *GLCube1;
	TGLLightSource *GLLightSource1;
	TGLDummyCube *DCPositionInvariant;
	TGLCylinder *GLCylinder1;
	TGLDummyCube *DCOrientationInvariant;
	TGLArrowLine *GLArrowLine1;
	TGLMaterialLibrary *GLMaterialLibrary;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
		  int Y);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
		  int X, int Y);
private:	// User declarations
    int mx, my;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
