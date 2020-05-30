//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGeomObjects.hpp"
#include "GLObjects.hpp"
#include "GLParticleFX.hpp"
#include "GLPerlinPFX.hpp"
#include "GLScene.hpp"
#include "GLShadowPlane.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLFile3DS.hpp"
#include "GLFileJPEG.hpp"
#include "Jpeg.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLDummyCube *GLDummyCube1;
	TGLLightSource *GLLightSource1;
	TGLCylinder *GLCylinder1;
	TGLParticleFXRenderer *GLParticleFXRenderer2;
	TGLCylinder *GLCylinder2;
	TGLDummyCube *GLDummyCube3;
	TGLParticleFXRenderer *GLParticleFXRenderer1;
	TGLFreeForm *GLFreeForm1;
	TGLShadowPlane *GLShadowPlane1;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	TGLPerlinPFXManager *GLPerlinPFXManager1;
	TGLPolygonPFXManager *GLPolygonPFXManager1;
	void __fastcall FormActivate(TObject *Sender);
	void __fastcall GLSceneViewer1DblClick(TObject *Sender);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);

private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
