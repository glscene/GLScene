//---------------------------------------------------------------------------

#ifndef fBeerCH
#define fBeerCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.GeomObjects.hpp"
#include "GLS.Objects.hpp"
#include "GLS.ParticleFX.hpp"
#include "GLS.PerlinPFX.hpp"
#include "GLS.Scene.hpp"
#include "GLS.ShadowPlane.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.File3DS.hpp"
#include "GLS.FileJPEG.hpp"
#include "Jpeg.hpp"
#include "Stage.Utils.hpp"


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
