//---------------------------------------------------------------------------

#ifndef fMain1CH
#define fMain1CH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Behaviours.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.GeomObjects.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.ScreenSaver.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLDummyCube *DummyCube1;
	TGLDummyCube *DummyCube2;
	TGLDummyCube *DummyCube3;
	TGLTorus *Torus1;
	TGLDummyCube *DummyCube4;
	TGLLightSource *GLLightSource1;
	TGLLightSource *GLLightSource2;
	TGLLightSource *GLLightSource3;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	TGLScreenSaver *GLScreenSaver1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall FormResize(TObject *Sender);
	void __fastcall GLScreenSaver1PropertiesRequested(TObject *Sender);
   //	void __fastcall ScreenSaver1PropertiesRequested(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
