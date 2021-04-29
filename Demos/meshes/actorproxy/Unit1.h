//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <vcl.h>
#include <tchar.h>
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Imaging.jpeg.hpp>

#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.Objects.hpp"
#include "GLS.GeomObjects.hpp"
#include "GLS.ProxyObjects.hpp"
#include "GLS.VectorGeometry.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Texture.hpp"
#include "GLS.Material.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.BaseClasses.hpp"
#include "GLS.FileSMD.hpp"
#include "GLS.Utils.hpp"
#include "GLS.BaseClasses.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TCheckBox *cbActorsAreTurning;
	TGLScene *GLScene1;
	TGLDummyCube *InvisibleDummyCube;
	TGLActor *MasterActor;
	TGLDummyCube *GLDummyCube2;
	TGLActorProxy *GLActorProxy1;
	TGLArrowLine *GLArrowLine1;
	TGLActorProxy *GLActorProxy2;
	TGLArrowLine *GLArrowLine2;
	TGLSphere *GLSphere1;
	TGLArrowLine *GLArrowLine3;
	TGLCamera *GLCamera1;
	TGLLightSource *GLLightSource1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall Timer1Timer(TObject *Sender);

private:	// User declarations
	int mouseX,mouseY;
	void __fastcall DoRaycastStuff();

public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
