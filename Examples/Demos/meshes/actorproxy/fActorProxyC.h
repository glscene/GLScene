//---------------------------------------------------------------------------

#ifndef fActorProxyCH
#define fActorProxyCH
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
#include "Stage.VectorGeometry.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Texture.hpp"
#include "GLS.Material.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.BaseClasses.hpp"
#include "GLS.FileSMD.hpp"
#include "Stage.Utils.hpp"
#include "GLS.BaseClasses.hpp"

//---------------------------------------------------------------------------
class TFormActorProxy : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TCheckBox *chbActorsAreTurning;
	TGLScene *GLScene1;
	TGLDummyCube *dcInvisible;
	TGLActor *MasterActor;
	TGLDummyCube *dcShow;
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
	TCheckBox *chbShowMasterActor;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall Timer1Timer(TObject *Sender);

private:	// User declarations
	int mouseX,mouseY;
	void __fastcall DoRaycastStuff();

public:		// User declarations
	__fastcall TFormActorProxy(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormActorProxy *FormActorProxy;
//---------------------------------------------------------------------------
#endif
