//---------------------------------------------------------------------------

#ifndef fOdeRagdollCH
#define fOdeRagdollCH
//---------------------------------------------------------------------------
#include <tchar.h>


#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.BitmapFont.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.HUDObjects.hpp"
#include "GLS.Material.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.ShadowPlane.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.WindowsFont.hpp"
#include "GLS.ODEManager.hpp"
#include "ODE.Import.hpp"
#include "GLS.ODERagdoll.hpp"
#include "Stage.Utils.hpp"

//---------------------------------------------------------------------------
//Physic World ODE
class PACKAGE TWorld_ODE : public TObject
{
	PdxWorld World;
	PdxSpace Space;
	TdJointGroupID *ContactGroup;
	PdxGeom Ground_box;
	PdxGeom Ground_box2;
	TGLCube *Cube;
	TGLCube *Cube2;
public:
	bool ODEEnable;
	double PhysTime;
	virtual __fastcall TWorld_ODE(TObject *AOwner); // constructor Create;
//	~TWorld_ODE(){}; // destructor Destroy;
	void _fastcall WorldUpdate();
};


//---------------------------------------------------------------------------
class TfRagDoll : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLDummyCube *ODEScene;
	TGLActor *Actor1;
	TGLDummyCube *Targetrag;
	TGLShadowPlane *GLShadowPlane1;
	TGLLightSource *GLLightSource1;
	TGLHUDText *GLHUDText1;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	TGLWindowsBitmapFont *GLWindowsBitmapFont1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
	int my,mx;
	TWorld_ODE *WorldODE;
	TGLODERagdoll *Rag;
	TGLODERagdollWorld *RagWorld;
	TGLODERagdollBone *HeadBone;
	TGLODERagdollBone *Spine;
	TGLODERagdollBone *Torso;
	TGLODERagdollBone *RightLeg;

public:		// User declarations
	__fastcall TfRagDoll(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfRagDoll *fRagDoll;
//---------------------------------------------------------------------------
#endif
