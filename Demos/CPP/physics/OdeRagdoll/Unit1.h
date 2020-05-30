//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <tchar.h>


#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLBaseClasses.hpp"
#include "GLBitmapFont.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLHUDObjects.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLShadowPlane.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLWindowsFont.hpp"
#include "ODEUtils.hpp"
#include "ODEImport.hpp"
#include "GLODERagdoll.hpp"

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
