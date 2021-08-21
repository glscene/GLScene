//---------------------------------------------------------------------------
#ifndef fOctreeRenderCH
#define fOctreeRenderCH
//---------------------------------------------------------------------------
#include <vcl.h>
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Material.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SimpleNavigation.hpp"
#include "GLS.SceneViewer.hpp"
#include <System.Classes.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.StdCtrls.hpp>

#include "GLS.SpacePartition.hpp"
#include "GLS.Context.hpp"
#include "GLS.VectorGeometry.hpp"
#include "GLS.OpenGLTokens.hpp"


//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *Label3;
	TLabel *Label2;
	TLabel *LabelCollisions;
	TTrackBar *TrackBar_LeafThreshold;
	TButton *Button_ResetOctreeSize;
	TGLScene *GLScene1;
	TGLDummyCube *GLDummyCube1;
	TGLDirectOpenGL *GLDirectOpenGL1;
	TGLCube *GLCube1;
	TGLSphere *GLSphere1;
	TGLPlane *GLPlane1;
	TGLLines *GLLines1;
	TGLCamera *GLCamera1;
	TGLLightSource *GLLightSource1;
	TGLCadencer *GLCadencer1;
	TGLSimpleNavigation *GLSimpleNavigation1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall TrackBar_LeafThresholdChange(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall Button_ResetOctreeSizeClick(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLDirectOpenGL1Render(TObject *Sender, TGLRenderContextInfo &rci);


private:	// User declarations
	TOctreeSpacePartition *Octree;
	void __fastcall CreateBox();
	void __fastcall VerifySpacialMisc();
	void __fastcall RenderAABB(TAABB* AABB, float w, float r, float g, float b);
	void __fastcall RenderOctreeNode(TSectorNode* Node);

public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};

class TGLSpacePartitionLeaf : public TSpacePartitionLeaf
{
__published:	// IDE-managed Components
	TGLBaseSceneObject *GLBaseSceneObject;
	void __fastcall UpdateCachedAABBAndBSphere(); //override;
	virtual __fastcall CreateGLOwned(TBaseSpacePartition *SpacePartition, TGLBaseSceneObject *aGLBaseSceneObject);
private:	// User declarations
	TAffineVector Direction;
};

//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
