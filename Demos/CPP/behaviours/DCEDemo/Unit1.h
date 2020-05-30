//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <vcl.h>
#include <tchar.h>
#include <gl.h>
#include <glext.h>

//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Imaging.Jpeg.hpp>

#include "GLScene.hpp"
#include "GLObjects.hpp"
#include "GLCadencer.hpp"
#include "GLWin32Viewer.hpp"
#include "GLDCE.hpp"
#include "GLMaterial.hpp"
#include "GLTexture.hpp"
#include "GLHeightData.hpp"
#include "GLTerrainRenderer.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLBitmapFont.hpp"
#include "GLWindowsFont.hpp"
#include "GLHUDObjects.hpp"
#include "GLCrossPlatform.hpp"
#include "GLCoordinates.hpp"
#include "GLBaseClasses.hpp"
#include "GLRenderContextInfo.hpp"
#include "GLKeyBoard.hpp"
#include "GLState.hpp"
#include "GLVectorGeometry.hpp"
#include "GLContext.hpp"
#include "GLEllipseCollision.hpp"
#include "GLUtils.hpp"


//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource2;
	TGLTerrainRenderer *Terrain;
	TGLPlane *Ground;
	TGLDummyCube *Balls;
	TGLFreeForm *moMushroom;
	TGLDummyCube *Mushrooms;
	TGLCube *GLCube1;
	TGLDirectOpenGL *GLDirectOpenGL1;
	TGLDummyCube *Player;
	TGLCamera *GLCamera1;
	TGLLightSource *GLLightSource1;
	TGLActor *GLActor1;
	TGLSphere *GLSphere1;
	TGLHUDText *GLHUDText1;
	TGLHUDText *HelpShadow;
	TGLHUDText *Help;
	TGLCadencer *GLCadencer1;
	TGLDCEManager *GLDCEManager1;
	TGLBitmapHDS *GLBitmapHDS1;
	TGLWindowsBitmapFont *GLWindowsBitmapFont1;
	TTimer *Timer1;
	TGLMaterialLibrary *GLMatLib;
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall PlayerBehaviours0Collision(TObject *Sender,
	  TGLBaseSceneObject *ObjectCollided, TDCECollision &CollisionInfo);
	void __fastcall GLDirectOpenGL1Render(TObject *Sender, TGLRenderContextInfo &rci);
	void __fastcall FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift);


private:	// User declarations
	int mx, my;
	bool Jumped;
	void Load();
	void HandleKeys();
	void HandleAnimation();
	void AddBall();
	void AddMushrooms();

public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
