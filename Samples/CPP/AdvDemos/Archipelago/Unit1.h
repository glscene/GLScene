//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
#include <gl\gl.h>
#include <System.Classes.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Imaging.jpeg.hpp>
//---------------------------------------------------------------------------

#include "GLBaseClasses.hpp"
#include "GLBitmapFont.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLHeightData.hpp"
#include "GLHeightTileFileHDS.hpp"
#include "GLHUDObjects.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLSkydome.hpp"
#include "GLTerrainRenderer.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLWindowsFont.hpp"

#include "GLBaseClasses.hpp"
#include "GLBitmapFont.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLHeightData.hpp"
#include "GLHeightTileFileHDS.hpp"
#include "GLHUDObjects.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLSkydome.hpp"
#include "GLTerrainRenderer.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLWindowsFont.hpp"

#include "GLScene.hpp"
#include "GLCadencer.hpp"
#include "GLObjects.hpp"
#include "GLTerrainRenderer.hpp"
#include "GLHeightData.hpp"
#include "GLHeightTileFileHDS.hpp"
#include "GLColor.hpp"
#include "GLTexture.hpp"
#include "GLHUDObjects.hpp"
#include "GLMaterial.hpp"
#include "GLSkydome.hpp"
#include "GLWin32Viewer.hpp"
#include "GLWindowsFont.hpp"
#include "GLBitmapFont.hpp"
#include "GLCrossPlatform.hpp"
#include "GLCoordinates.hpp"
#include "GLRoamPatch.hpp"
#include "GLRenderContextInfo.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLBaseClasses.hpp"
#include "GLVectorLists.hpp"
#include "GLVectorTypes.hpp"
#include "GLVectorGeometry.hpp"

#include "GLKeyboard.hpp"
#include "GLContext.hpp"
#include "GLState.hpp"
#include "GLTextureFormat.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer;
	TPanel *PAProgress;
	TLabel *Label1;
	TProgressBar *ProgressBar;
	TGLScene *GLScene1;
	TGLSkyDome *SkyDome;
	TGLDummyCube *DCCamera;
	TGLCamera *GLCamera;
	TGLTerrainRenderer *TerrainRenderer;
	TGLDirectOpenGL *DOWake;
	TGLFreeForm *FFSailBoat;
	TGLLightSource *LSSun;
	TGLHUDText *HTFPS;
	TGLHUDText *HTHelp;
	TTimer *Timer1;
	TGLCadencer *GLCadencer;
	TGLMaterialLibrary *MaterialLibrary;
	TGLHeightTileFileHDS *GLHeightTileFileHDS1;
	TGLWindowsBitmapFont *BFSmall;
	TGLCustomHDS *GLCustomHDS1;
	TGLMaterialLibrary *MLSailBoat;
	TGLWindowsBitmapFont *BFLarge;
	void __fastcall GLCadencerProgress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall FormKeyPress(TObject *Sender, System::WideChar &Key);
	void __fastcall GLCustomHDS1MarkDirtyEvent(const TRect &area);
	void __fastcall GLCustomHDS1StartPreparingData(TGLHeightData *heightData);
	void __fastcall GLSceneViewerBeforeRender(TObject *Sender);
	void __fastcall DOWakeProgress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall DOWakeRender(TObject *Sender, TGLRenderContextInfo &rci);
	void __fastcall TerrainRendererHeightDataPostRender(TGLRenderContextInfo &rci, TList *&HeightDatas);
	void __fastcall FormCreate(TObject *Sender);

private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
	bool FullScreen;
	float CamHeight;
	int WaterPolyCount;
	bool WaterPlane;
	bool WasAboveWater;
	float HelpOpacity;

	TAffineVectorList *WakeVertices;
	TAffineVectorList *WakeStretch;
	TSingleList *WakeTime;
	void ResetMousePos(void);
	float WaterPhase(const float px, const float py);
	float WaterHeight(const float px, const float py);
	void IssuePoint(TGLHeightData *hd, int x, int y, int s2, float t, int rx, int ry);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
