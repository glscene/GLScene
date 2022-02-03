//---------------------------------------------------------------------------

#ifndef fArchipelagoCH
#define fArchipelagoCH

#include <System.Classes.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <System.Classes.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Imaging.jpeg.hpp>

#include "GLS.BaseClasses.hpp"
#include "GLS.BitmapFont.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"
#include "GLS.HeightData.hpp"
#include "GLS.HeightTileFileHDS.hpp"
#include "GLS.HUDObjects.hpp"
#include "GLS.Material.hpp"

#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.SkyDome.hpp"
#include "GLS.TerrainRenderer.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.WindowsFont.hpp"

//---------------------------------------------------------------------------

#include "GLS.RoamPatch.hpp"
#include "GLS.RenderContextInfo.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.BaseClasses.hpp"
#include "GLS.VectorLists.hpp"
#include "GLS.VectorTypes.hpp"
#include "GLS.VectorGeometry.hpp"

#include "GLS.Keyboard.hpp"
#include "GLS.Context.hpp"
#include "GLS.State.hpp"
#include "GLS.TextureFormat.hpp"
#include "GLS.File3DS.hpp"

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

	TGLAffineVectorList *WakeVertices;
	TGLAffineVectorList *WakeStretch;
	TGLSingleList *WakeTime;
	void ResetMousePos(void);
	float WaterPhase(const float px, const float py);
	float WaterHeight(const float px, const float py);
	void IssuePoint(TGLHeightData *hd, int x, int y, int s2, float t, int rx, int ry);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
