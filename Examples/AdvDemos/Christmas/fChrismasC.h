//---------------------------------------------------------------------------

#ifndef fChrismasCH
#define fChrismasCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.Math.hpp>
#include <System.Math.Vectors.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Menus.hpp>

#include "GLS.BitmapFont.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.GeomObjects.hpp"
#include "GLS.HUDObjects.hpp"
#include "GLS.LensFlare.hpp"
#include "GLS.Material.hpp"
#include "GLS.Objects.hpp"
#include "GLS.ParticleFX.hpp"
#include "GLS.Scene.hpp"
#include "GLS.ShadowPlane.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.WindowsFont.hpp"
#include "GLS.ScreenSaver.hpp"
#include "GLS.FileWAV.hpp"
#include "Jpeg.hpp"
#include "Bass.Import.hpp"
#include "GLS.FileMP3.hpp"
#include "GLS.ThorFX.hpp"
#include "GLS.FireFX.hpp"
#include "GLS.BaseClasses.hpp"
#include "Stage.Utils.hpp"
#include "GLS.SoundManager.hpp"
#include "GLS.Sounds.BASS.hpp"

//-----------------

class TForm1 : public TForm
{
  __published: // IDE-managed Components
    TGLSceneViewer* Viewer;
    TGLSMBASS* GLSMBASS;
    TGLScene* Scene;
	TGLFreeForm *ffFirePlace;
    TGLDummyCube* DCFire;
    TGLLightSource* LSFire;
    TGLDummyCube* DCFireSource;
    TGLCylinder* CYLog;
	TGLDummyCube *dcFirTree;
	TGLDummyCube *dcTree;
	TGLFreeForm *ffFirTree;
	TGLProxyObject *poFirTree2;
	TGLProxyObject *poFirTree3;
	TGLCube *GLCube3;
    TGLCube* GLCube4;
	TGLDummyCube *dcDecoWhite;
    TGLProxyObject* POWhiteBall1;
    TGLProxyObject* POWhiteBall2;
    TGLProxyObject* POWhiteBall3;
	TGLDummyCube *dcDecoGold;
    TGLProxyObject* POGoldBall1;
    TGLProxyObject* POGoldBall2;
    TGLProxyObject* POGoldBall3;
	TGLDummyCube *dcLensFlares;
    TGLLensFlare* GLLensFlare1;
    TGLLensFlare* GLLensFlare2;
    TGLLensFlare* GLLensFlare3;
    TGLLensFlare* GLLensFlare4;
    TGLLensFlare* GLLensFlare5;
    TGLLensFlare* GLLensFlare6;
	TGLDummyCube *dcGifts;
    TGLCube* GLCube1;
    TGLCube* GLCube2;
    TGLShadowPlane* ShadowPlane;
	TGLFlatText *ftCountDown;
    TGLLensFlare* LSFireLens;
	TGLLightSource *LightSourceRoom;
	TGLDummyCube *dcCameraTarget;
    TGLParticleFXRenderer* ParticleFXRenderer;
	TGLDummyCube *dcBalls;
    TGLSphere* SPWhiteBall;
    TGLSphere* SPGoldBall;
    TGLHUDSprite* HUDSprite;
    TGLCamera* Camera;
    TTimer* Timer;
    TGLCadencer* Cadencer;
    TGLMaterialLibrary* MaterialLibrary;
    TGLPolygonPFXManager* PFXFire;
    TGLSoundLibrary* SoundLibrary;
    TGLPolygonPFXManager* PFXTree;
    TGLWindowsBitmapFont* WindowsBitmapFont;
    TGLScreenSaver* ScreenSaver;
	TGLFlatText *ftCongratulations;
	TGLFlatText *ftYear;
    TGLFireFXManager* GLFireFXManager1;
    //	TScreenSaver *ScreenSaver;
    void __fastcall FormCreate(TObject* Sender);
    void __fastcall ViewerMouseDown(
        TObject* Sender, TMouseButton Button, TShiftState Shift, int X, int Y);
    void __fastcall ViewerMouseMove(
        TObject* Sender, TShiftState Shift, int X, int Y);
    void __fastcall TimerTimer(TObject* Sender);
    void __fastcall CadencerProgress(
        TObject* Sender, const double deltaTime, const double newTime);
    void __fastcall FormResize(TObject* Sender);
    void __fastcall FormKeyPress(TObject* Sender, System::WideChar &Key);
    //	void __fastcall ScreenSaverCloseQuery(TObject *Sender, bool &CanClose);
    //	void __fastcall ScreenSaverPreview(TObject *Sender, HWND previewHwnd);
    //	void __fastcall ScreenSaverExecute(TObject *Sender);
    //	void __fastcall ScreenSaverPropertiesRequested(TObject *Sender);
    void __fastcall ViewerDblClick(TObject* Sender);
    void __fastcall ScreenSaverCloseQuery(TObject* Sender, bool &CanClose);
    void __fastcall ScreenSaverExecute(TObject* Sender);
    void __fastcall ScreenSaverPreview(TObject* Sender, HWND previewHwnd);
    void __fastcall FormMouseWheel(TObject* Sender, TShiftState Shift,
        int WheelDelta, TPoint &MousePos, bool &Handled);
  private: // User declarations
	int mx, my;
	float fireLight;
	bool inPreview, inSaver;
	Cardinal bStream;
	TFileName AssetPath;
  public: // User declarations
    __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1* Form1;
//---------------------------------------------------------------------------
#endif

