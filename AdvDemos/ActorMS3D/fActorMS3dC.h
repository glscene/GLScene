//---------------------------------------------------------------------------

#ifndef fActorMS3dCH
#define fActorMS3dCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>

#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>

#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.FBORenderer.hpp"
#include "GLS.Material.hpp"
#include "GLS.Objects.hpp"

#include "GLS.ArchiveManager.hpp"
#include "GLS.FileZLIB.hpp"
#include "GLS.TextureFormat.hpp"

#include "GLS.Scene.hpp"

#include "GLS.VectorGeometry.hpp"

#include "GLS.SimpleNavigation.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.FileMS3D.hpp"
#include "GLS.FileJPEG.hpp"
#include "GLS.FilePNG.hpp"
#include "GLS.CompositeImage.hpp"
#include "GLSL.CustomShader.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TButton *Button2;
	TButton *btnStartStop;
	TButton *Button4;
	TComboBox *aniBox;
	TTrackBar *aniPos;
	TGLScene *GLScene1;
	TGLCamera *GLCamera1;
	TGLCamera *GLCamera2;
	TGLLightSource *Light2;
	TGLSphere *Globus;
	TGLFBORenderer *GLFrameBuffer;
	TGLDummyCube *Root;
	TGLLightSource *GLLightSource1;
	TGLDirectOpenGL *GLDirectOpenGL1;
	TGLActor *Actor1;
	TGLPlane *GLPlane1;
	TGLFreeForm *Chair1;
	TGLCadencer *GLCadencer1;
	TGLMaterialLibrary *MatLib;
	TGLSimpleNavigation *GLNavigation;
	TTimer *Timer1;
	TGLSArchiveManager *GLSArchiveManager1;
	void __fastcall Actor1EndFrameReached(TObject *Sender);
	void __fastcall aniBoxSelect(TObject *Sender);
	void __fastcall aniPosChange(TObject *Sender);
	void __fastcall btnStartStopClick(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall Button4Click(TObject *Sender);
	void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLDirectOpenGL1Render(TObject *Sender, TGLRenderContextInfo &rci);
	void __fastcall GLFrameBufferAfterRender(TObject *Sender, TGLRenderContextInfo &rci);
	void __fastcall GLFrameBufferBeforeRender(TObject *Sender, TGLRenderContextInfo &rci);
	void __fastcall Timer1Timer(TObject *Sender);
    void __fastcall LoadTexture(String AName, String ext);



private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
