//---------------------------------------------------------------------------

#ifndef fAtmosphereCH
#define fAtmosphereCH
//---------------------------------------------------------------------------
#include <tchar.h>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include "Vcl.imaging.JPeg.hpp"

#include "GLS.Scene.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.LensFlare.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.Texture.hpp"
#include "GLS.SkyDome.hpp"
#include "Stage.VectorGeometry.hpp"

#include "GLS.Atmosphere.hpp"
#include "GLS.SimpleNavigation.hpp"
#include "GLS.Behaviours.hpp"
#include "GLS.Coordinates.hpp"
#include "GLS.BaseClasses.hpp"
#include "GLS.Color.hpp"

//---------------------------------------------------------------------------
class TFormAtmosphere : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TButton *ButtonRotate;
	TButton *Button4;
	TButton *Button5;
	TButton *Button2;
	TButton *Button3;
	TButton *Button9;
	TButton *Button10;
	TGLScene *GLScene1;
	TGLSkyDome *GLSkyDome1;
	TGLDummyCube *CameraTarget;
	TGLDummyCube *World;
	TGLSphere *Not_a_Planet;
	TGLDummyCube *GLDummyCube1;
	TGLSphere *Planet;
	TGLLensFlare *GLLensFlare1;
	TGLLightSource *GLLightSource1;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	TGLSimpleNavigation *GLSimpleNavigation1;
	TButton *Button8;
	TButton *btnShowAtmosphere;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall ButtonRotateClick(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall Button3Click(TObject *Sender);
	void __fastcall Button4Click(TObject *Sender);
	void __fastcall Button5Click(TObject *Sender);
	void __fastcall btnShowAtmosphereClick(TObject *Sender);
	void __fastcall Button8Click(TObject *Sender);
	void __fastcall Button10Click(TObject *Sender);
	void __fastcall Button9Click(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);

private:	// User declarations
	TGLAtmosphere *Atmosphere;
public:		// User declarations
	__fastcall TFormAtmosphere(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormAtmosphere *FormAtmosphere;
//---------------------------------------------------------------------------
#endif
