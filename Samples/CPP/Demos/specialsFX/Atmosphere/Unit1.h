//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <tchar.h>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLScene.hpp"
#include "GLObjects.hpp"
#include "GLCadencer.hpp"
#include "GLLensFlare.hpp"
#include "GLWin32Viewer.hpp"
#include "GLTexture.hpp"
#include "GLSkydome.hpp"
#include "GLVectorGeometry.hpp"
#include "GLCrossPlatform.hpp"
#include "GLAtmosphere.hpp"
#include "GLSimpleNavigation.hpp"
#include "GLBehaviours.hpp"
#include "JPeg.hpp"
#include "GLCoordinates.hpp"
#include "GLBaseClasses.hpp"
#include "GLColor.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TButton *Button1;
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
	TGLSphere *Not_a_planet;
	TGLDummyCube *GLDummyCube1;
	TGLSphere *GLSphere1;
	TGLLensFlare *GLLensFlare1;
	TGLLightSource *GLLightSource1;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	TGLSimpleNavigation *GLSimpleNavigation1;
	TButton *Button8;
	TButton *Button6;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall Button3Click(TObject *Sender);
	void __fastcall Button4Click(TObject *Sender);
	void __fastcall Button5Click(TObject *Sender);
	void __fastcall Button6Click(TObject *Sender);
	void __fastcall Button8Click(TObject *Sender);
	void __fastcall Button10Click(TObject *Sender);
	void __fastcall Button9Click(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);

private:	// User declarations
	TGLAtmosphere *Atmosphere;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
