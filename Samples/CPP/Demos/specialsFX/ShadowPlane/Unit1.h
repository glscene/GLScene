//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <tchar.h>
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGeomObjects.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLShadowPlane.hpp"
#include "GLWin32Viewer.hpp"
#include "GLUtils.hpp"
#include "JPeg.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TCheckBox *CBShadows;
	TCheckBox *CBStencil;
	TGLScene *GLScene1;
	TGLDummyCube *DCShadowing;
	TGLCube *Cube1;
	TGLSphere *Sphere1;
	TGLTorus *Torus1;
	TGLDummyCube *DCLight;
	TGLLightSource *GLLightSource1;
	TGLSphere *Sphere2;
	TGLDummyCube *DCCameraTarget;
	TGLCamera *GLCamera1;
	TGLShadowPlane *GLShadowPlane1;
	TGLShadowPlane *GLShadowPlane2;
	TGLShadowPlane *GLShadowPlane3;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	TGLMaterialLibrary *GLMaterialLibrary;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall CBShadowsClick(TObject *Sender);
	void __fastcall CBStencilClick(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);

private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
