//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
//#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLLensFlare.hpp"
#include "GLMaterial.hpp"
#include "GLNavigator.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLSimpleNavigation.hpp"
#include "GLSkyBox.hpp"
#include "GLWin32Viewer.hpp"
#include "GLKeyBoard.hpp"
#include "JPeg.hpp"
#include "GLFileJPEG.hpp"
#include "GLUtils.hpp"
#include <System.Classes.hpp>

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLSkyBox *GLSkyBox1;
	TGLSphere *GLSphere1;
	TGLSphere *GLSphere2;
	TGLSkyBox *GLSkyBox2;
	TGLDummyCube *Castle;
	TGLCube *GLCube1;
	TGLCube *GLCube2;
	TGLCube *GLCube11;
	TGLCube *GLCube21;
	TGLCube *GLCube111;
	TGLCube *GLCube211;
	TGLCube *GLCube112;
	TGLCube *GLCube212;
	TGLLightSource *GLLightSource1;
	TGLLensFlare *GLLensFlare1;
	TGLCamera *GLCamera1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TGLNavigator *GLNavigator1;
	TGLCadencer *GLCadencer1;
	TGLUserInterface *GLUserInterface1;
	TGLSimpleNavigation *GLSimpleNavigation1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);

private:	// User declarations
   int mx, my;
   void __fastcall HandleKeys(double d);
   TGLLibMaterial* __fastcall LoadTexture(String Matname, String Filename);
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
