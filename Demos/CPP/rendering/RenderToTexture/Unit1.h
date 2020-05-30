//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLFBORenderer.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLSimpleNavigation.hpp"
#include "GLWin32Viewer.hpp"
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *Label1;
	TCheckBox *CheckBox1;
	TRadioGroup *SB;
	TRadioGroup *RB;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLDirectOpenGL *GLDirectOpenGL1;
	TGLFBORenderer *GLFBORenderer1;
	TGLFBORenderer *GLFBORenderer2;
	TGLCube *GLCube1;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TTimer *Timer1;
	TGLSimpleNavigation *GLSimpleNavigation1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall CheckBox1Click(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLDirectOpenGL1Render(TObject *Sender, TGLRenderContextInfo &rci);
	void __fastcall GLFBORenderer1AfterRender(TObject *Sender, TGLRenderContextInfo &rci);
	void __fastcall GLFBORenderer2AfterRender(TObject *Sender, TGLRenderContextInfo &rci);
	void __fastcall RBClick(TObject *Sender);
	void __fastcall SBClick(TObject *Sender);




private:	// User declarations
	bool Triger;
	int FramerateRatio, N;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
