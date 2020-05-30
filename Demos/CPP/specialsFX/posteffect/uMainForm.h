//---------------------------------------------------------------------------

#ifndef uMainFormH
#define uMainFormH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMaterial.hpp"
#include "GLPostEffects.hpp"
#include "GLScene.hpp"
#include "GLSimpleNavigation.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
	TLabel *Label1;
	TPanel *Panel1;
	TLabel *Label2;
	TComboBox *ComboBox1;
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLActor *GLActor1;
	TGLPostEffect *GLPostEffect1;
	TGLLightSource *GLLightSource1;
	TGLCamera *GLCamera1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TGLCadencer *GLCadencer1;
	TGLSimpleNavigation *GLSimpleNavigation1;
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ComboBox1Change(TObject *Sender);
	void __fastcall GLPostEffect1CustomEffect(TObject *Sender, TGLRenderContextInfo &rci,
          TGLPostEffectBuffer &Buffer);

private:	// User declarations
public:		// User declarations
	__fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
