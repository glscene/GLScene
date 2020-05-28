//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <tchar.h>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLAsyncTimer.hpp"
#include "GLBaseClasses.hpp"
#include "GLBumpShader.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "OpenGLAdapter.hpp"
#include "OpenGL1x.hpp"
#include "JPeg.hpp"
#include "GLAsyncTimer.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *LabelFPS;
	TComboBox *ComboBox1;
	TGroupBox *GroupBox1;
	TShape *Shape1;
	TShape *Shape2;
	TShape *Shape3;
	TCheckBox *CheckBox1;
	TCheckBox *CheckBox2;
	TCheckBox *CheckBox3;
	TCheckBox *CheckBox4;
	TComboBox *ComboBox2;
	TGLScene *GLScene1;
	TGLDummyCube *DCLights;
	TGLLightSource *WhiteLight;
	TGLLightSource *RedLight;
	TGLLightSource *BlueLight;
	TGLFreeForm *Bunny;
	TGLCamera *Camera;
	TGLCadencer *GLCadencer1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TGLBumpShader *GLBumpShader1;
	TColorDialog *ColorDialog1;
	TGLAsyncTimer *AsyncTimer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall CheckBox1Click(TObject *Sender);
	void __fastcall Shape1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall ComboBox1Change(TObject *Sender);
	void __fastcall CheckBox2Click(TObject *Sender);
	void __fastcall CheckBox3Click(TObject *Sender);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall AsyncTimer1Timer(TObject *Sender);
	void __fastcall FormResize(TObject *Sender);
	void __fastcall GLSceneViewer1BeforeRender(TObject *Sender);
	void __fastcall ComboBox2Change(TObject *Sender);
	void __fastcall Shape2MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall Shape3MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);


private:	// User declarations
	int mx, my, dx, dy;
	bool IsInitialized;
	int StartHeight;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
