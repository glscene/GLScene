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
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include <Vcl.ExtCtrls.hpp>
#include "GLBlur.hpp"
#include "JPeg.hpp"

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
	TComboBox *ComboBox2;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLCube *GLCube1;
	TGLSphere *GLSphere1;
	TGLDummyCube *GLDummyCube1;
	TGLCamera *GLCamera1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ComboBox1Change(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall ComboBox2Change(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);

private:	// User declarations
	int oldx, oldy;
	TGLBlur *B;

public:		// User declarations
	__fastcall TForm1(TComponent* Owner);

};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
