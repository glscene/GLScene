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
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *Viewer;
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *LabelFPS;
	TButton *Button1;
	TCheckBox *CheckBox1;
	TCheckBox *CheckBox2;
	TGLScene *GLScene;
	TGLCamera *GLCamera1;
	TGLLightSource *GLLightSource1;
	TGLLightSource *GLLightSource2;
	TGLDummyCube *GLDummyCube1;
	TGLDummyCube *DCCamTarg;
	TGLDummyCube *DCCube1;
	TGLCube *GLCube1;
	TGLLines *GLLines1;
	TGLPoints *GLPoints1;
	TGLCadencer *GLCadencer;
	TTimer *Timer1;
	TGLMaterialLibrary *GLMaterialLibrary;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall CheckBox1Click(TObject *Sender);
	void __fastcall GLCadencerProgress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall ViewerMouseMove(TObject *Sender, TShiftState Shift, int X, int Y);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);
	void __fastcall ViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall FormResize(TObject *Sender);
	void __fastcall FormKeyPress(TObject *Sender, System::WideChar &Key);


private:	// User declarations
	int mdx, mdy;
	TAffineVector BoxPos, BoxScale,	RayStart, RayDir;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
