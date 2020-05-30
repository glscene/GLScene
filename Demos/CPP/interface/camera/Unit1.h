//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <math.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLTeapot.hpp"
#include "GLWin32Viewer.hpp"
#include "GLKeyboard.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TRadioGroup *RadioGroup1;
	TRadioGroup *RadioGroup2;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLTeapot *Teapot1;
	TGLDummyCube *DummyCube1;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	void __fastcall FormKeyPress(TObject *Sender, System::WideChar &Key);
	void __fastcall RadioGroup1Click(TObject *Sender);
	void __fastcall RadioGroup2Click(TObject *Sender);
	void __fastcall GLCamera1CustomPerspective(const TRectangle &viewport, int width,
          int height, int DPI, float &viewPortRadius);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);

private:	// User declarations
	int mdx, mdy;
	double a;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
