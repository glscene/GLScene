//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLBaseClasses.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGraph.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer;
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TRadioButton *RBPaint;
	TRadioButton *RadioButton2;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLCamera *GLCamera1;
	TGLHeightField *HeightField;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall HeightFieldGetHeight(const float x, const float y, float &z, TVector4f &Color,
          TTexPoint &TexPoint);
	void __fastcall GLSceneViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewerMouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
private:	// User declarations
	int mx, my;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
