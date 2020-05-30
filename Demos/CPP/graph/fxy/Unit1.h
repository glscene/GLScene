//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
#include "GLBaseClasses.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGraph.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include <System.Classes.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.StdCtrls.hpp>

//---------------------------------------------------------------------------
class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TGLScene * GLScene1;
  TGLSceneViewer *GLSceneViewer1;
  TGLCamera *GLCamera1;
  TGLLightSource *GLLightSource1;
  TGLHeightField *GLHeightField1;
  TGLXYZGrid *XYGrid;
  TGLXYZGrid *XZGrid;
  TGLXYZGrid *YZGrid;
	TPanel *Panel1;
	TTrackBar *TrackBar1;
	TTrackBar *TrackBar2;
	TTrackBar *TrackBar3;
	TLabel *Label1;
	TCheckBox *CheckBox1;
	TRadioGroup *RadioGroup1;
  void __fastcall CheckBox1Click(TObject * Sender);
  void __fastcall TrackBar1Change(TObject * Sender);
  void __fastcall GLSceneViewer1MouseDown(TObject * Sender, TMouseButton Button,
                                          TShiftState Shift, int X, int Y);
  void __fastcall GLSceneViewer1MouseMove(TObject * Sender, TShiftState Shift,
                                          int X, int Y);
	void __fastcall TrackBar2Change(TObject *Sender);
	void __fastcall TrackBar3Change(TObject *Sender);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);
	void __fastcall RadioGroup1Click(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
private:                       // User declarations
	int mx,my;
	void __fastcall Formula0(const float x, const float y, float &z,
		  TVector4f &color, TTexPoint &texPoint);
	void __fastcall Formula1(const float x, const float y, float &z,
		  TVector4f &color, TTexPoint &texPoint);
	void __fastcall Formula2(const float x, const float y, float &z,
		  TVector4f &color, TTexPoint &texPoint);
	void __fastcall Formula3(const float x, const float y, float &z,
		  TVector4f &color, TTexPoint &texPoint);

public:                        // User declarations
    __fastcall TForm1(TComponent * Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif

