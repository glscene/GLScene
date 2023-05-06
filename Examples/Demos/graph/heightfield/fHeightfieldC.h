//---------------------------------------------------------------------------

#ifndef fHeightfieldCH
#define fHeightfieldCH
//---------------------------------------------------------------------------
#include <tchar.h>
#include <System.Classes.hpp>
#include <System.Math.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Graph.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.Texture.hpp"
#include "GLS.SceneViewer.hpp"
//---------------------------------------------------------------------------
class TFormHeightField : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer;
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *LabelFPS;
	TTrackBar *TrackBar1;
	TTrackBar *TrackBar2;
	TTrackBar *TrackBar3;
	TRadioGroup *RadioGroup1;
	TCheckBox *CheckBox1;
	TComboBox *ComboBox1;
	TCheckBox *CheckBox2;
	TGLScene *GLScene1;
	TGLHeightField *HeightField1;
	TGLSphere *Sphere1;
	TGLLines *Lines1;
	TGLLightSource *GLLightSource1;
	TGLCamera *GLCamera1;
	TTimer *Timer1;
	TGLCadencer *GLCadencer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall Sphere1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall ComboBox1Change(TObject *Sender);
	void __fastcall CheckBox1Click(TObject *Sender);
	void __fastcall TrackBar1Change(TObject *Sender);
	void __fastcall GLSceneViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewerMouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall TrackBar2Change(TObject *Sender);
	void __fastcall TrackBar3Change(TObject *Sender);
	void __fastcall RadioGroup1Click(TObject *Sender);
	void __fastcall CheckBox2Click(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);

private:	// User declarations
	int mx,my;
	void __fastcall Formula1(const float x, const float y, float &z,
		  TVector4f &color, TTexPoint &texPoint);
	void __fastcall Formula2(const float x, const float y, float &z,
		  TVector4f &color, TTexPoint &texPoint);
	void __fastcall Formula3(const float x, const float y, float &z,
		  TVector4f &color, TTexPoint &texPoint);
public:		// User declarations
	__fastcall TFormHeightField(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormHeightField *FormHeightField;
//---------------------------------------------------------------------------
#endif
