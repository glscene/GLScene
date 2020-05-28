//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <tchar.h>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLAsyncTimer.hpp"
#include "GLBaseClasses.hpp"
#include "GLBehaviours.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGeomObjects.hpp"
#include "GLGraph.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLTeapot.hpp"
#include "GLWin32Viewer.hpp"
#include "GLzBuffer.hpp"
#include "JPeg.hpp"


//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TLabel *Label1;
	TLabel *Label2;
	TPanel *Panel2;
	TGLSceneViewer *Caster;
	TPanel *Panel1;
	TGLSceneViewer *Viewer;
	TPanel *Panel3;
	TLabel *Label4;
	TLabel *TimeLbl;
	TTrackBar *DistanceBar;
	TButton *CastBtn;
	TPanel *Panel4;
	TLabel *Label3;
	TLabel *Label5;
	TTrackBar *DistanceBar2;
	TTrackBar *Focal;
	TPanel *Panel5;
	TCheckBox *FrustBox;
	TCheckBox *RotateBox;
	TCheckBox *ShadowOnBox;
	TCheckBox *SoftBox;
	TCheckBox *SkyShadBox;
	TPanel *Panel6;
	TLabel *Label9;
	TCheckBox *FadeBox;
	TTrackBar *dovBar;
	TTrackBar *AlphaBar;
	TMemo *Memo1;
	TGLScene *GLScene1;
	TGLDummyCube *Objects;
	TGLHeightField *HeightField1;
	TGLCube *Cube1;
	TGLTorus *Torus1;
	TGLTeapot *Teapot1;
	TGLZShadows *Shadows1;
	TGLCamera *GLCamera1;
	TGLCamera *GLCamera2;
	TGLLightSource *GLLightSource1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TGLMemoryViewer *MemView;
	TGLAsyncTimer *AsyncTimer1;
	TGLCadencer *GLCadencer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall ViewerMouseMove(TObject *Sender, TShiftState Shift, int X, int Y);
	void __fastcall CasterMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall CasterMouseMove(TObject *Sender, TShiftState Shift, int X, int Y);
	void __fastcall DistanceBarChange(TObject *Sender);
	void __fastcall DistanceBar2Change(TObject *Sender);
	void __fastcall CastBtnClick(TObject *Sender);
	void __fastcall ViewerMouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall CasterMouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall FadeBoxClick(TObject *Sender);
	void __fastcall HeightField1GetHeight(const float x, const float y, float &z, TVector4f &Color,
          TTexPoint &TexPoint);
	void __fastcall FrustBoxClick(TObject *Sender);
	void __fastcall AsyncTimer1Timer(TObject *Sender);
	void __fastcall RotateBoxClick(TObject *Sender);
	void __fastcall ShadowOnBoxClick(TObject *Sender);
	void __fastcall SoftBoxClick(TObject *Sender);
	void __fastcall SkyShadBoxClick(TObject *Sender);
	void __fastcall FocalChange(TObject *Sender);
	void __fastcall dovBarChange(TObject *Sender);
	void __fastcall AlphaBarChange(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);



private:	// User declarations
	int mx,my;
	int mx2,my2;
	TGLzBuffer *zViewer;
	TGLzBuffer *zCaster;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
