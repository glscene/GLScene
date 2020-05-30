//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Windows.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ComCtrls.hpp>

#include "GLBaseClasses.hpp"
#include "GLBitmapFont.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGeomObjects.hpp"
#include "GLGraph.hpp"
#include "GLObjects.hpp"
#include "GLPolyhedron.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include "GLWindowsFont.hpp"
#include "GLKeyboard.hpp"
#include "GLGizmoEx.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TLabel *Label9;
	TGLSceneViewer *Viewer;
	TPanel *Panel2;
	TSpeedButton *SpeedButton1;
	TSpeedButton *SpeedButton2;
	TSpeedButton *SpeedButton3;
	TSpeedButton *SpeedButton4;
	TSpeedButton *SpeedButton5;
	TLabel *Label12;
	TSpeedButton *SpeedButton6;
	TSpeedButton *SpeedButton11;
	TLabel *Label15;
	TSpeedButton *SpeedButton12;
	TSpeedButton *SpeedButton13;
	TLabel *Label17;
	TComboBox *ComboBox4;
	TPanel *Panel4;
	TSpeedButton *SpeedButton7;
	TSpeedButton *SpeedButton8;
	TSpeedButton *SpeedButton9;
	TSpeedButton *SpeedButton10;
	TPageControl *PageControl1;
	TTabSheet *TabSheet1;
	TLabel *Label10;
	TLabel *Label11;
	TLabel *Label4;
	TLabel *Label5;
	TLabel *Label16;
	TLabel *Label6;
	TLabel *Label7;
	TLabel *Label13;
	TCheckBox *CheckBox1;
	TCheckBox *CheckBox10;
	TCheckBox *CheckBox11;
	TCheckBox *CheckBox12;
	TCheckBox *CheckBox2;
	TCheckBox *CheckBox4;
	TCheckBox *CheckBox5;
	TCheckBox *CheckBox6;
	TCheckBox *CheckBox7;
	TCheckBox *CheckBox8;
	TCheckBox *CheckBox9;
	TEdit *edAutoZoomFactor;
	TEdit *edtGizmoThickness;
	TEdit *edtScaleCoef;
	TEdit *edzoomfactor;
	TRadioGroup *OptPickMode;
	TComboBox *ComboBox3;
	TCheckBox *CheckBox13;
	TCheckBox *CheckBox14;
	TCheckBox *CheckBox15;
	TCheckBox *CheckBox3;
	TEdit *edMoveCoef;
	TEdit *edRotateCoef;
	TCheckBox *CheckBox16;
	TEdit *Edit1;
	TTabSheet *TabSheet2;
	TLabel *Label1;
	TLabel *Label14;
	TLabel *Label2;
	TLabel *Label3;
	TColorBox *ColorBox1;
	TColorBox *ColorBox2;
	TColorBox *ColorBox3;
	TColorBox *ColorBox4;
	TTabSheet *TabSheet3;
	TLabel *Label8;
	TPanel *Panel1;
	TSpeedButton *SpeedButton14;
	TSpeedButton *SpeedButton15;
	TSpeedButton *SpeedButton17;
	TSpeedButton *SpeedButton18;
	TPanel *Panel3;
	TTreeView *TreeView1;
	TPanel *Panel5;
	TGroupBox *GroupBox1;
	TSpeedButton *SpeedButton16;
	TSpeedButton *SpeedButton19;
	TSpeedButton *SpeedButton20;
	TGLScene *GLScene1;
	TGLDummyCube *GLRootUserInterface;
	TGLDummyCube *GLTargetCamera;
	TGLCamera *Camera;
	TGLLightSource *GLLightSource1;
	TGLLightSource *GLLightSource2;
	TGLXYZGrid *GLXYZGrid1;
	TGLDummyCube *GLRootObjects;
	TGLDodecahedron *GLDodecahedron3;
	TGLArrowLine *GLArrowLine3;
	TGLArrowLine *GLArrowLine4;
	TGLCube *GLCube2;
	TGLCube *GLCube1;
	TGLSphere *GLSphere1;
	TGLFrustrum *GLFrustrum1;
	TGLDisk *GLDisk1;
	TGLDummyCube *RootGizmo;
	TGLDummyCube *RootTempObjects;
	TGLCadencer *GLCadencer1;
	TGLWindowsBitmapFont *WindowsBitmapFont;
	TTimer *Timer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
	void __fastcall FormKeyUp(TObject *Sender, WORD &Key, TShiftState Shift);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall Timer1Timer(TObject *Sender);

private:	// User declarations
	int mx, my;
	Glvectorgeometry::TVector MousePos,LostMousePos;
	bool MouseMoving;
	Glvectorgeometry::TVector pos;
	TGLBaseSceneObject *FObj;
	TGLGizmoEx *Gizmo;
	float FVectorLength;
	int FCreationScenarious;
	void __fastcall UpdateTreeView();

public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
