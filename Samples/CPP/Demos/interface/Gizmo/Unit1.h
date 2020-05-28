//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <vcl.h>
#include <tchar.h>
//---------------------------------------------------------------------------
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLBaseClasses.hpp"
#include "GLBitmapFont.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGeomObjects.hpp"
#include "GLObjects.hpp"
#include "GLPolyhedron.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include "GLWindowsFont.hpp"
#include "GLSpaceText.hpp"
#include "GLHUDObjects.hpp"
#include "GLGizmo.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *Viewer;
	TPanel *Panel1;
	TBevel *Bevel1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *Label5;
	TLabel *Label6;
	TLabel *Label10;
	TLabel *Label11;
	TLabel *Label7;
	TCheckBox *CheckBox1;
	TCheckBox *CheckBox2;
	TComboBox *CBXAxis;
	TCheckBox *CheckBox3;
	TComboBox *CBXOperation;
	TCheckBox *CheckBox4;
	TCheckBox *CheckBox5;
	TCheckBox *CheckBox6;
	TCheckBox *CheckBox7;
	TCheckBox *CheckBox8;
	TCheckBox *CheckBox9;
	TCheckBox *CheckBox10;
	TCheckBox *CheckBox11;
	TColorBox *ColorBox1;
	TColorBox *ColorBox2;
	TColorBox *ColorBox3;
	TEdit *edAutoZoomFactor;
	TEdit *edZoomFactor;
	TCheckBox *CheckBox12;
	TEdit *edMoveCoef;
	TEdit *edRotateCoef;
	TCheckBox *CheckBox13;
	TCheckBox *CheckBox14;
	TCheckBox *CheckBox15;
	TEdit *edGizmoThickness;
	TRadioGroup *OptPickMode;
	TEdit *edScaleCoef;
	TGLScene *GLScene1;
	TGLDummyCube *GLDummyCube1;
	TGLDodecahedron *GLDodecahedron3;
	TGLArrowLine *GLArrowLine3;
	TGLArrowLine *GLArrowLine4;
	TGLCube *GLCube1;
	TGLSphere *GLSphere1;
	TGLLightSource *GLLightSource1;
	TGLLightSource *GLLightSource2;
	TGLDummyCube *RootGizmo;
	TGLCamera *Camera;
	TGLCadencer *GLCadencer1;
	TGLWindowsBitmapFont *WindowsBitmapFont;
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall OptPickModeClick(TObject *Sender);
	void __fastcall ViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall ViewerMouseMove(TObject *Sender, TShiftState Shift, int X, int Y);
	void __fastcall ViewerMouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall edAutoZoomFactorChange(TObject *Sender);
	void __fastcall edAutoZoomFactorKeyPress(TObject *Sender, System::WideChar &Key);
	void __fastcall CheckBox12Click(TObject *Sender);
	void __fastcall CBXAxisChange(TObject *Sender);
	void __fastcall CBXOperationChange(TObject *Sender);
	void __fastcall edMoveCoefChange(TObject *Sender);
	void __fastcall edRotateCoefChange(TObject *Sender);
	void __fastcall edGizmoThicknessChange(TObject *Sender);
	void __fastcall edScaleCoefChange(TObject *Sender);
	void __fastcall edZoomFactorChange(TObject *Sender);
	void __fastcall ColorBox1Change(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);



private:	// User declarations
	int mx, my;
	bool noMouseMotion;
	TGLGizmo *Gizmo;
	void __fastcall FillPickableObjectsList(TGLBaseSceneObject *root, bool doClearList);
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
