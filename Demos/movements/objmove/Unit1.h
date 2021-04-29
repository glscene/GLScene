//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <vcl.h>
#include <tchar.h>
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLS.Scene.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Graph.hpp"
#include "GLS.Collision.hpp"
#include "GLS.Texture.hpp"
#include "GLS.VectorGeometry.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.SpaceText.hpp"
#include "GLS.GeomObjects.hpp"
#include "GLS.Color.hpp"

#include "GLS.Coordinates.hpp"
#include "GLS.BaseClasses.hpp"
#include "GLS.BitmapFont.hpp"
#include "GLS.WindowsFont.hpp"
#include "GLS.HUDObjects.hpp"
#include <Vcl.ComCtrls.hpp>
#include "GLS.Navigator.hpp"
#include "GLS.SmoothNavigator.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *Scn;
	TPanel *Panel1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TButton *Button1;
	TGroupBox *GroupBox1;
	TCheckBox *ShowAxes;
	TGLScene *GLScene1;
	TGLCube *Floor;
	TGLLightSource *TopLight1;
	TGLCube *Cube1;
	TGLCube *Cube2;
	TGLDummyCube *DummyCube1;
	TGLArrowLine *XArrow;
	TGLArrowLine *YArrow;
	TGLArrowLine *ZArrow;
	TGLSpaceText *TxtX;
	TGLSpaceText *TxtY;
	TGLSpaceText *TxtZ;
	TGLCamera *GLCamera1;
	TGLHUDText *TopText;
	TGLHUDText *ObjText;
	TGLWindowsBitmapFont *GLWindowsBitmapFont1;
	TStatusBar *StatusBar;
	TGLSmoothNavigator *GLSmoothNavigator1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ScnMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall ScnMouseMove(TObject *Sender, TShiftState Shift, int X, int Y);
	void __fastcall ShowAxesClick(TObject *Sender);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);
	void __fastcall FormKeyPress(TObject *Sender, System::WideChar &Key);
	void __fastcall FormKeyUp(TObject *Sender, WORD &Key, TShiftState Shift);

private:	// User declarations
	TGLVector lastMouseWorldPos;
	bool movingOnZ;
	TGLCustomSceneObject *CurrentPick;
	int ScnMouseMoveCnt;
	TGLVector __fastcall MouseWorldPos(int X, int Y);
	void __fastcall UpdateHudText();
	void __fastcall ProcessPick(TGLBaseSceneObject* pick);
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
