//---------------------------------------------------------------------------

#ifndef fObjmoveCH
#define fObjmoveCH
//---------------------------------------------------------------------------
#include <vcl.h>
#include <tchar.h>
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>

#include <GLS.Scene.hpp>
#include "GLS.Objects.hpp"
#include "GLS.Graph.hpp"
#include "GLS.Collision.hpp"
#include "GLS.Texture.hpp"
#include "Stage.VectorGeometry.hpp"
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

//---------------------------------------------------------------------------
class TFormObjmove : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *Scene;
	TPanel *Panel1;
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
	TGLHUDText *HUDText;
	TGLHUDText *HUDTextObj;
	TGLWindowsBitmapFont *GLWindowsBitmapFont1;
	TStatusBar *StatusBar;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall SceneMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall SceneMouseMove(TObject *Sender, TShiftState Shift, int X, int Y);
	void __fastcall ShowAxesClick(TObject *Sender);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);
	void __fastcall FormKeyPress(TObject *Sender, System::WideChar &Key);
	void __fastcall FormKeyUp(TObject *Sender, WORD &Key, TShiftState Shift);

private:	// User declarations
	TGLVector lastMouseWorldPos;
	bool movingOnZ;
	TGLCustomSceneObject *CurrentPick;
	int SceneMouseMoveCnt;
	TGLVector __fastcall MouseWorldPos(int X, int Y);
	void __fastcall UpdateHUDText();
	void __fastcall ProcessPick(TGLBaseSceneObject* pick);
public:		// User declarations
	__fastcall TFormObjmove(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormObjmove *FormObjmove;
//---------------------------------------------------------------------------
#endif
