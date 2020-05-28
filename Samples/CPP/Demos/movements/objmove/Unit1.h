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

#include "GLScene.hpp"
#include "GLObjects.hpp"
#include "GLGraph.hpp"
#include "GLCollision.hpp"
#include "GLTexture.hpp"
#include "GLVectorGeometry.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLSpaceText.hpp"
#include "GLGeomObjects.hpp"
#include "GLColor.hpp"
#include "GLCrossPlatform.hpp"
#include "GLCoordinates.hpp"
#include "GLBaseClasses.hpp"
#include "GLBitmapFont.hpp"
#include "GLWindowsFont.hpp"
#include "GLHUDObjects.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *Scn;
	TPanel *Panel1;
	TLabel *Label2;
	TLabel *Label1;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *Label5;
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
	Glvectorgeometry::TVector lastMouseWorldPos;
	bool movingOnZ;
	TGLCustomSceneObject *CurrentPick;
	int ScnMouseMoveCnt;
	Glvectorgeometry::TVector __fastcall MouseWorldPos(int X, int Y);
	void __fastcall UpdateHudText();
	void __fastcall ProcessPick(TGLBaseSceneObject* pick);
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
