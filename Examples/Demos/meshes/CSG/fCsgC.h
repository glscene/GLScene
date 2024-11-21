//---------------------------------------------------------------------------

#ifndef fCsgCH
#define fCsgCH
//---------------------------------------------------------------------------
#include <vcl.h>
#include <tchar.h>
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Material.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.MeshBSP.hpp"
#include "GLS.MeshCSG.hpp"

#include "Stage.Utils.hpp"



//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TButton *ButtonClear;
	TButton *btnUnionAandB;
	TButton *btnSubtractAB;
	TButton *btnSubtractBA;
	TButton *btnIntersectAorB;
	TCheckBox *CheckBox1;
	TGLScene *GLScene1;
	TGLCamera *GLCamera1;
	TGLLightSource *GLLightSource1;
	TGLDummyCube *GLDummyCube1;
	TGLFreeForm *GLFreeForm3;
	TGLFreeForm *GLFreeForm2;
	TGLFreeForm *GLFreeForm1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer1MouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall FormMouseWheelDown(TObject *Sender, TShiftState Shift, TPoint &MousePos,
          bool &Handled);
	void __fastcall FormMouseWheelUp(TObject *Sender, TShiftState Shift, TPoint &MousePos,
          bool &Handled);
	void __fastcall ButtonClearClick(TObject *Sender);
	void __fastcall btnUnionAandBClick(TObject *Sender);
	void __fastcall btnSubtractABClick(TObject *Sender);
	void __fastcall btnSubtractBAClick(TObject *Sender);
	void __fastcall btnIntersectAorBClick(TObject *Sender);
	void __fastcall CheckBox1Click(TObject *Sender);
private:	// User declarations
	int mx, my;
	bool Drag;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
