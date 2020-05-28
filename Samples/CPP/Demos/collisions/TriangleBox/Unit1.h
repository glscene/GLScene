//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGeomObjects.hpp"
#include "GLGraph.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *Viewer;
	TPanel *Panel2;
	TCheckBox *CheckBox6;
	TButton *Button1;
	TCheckBox *CheckBox1;
	TCheckBox *CheckBox2;
	TCheckBox *CheckBox3;
	TButton *Button2;
	TCheckBox *CheckBox4;
	TCheckBox *CheckBox5;
	TGLScene *GLScene;
	TGLDummyCube *DCCamTarget;
	TGLLightSource *GLLightSource1;
	TGLLightSource *GLLightSource2;
	TGLCube *GLCube1;
	TGLXYZGrid *GLXYZGrid1;
	TGLPolygon *GLPolygon1;
	TGLLines *GLLines1;
	TGLPoints *GLPoints1;
	TGLLines *GLLines2;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall CheckBox4Click(TObject *Sender);
	void __fastcall GLCadencerProgress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall ViewerMouseMove(TObject *Sender, TShiftState Shift, int X, int Y);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);
	void __fastcall FormResize(TObject *Sender);
	void __fastcall FormKeyPress(TObject *Sender, System::WideChar &Key);


private:	// User declarations
	int mdx, mdy;
	TAffineVector BoxPos, BoxScale,	MinExtend, MaxExtend;
	TAffineVector TriangePos[3];
	void __fastcall MakeRandomData();
    void __fastcall DrawResult();
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
