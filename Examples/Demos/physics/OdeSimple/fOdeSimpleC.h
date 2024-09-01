//---------------------------------------------------------------------------

#ifndef fOdeSimpleCH
#define fOdeSimpleCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLS.Scene.hpp"
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Graph.hpp"
#include "GLS.Objects.hpp"
#include "GLS.GeomObjects.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.ODEManager.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TButton *Spawn;
	TComboBox *ComboBox1;
	TCheckBox *CheckBox1;
	TCheckBox *CheckBox2;
	TTrackBar *TrackBar1;
	TComboBox *ComboBox2;
	TGLScene *GLScene1;
	TGLDummyCube *GLDummyCube1;
	TGLCamera *GLCamera1;
	TGLLightSource *GLLightSource1;
	TGLHeightField *GLHeightField1;
	TGLPlane *GLPlane1;
	TGLDummyCube *ODEObjects;
	TGLRenderPoint *GLRenderPoint1;
	TGLCadencer *GLCadencer1;
	TGLODEManager *GLODEManager1;
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall SpawnClick(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall GLHeightField1GetHeight(const float x, const float y, float &z,
          TVector4f &color, TTexPoint &texPoint);
	void __fastcall CheckBox1Click(TObject *Sender);
	void __fastcall CheckBox2Click(TObject *Sender);
	void __fastcall ComboBox2Change(TObject *Sender);
	void __fastcall TrackBar1Change(TObject *Sender);
	void __fastcall FormActivate(TObject *Sender);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);

private:	// User declarations
	int mx,my;
	void DoSphere(void);
	void DoBox(void);
	void DoCapsule(void);
	void DoCylinder(void);
	void DoCone(void);
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
