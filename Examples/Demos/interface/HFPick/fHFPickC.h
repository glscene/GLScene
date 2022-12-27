//---------------------------------------------------------------------------

#ifndef fHFPickCH
#define fHFPickCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Graph.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TFormHFPick : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer;
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TRadioButton *RBPaint;
	TRadioButton *RadioButton2;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLCamera *GLCamera1;
	TGLHeightField *HeightField;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall HeightFieldGetHeight(const float x, const float y, float &z, TVector4f &Color,
          TTexPoint &TexPoint);
	void __fastcall GLSceneViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewerMouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
private:	// User declarations
	int mx, my;
public:		// User declarations
	__fastcall TFormHFPick(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormHFPick *FormHFPick;
//---------------------------------------------------------------------------
#endif
