//---------------------------------------------------------------------------

#ifndef fBoomCH
#define fBoomCH
//---------------------------------------------------------------------------
#include <tchar.h>

#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLS.BaseClasses.hpp"
#include "GLS.Behaviours.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.FireFX.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TButton *Button1;
	TGLScene *GLScene1;
	TGLDummyCube *DummyCube1;
	TGLLightSource *GLLightSource1;
	TGLSphere *Sphere1;
	TGLCamera *GLCamera1;
	TGLFireFXManager *FireFX;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	TGLFireFXManager *SmokeFX;
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall FormResize(TObject *Sender);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);


private:	// User declarations
	int mx, my;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
