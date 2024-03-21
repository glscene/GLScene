//---------------------------------------------------------------------------

#ifndef fManualCH
#define fManualCH
//---------------------------------------------------------------------------
#include <vcl.h>
#include <tchar.h>
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include <Vcl.ComCtrls.hpp>
//---------------------------------------------------------------------------
class TFormManual : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TTrackBar *TrackBar;
	TCheckBox *CBPlay;
	TStaticText *StaticText1;
	TGLScene *GLScene1;
	TGLCube *Cube1;
	TGLCube *Cube2;
	TGLCube *Cube3;
	TGLLightSource *GLLightSource1;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	void __fastcall TrackBarChange(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall FormResize(TObject *Sender);

private:	// User declarations
public:		// User declarations
	__fastcall TFormManual(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormManual *FormManual;
//---------------------------------------------------------------------------
#endif
