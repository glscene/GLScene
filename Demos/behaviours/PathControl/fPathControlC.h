//---------------------------------------------------------------------------

#ifndef fPathControlCH
#define fPathControlCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>

#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SimpleNavigation.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.Movement.hpp"
//---------------------------------------------------------------------------
class TFormPathControl : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TBitBtn *MoveBtn;
	TGLScene *GLScene1;
	TGLDummyCube *DummyCube1;
	TGLCube *Cube2;
	TGLLightSource *GLLightSource1;
	TGLSphere *Sphere1;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	TGLSimpleNavigation *GLSimpleNavigation1;
	void __fastcall FormActivate(TObject *Sender);
	void __fastcall MoveBtnClick(TObject *Sender);
private:	// User declarations
	void PathTravelStop(TObject *Sender, TGLMovementPath *Path, bool Looped);
	void PathAllTravelledOver(TObject *Sender);

public:		// User declarations
	__fastcall TFormPathControl(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormPathControl *FormPathControl;
//---------------------------------------------------------------------------
#endif
