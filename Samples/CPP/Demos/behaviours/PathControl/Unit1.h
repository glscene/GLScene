//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>

#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLSimpleNavigation.hpp"
#include "GLWin32Viewer.hpp"
#include "GLMovement.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
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
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
