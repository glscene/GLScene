//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLBaseClasses.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGeomObjects.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLFile3DS.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TLabel *Label1;
	TLabel *Label2;
	TGLSceneViewer *GLSceneViewer1;
	TGLSceneViewer *GLSceneViewer2;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLDummyCube *DummyCube1;
	TGLFreeForm *FreeForm1;
	TGLSphere *Sphere1;
	TGLArrowLine *ArrowLine1;
	TGLCamera *GLCamera1;
	TGLCamera *GLCamera2;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall GLSceneViewer2MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer2MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
