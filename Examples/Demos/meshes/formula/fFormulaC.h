//---------------------------------------------------------------------------

#ifndef fFormulaCH
#define fFormulaCH
//---------------------------------------------------------------------------
#include <vcl.h>
#include <tchar.h>
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLS.Scene.hpp"
#include "GLS.Objects.hpp"
#include "GLS.VectorGeometry.hpp"
#include "GLS.Texture.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Mesh.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.State.hpp"
#include "GLS.Color.hpp"

#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"


//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLSceneViewer *GLSceneViewer2;
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TGLScene *GLScene1;
	TGLDummyCube *DummyCube1;
	TGLMesh *Mesh1;
	TGLLightSource *GLLightSource1;
	TGLCamera *GLCamera1;
	TTimer *Timer1;
	TGLScene *GLScene2;
	TGLDummyCube *DummyCube2;
	TGLMesh *Mesh2;
	TGLLightSource *GLLightSource2;
	TGLCamera *GLCamera2;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall GLSceneViewer2MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer2MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
private:	// User declarations
	int mx, my;
	float invRes1, invRes2;
	TAffineVector __fastcall MakeVect(const float aX, const float aY);
	void __fastcall AddTriangle(const TAffineVector p1,
						  const TAffineVector p2,
						  const TAffineVector p3,
						  const TGLColorVector color);
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
