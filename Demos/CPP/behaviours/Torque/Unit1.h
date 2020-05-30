//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <tchar.h>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLObjects.hpp"
#include "GLPolyhedron.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include "GLBehaviours.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *Label1;
	TLabel *Label5;
	TCheckBox *CheckBox1;
	TPanel *Panel2;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLDummyCube *DummyCube1;
	TGLTetrahedron *Tetrahedron;
	TGLOctahedron *Octahedron;
	TGLCube *Hexahedron;
	TGLDodecahedron *Dodecahedron;
	TGLIcosahedron *Icosahedron;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall CheckBox1Click(TObject *Sender);

private:	// User declarations
	Double lastTime;
	TGLBaseSceneObject *pickedObject;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
