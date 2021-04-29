//---------------------------------------------------------------------------

#ifndef fTorqueCH
#define fTorqueCH
//---------------------------------------------------------------------------
#include <tchar.h>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Objects.hpp"
#include "GLS.GeomObjects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.Behaviours.hpp"
#include "GLS.VectorFileObjects.hpp"
#include "GLS.BitmapFont.hpp"
#include "GLS.HUDObjects.hpp"
//---------------------------------------------------------------------------
class TFormTorqueC : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLCamera *GLCamera1;
	TGLScene *GLScene1;
	TGLCadencer *GLCadencer1;
	TGLLightSource *GLLightSource1;
	TGLDummyCube *DummyCube1;
	TPanel *Panel1;
	TPanel *PanelBottom;
	TCheckBox *CheckBox1;
	TGLBitmapFont *GLBitmapFont1;
	TLabel *lHexahedron;
	TLabel *lDodecahedron;
	TLabel *lOctagedron;
	TLabel *lTetrahedron;
	TLabel *lIcosahedron;
	TGLTetrahedron *Tetrahedron;
	TGLOctahedron *Octahedron;
	TGLHexahedron *Hexahedron;
	TGLDodecahedron *Dodecahedron;
	TGLIcosahedron *Icosahedron;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
		  int Y);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall CheckBox1Click(TObject *Sender);
private:	// User declarations
	Double lastTime;
	TGLBaseSceneObject *pickedObject;
public:		// User declarations
	__fastcall TFormTorqueC(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormTorqueC *FormTorqueC;
//---------------------------------------------------------------------------
#endif
