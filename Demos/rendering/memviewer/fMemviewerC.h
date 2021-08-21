//---------------------------------------------------------------------------

#ifndef fMemviewerCH
#define fMemviewerCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <signal.h>

#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.Context.hpp"
#include "GLS.OpenGLAdapter.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TRadioButton *RB1to1;
	TRadioButton *RB1to2;
	TRadioButton *RB1to10;
	TCheckBox *CheckBox1;
	TTimer *Timer1;
	TGLScene *GLScene1;
	TGLDummyCube *DummyCube1;
	TGLCube *Cube1;
	TGLLightSource *GLLightSource1;
	TGLCamera *GLCamera1;
	TGLMemoryViewer *GLMemoryViewer1;
	TGLCadencer *GLCadencer1;
	TLabel *LabelFPS;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall RB1to1Click(TObject *Sender);
	void __fastcall CheckBox1Click(TObject *Sender);
	void __fastcall GLSceneViewer1AfterRender(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);

private:	// User declarations
    int textureFramerateRatio, n;

public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
