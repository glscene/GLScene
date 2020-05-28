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
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TTrackBar *TrackBar1;
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TGLScene *GLScene1;
	TGLDummyCube *DummyCube3;
	TGLFreeForm *FreeForm3;
	TGLDummyCube *DummyCube2;
	TGLFreeForm *FreeForm2;
	TGLDummyCube *DummyCube1;
	TGLFreeForm *FreeForm1;
	TGLLightSource *GLLightSource1;
	TGLDummyCube *DCCamera;
	TGLCamera *GLCamera1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall TrackBar1Change(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
