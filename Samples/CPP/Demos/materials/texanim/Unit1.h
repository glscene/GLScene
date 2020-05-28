//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLScene *GLScene1;
	TGLSceneViewer *GLSceneViewer1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TGLCamera *GLCamera1;
	TGLCube *Cube1;
	TGLLightSource *GLLightSource1;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	TPanel *Panel1;
	TButton *Button1;
	TCheckBox *CBAnimate;
	TLabel *LabelFPS;
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);

private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
	double timeToNextFrame;
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
