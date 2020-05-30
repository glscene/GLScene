//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMaterial.hpp"
#include "GLMaterialScript.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include "GLUtils.hpp"
#include "Jpeg.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TMemo *Memo1;
	TButton *Button1;
	TButton *Button2;
	TMemo *Memo2;
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLDummyCube *GLDummyCube1;
	TGLCube *GLCube1;
	TGLCamera *GLCamera1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TOpenDialog *OpenDialog1;
	TGLMaterialScripter *GLMaterialScripter1;
	TGLCadencer *GLCadencer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
