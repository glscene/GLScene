//---------------------------------------------------------------------------

#ifndef fTexCombineCH
#define fTexCombineCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"
#include "GLS.HUDObjects.hpp"
#include "GLS.Material.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLSL.TextureShaders.hpp"
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TFormCombine : public TForm
{
__published:	// IDE-managed Components
	TImage *Image1;
	TImage *Image2;
	TImage *Image3;
	TLabel *Label1;
	TImage *Image4;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *Label2;
	TGLSceneViewer *SceneViewer;
	TButton *BUApply;
	TPanel *PATex1;
	TPanel *PATex2;
	TPanel *PATex3;
	TCheckBox *CBTex0;
	TCheckBox *CBTex1;
	TCheckBox *CBTex2;
	TCheckBox *CBTex3;
	TPanel *Panel1;
	TMemo *MECombiner;
	TGLScene *GLScene;
	TGLDummyCube *GLDummyCube;
	TGLHUDSprite *GLHUDSprite;
	TGLCamera *GLCamera;
	TGLMaterialLibrary *GLMaterialLibrary;
	TGLTexCombineShader *GLTexCombineShader;
	TColorDialog *ColorDialog;
	TShape *Shape1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall BUApplyClick(TObject *Sender);
	void __fastcall SceneViewerPostRender(TObject *Sender);
	void __fastcall Shape1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y);
	void __fastcall CBTex0Click(TObject *Sender);

private:	// User declarations
    TFileName PathToData;
public:		// User declarations
	__fastcall TFormCombine(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormCombine *FormCombine;
//---------------------------------------------------------------------------
#endif
