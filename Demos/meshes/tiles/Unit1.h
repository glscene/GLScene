//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.Graph.hpp"
#include "GLS.Material.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.TilePlane.hpp"
#include "GLS.SceneViewer.hpp"
#include <Vcl.Imaging.jpeg.hpp>
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TComboBox *CBMaterial;
	TButton *BUPack;
	TCheckBox *CBShowGrid;
	TCheckBox *CBSortByMaterials;
	TGLScene *GLScene;
	TGLLightSource *GLLightSource;
	TGLDummyCube *DCTarget;
	TGLCamera *GLCamera;
	TGLTilePlane *GLTilePlane;
	TGLDirectOpenGL *GLDirectOpenGL;
	TGLXYZGrid *GLXYZGrid;
	TGLDummyCube *DCSelection;
	TGLLines *GLLines1;
	TGLMaterialLibrary *GLMaterialLibrary;
	TTimer *Timer1;
	TGLCadencer *GLCadencer1;
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
	int mx, my;
	int tileX, tileY;
	TGLVector* mip;
	TGLVector* translateOffset;
	bool translating;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
