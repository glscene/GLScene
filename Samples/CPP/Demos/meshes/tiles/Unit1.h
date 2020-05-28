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
#include "GLGraph.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLTilePlane.hpp"
#include "GLWin32Viewer.hpp"
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
	TVector* mip;
	TVector* translateOffset;
	bool translating;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
