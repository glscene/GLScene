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
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include <System.Actions.hpp>
#include <System.ImageList.hpp>
#include <Vcl.ActnList.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.ExtDlgs.hpp>
#include <Vcl.ImgList.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.ToolWin.hpp>
//---------------------------------------------------------------------------
class TFormTexCombine : public TForm
{
__published:	// IDE-managed Components
	TSplitter *Splitter1;
	TPanel *PAImages;
	TPageControl *PageControl;
	TTabSheet *TSRGB;
	TScrollBox *ScrollBox1;
	TImage *IMRGB;
	TTabSheet *TSAlpha;
	TScrollBox *ScrollBox2;
	TImage *IMAlpha;
	TToolBar *ToolBar;
	TToolButton *tbImport;
	TToolButton *tbExport;
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TComboBox *CBWidth;
	TComboBox *CBHeight;
	TPanel *PAPreview;
	TGLSceneViewer *GLSceneViewer;
	TPanel *Panel2;
	TCheckBox *CBTextureFiltering;
	TComboBox *CBBackground;
	TMainMenu *MainMenu;
	TMenuItem *File1;
	TMenuItem *Exit2;
	TMenuItem *SaveTexture1;
	TMenuItem *N1;
	TMenuItem *Exit1;
	TMenuItem *ools1;
	TMenuItem *Colormapdilatation1;
	TMenuItem *N2;
	TMenuItem *Alphamaperosion1;
	TMenuItem *AlphamapDilatation1;
	TMenuItem *Alpha1;
	TMenuItem *GenerateAlpha1;
	TMenuItem *Opaque1;
	TMenuItem *SuperBlackTransparent1;
	TMenuItem *FromRGBIntensity1;
	TMenuItem *FromRGBSqrtIntensity1;
	TMenuItem *N3;
	TMenuItem *Negate1;
	TMenuItem *Offset1;
	TMenuItem *Saturate1;
	TImageList *ImageList;
	TActionList *ActionList;
	TAction *ACExit;
	TAction *ACImport;
	TAction *ACOpenTexture;
	TAction *ACSaveTexture;
	TAction *ACColorDilatation;
	TAction *ACAlphaErosion;
	TAction *ACExport;
	TAction *ACAlphaDilatation;
	TAction *ACOpaque;
	TAction *ACAlphaSuperBlack;
	TAction *ACFromRGBIntensity;
	TAction *ACFromRGBSqrtIntensity;
	TAction *ACAlphaOffset;
	TAction *ACAlphaSaturate;
	TAction *ACAlphaNegate;
	TGLScene *GLScene;
	TGLHUDSprite *HSBkgnd;
	TGLDummyCube *GLDummyCube;
	TGLCube *GLCube;
	TGLLightSource *GLLightSource;
	TGLCamera *GLCamera;
	TOpenPictureDialog *OpenPictureDialog;
	TSaveDialog *SaveDialog;
	void __fastcall ACExportExecute(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TFormTexCombine(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormTexCombine *FormTexCombine;
//---------------------------------------------------------------------------
#endif
