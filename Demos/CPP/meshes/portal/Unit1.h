//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Grids.hpp>

#include "GLScene.hpp"
#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLPortal.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLKeyboard.hpp"
#include "JPeg.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TGLSceneViewer *GLSceneViewer1;
	TButton *BUForward;
	TButton *BUTurnLeft;
	TButton *BUTurnRight;
	TButton *BUBackward;
	TStringGrid *SGMap;
	TButton *BBProcess;
	TCheckBox *CBAuto;
	TCheckBox *CBFog;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLDummyCube *DummyCube1;
	TGLCamera *GLCamera1;
	TGLPortal *Portal1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TTimer *Timer1;
	TGLCadencer *GLCadencer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall BBProcessClick(TObject *Sender);
private:	// User declarations
	int portalCount, triangleCount;
public:		// User declarations
__published:
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
