//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <vcl.h>
#include <tchar.h>
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLBaseClasses.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLTerrainRenderer.hpp"
#include "GLHeightData.hpp"
#include "GLWin32Viewer.hpp"
#include "Jpeg.hpp"
#include "GLKeyBoard.hpp"



//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLDummyCube *DummyCube1;
	TGLCamera *GLCamera1;
	TGLTerrainRenderer *TerrainRenderer1;
	TTimer *Timer1;
	TGLCadencer *GLCadencer1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TGLCustomHDS *GLCustomHDS;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall GLCustomHDSStartPreparingData(TGLHeightData *HeightData);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall FormKeyPress(TObject *Sender, System::WideChar &Key);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);

private:	// User declarations
	int mx, my;
	bool fullScreen;
	float FCamHeight;

public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
