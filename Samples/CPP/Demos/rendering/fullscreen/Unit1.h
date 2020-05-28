//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include "System.SysUtils.hpp"
#include "GLBaseClasses.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLFullScreenViewer.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLTeapot.hpp"
#include "GLCanvas.hpp"

//---------------------------------------------------------------------------
class TDataModule1 : public TDataModule
{
__published:	// IDE-managed Components
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLTeapot *Teapot1;
	TGLDummyCube *DCBlueLight;
	TGLLightSource *GLLightSource2;
	TGLCamera *GLCamera1;
	TGLFullScreenViewer *GLFullScreenViewer1;
	void __fastcall DataModuleCreate(TObject *Sender);
	void __fastcall GLFullScreenViewer1PostRender(TObject *Sender);
	void __fastcall GLFullScreenViewer1KeyPress(TObject *Sender, System::WideChar &Key);

private:	// User declarations
public:		// User declarations
	__fastcall TDataModule1(TComponent* Owner);
	bool firstPassDone;
};
//---------------------------------------------------------------------------
extern PACKAGE TDataModule1 *DataModule1;
//---------------------------------------------------------------------------
#endif
