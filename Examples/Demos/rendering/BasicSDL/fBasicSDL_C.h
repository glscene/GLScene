//---------------------------------------------------------------------------

#ifndef fBasicSDL_CH
#define fBasicSDL_CH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"
#include "GLS.GeomObjects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SDL.Context.hpp"

//---------------------------------------------------------------------------
class TDataModule2 : public TDataModule
{
__published:	// IDE-managed Components
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLTeapot *Teapot1;
	TGLCamera *GLCamera1;
	TSDLViewer *GLSDLViewer1;
	void __fastcall DataModuleCreate(TObject *Sender);
	void __fastcall GLSDLViewer1EventPollDone(TObject *Sender);
	void __fastcall GLSDLViewer1Resize(TObject *Sender);
private:	// User declarations
public:		// User declarations
	bool firstPassDone;
	__fastcall TDataModule2(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TDataModule2 *DataModule2;
//---------------------------------------------------------------------------
#endif
