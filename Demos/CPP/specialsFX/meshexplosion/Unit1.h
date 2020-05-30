//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <tchar.h>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLScene.hpp"
#include "GLCadencer.hpp"
#include "GLWin32Viewer.hpp"
#include "GLCoordinates.hpp"
#include "GLBaseClasses.hpp"
#include "GLCrossPlatform.hpp"
#include "GLExplosionFx.hpp"
#include "GLVectorFileObjects.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *Viewer;
	TPanel *Panel1;
	TLabel *Label2;
	TLabel *Label1;
	TLabel *Label3;
	TCheckBox *CheckOn;
	TButton *Button1;
	TProgressBar *StepBar;
	TTrackBar *MaxStepsBar;
	TTrackBar *SpeedBar;
	TGLScene *GLScene1;
	TGLFreeForm *mesh;
	TGLCamera *Camera1;
	TGLLightSource *GLLightSource1;
	TGLCadencer *GLCadencer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall CheckOnClick(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall ViewerMouseMove(TObject *Sender, TShiftState Shift, int X, int Y);
	void __fastcall ViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall SpeedBarChange(TObject *Sender);
	void __fastcall MaxStepsBarChange(TObject *Sender);


private:	// User declarations
	int vx, vy;
	TGLMeshObjectList *Cache;
	TGLBExplosionFX *expl;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
