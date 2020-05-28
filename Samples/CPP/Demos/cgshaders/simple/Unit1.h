//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "GLBaseClasses.hpp"
#include "GLCgShader.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMaterial.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include "GLCadencer.hpp"
#include "GLCoordinates.hpp"
#include "GLGraph.hpp"
#include "GLObjects.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLCgShader.hpp"
#include "GLFile3DS.hpp"
#include "Cg.hpp"
#include "CgGL.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TSplitter *Splitter1;
	TPanel *Panel1;
	TPageControl *PageControl1;
	TTabSheet *TabSheet1;
	TSplitter *Splitter3;
	TPanel *Panel2;
	TLabel *LabelVertProfile;
	TCheckBox *CBVertexProgram;
	TPanel *Panel11;
	TPanel *Panel12;
	TMemo *MemoVertCode;
	TPanel *Panel13;
	TButton *ButtonApplyVP;
	TPanel *Panel5;
	TLabel *Label2;
	TMemo *Memo1;
	TButton *Button1;
	TButton *Button4;
	TTabSheet *TabSheet2;
	TSplitter *Splitter2;
	TPanel *Panel4;
	TLabel *LabelFragProfile;
	TCheckBox *CBFragmentProgram;
	TPanel *Panel6;
	TPanel *Panel7;
	TMemo *MemoFragCode;
	TPanel *Panel3;
	TButton *ButtonApplyFP;
	TPanel *Panel8;
	TLabel *Label1;
	TMemo *Memo3;
	TButton *Button2;
	TButton *Button3;
	TPanel *Panel9;
	TPanel *PanelFPS;
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLFreeForm *GLFreeForm1;
	TGLXYZGrid *GLXYZGrid1;
	TGLDummyCube *GLDummyCube1;
	TGLCamera *GLCamera1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TGLCadencer *GLCadencer1;
	TCgShader *CgShader1;
	TTimer *Timer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall CgShader1ApplyVP(TCgProgram *CgProgram, TObject *Sender);
	void __fastcall CgShader1Initialize(TCustomCgShader *CgShader);
	void __fastcall CBVertexProgramClick(TObject *Sender);
	void __fastcall CBFragmentProgramClick(TObject *Sender);
	void __fastcall ButtonApplyFPClick(TObject *Sender);
	void __fastcall ButtonApplyVPClick(TObject *Sender);
	void __fastcall MemoFragCodeChange(TObject *Sender);
	void __fastcall MemoVertCodeChange(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall Button3Click(TObject *Sender);
	void __fastcall Button4Click(TObject *Sender);
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
		  int Y);
	void __fastcall GLCadencer1Progress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall FormKeyPress(TObject *Sender, System::WideChar &Key);

private:	// User declarations
    int mx,my;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
