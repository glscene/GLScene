//---------------------------------------------------------------------------

#ifndef fReflectCH
#define fReflectCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.CgShader.hpp"
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"
#include "GLS.GeomObjects.hpp"
#include "GLS.Material.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.VectorFileObjects.hpp"
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TFormRef : public TForm
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
	TCheckBox *CheckBox1;
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
	TTabSheet *TabSheet3;
	TTrackBar *TrackBar1;
	TPanel *Panel9;
	TPanel *Panel10;
	TGLSceneViewer *GLSceneViewer1;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLDummyCube *GLDummyCube1;
	TGLPlane *bottom;
	TGLPlane *back;
	TGLPlane *left;
	TGLPlane *right;
	TGLPlane *front;
	TGLPlane *top;
	TGLDummyCube *GLDummyCube2;
	TGLSphere *GLSphere1;
	TGLTorus *GLTorus1;
	TGLFreeForm *plane;
	TGLCamera *GLCamera2;
	TGLCamera *GLCamera1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TGLCadencer *GLCadencer1;
	TCgShader *CgShader1;
	TTimer *Timer1;
	TGLMemoryViewer *GLMemoryViewer1;
private:	// User declarations
public:		// User declarations
	int mx, my;
	__fastcall TFormRef(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormRef *FormRef;
//---------------------------------------------------------------------------
#endif
