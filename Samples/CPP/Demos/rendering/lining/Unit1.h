//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLBaseClasses.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLGeomObjects.hpp"
#include "GLHiddenLineShader.hpp"
#include "GLMaterial.hpp"
#include "GLObjects.hpp"
#include "GLOutlineShader.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TBevel *Bevel1;
	TCheckBox *CheckBox1;
	TGroupBox *GroupBox1;
	TBevel *Bevel2;
	TCheckBox *CheckBox2;
	TCheckBox *CheckBox3;
	TCheckBox *CheckBox4;
	TCheckBox *CheckBox5;
	TCheckBox *CheckBox6;
	TPanel *Panel2;
	TGLScene *GLScene1;
	TGLLightSource *GLLightSource1;
	TGLTorus *Torus1;
	TGLSphere *Sphere1;
	TGLAnnulus *GLAnnulusOutlined;
	TGLAnnulus *GLAnnulusPink;
	TGLAnnulus *GLAnnulusDotted;
	TGLCube *GLCubeGreen;
	TGLCube *GLCubeTransparent;
	TGLCamera *GLCamera1;
	TGLMaterialLibrary *GLMaterialLibrary1;
	TGLOutlineShader *GLOutlineShader1;
	TGLHiddenLineShader *GLHiddenLineShader1;
	TGLHiddenLineShader *GLHiddenLineShader2;
	TGLHiddenLineShader *GLHiddenLineShader3;
	TGLHiddenLineShader *GLHiddenLineShader4;
	TGLHiddenLineShader *GLHiddenLineShader5;
	void __fastcall GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift, int X,
		  int Y);
	void __fastcall CheckBox1Click(TObject *Sender);
	void __fastcall CheckBox2Click(TObject *Sender);
	void __fastcall CheckBox3Click(TObject *Sender);
	void __fastcall CheckBox4Click(TObject *Sender);
	void __fastcall CheckBox5Click(TObject *Sender);
	void __fastcall CheckBox6Click(TObject *Sender);
private:	// User declarations
   int mx, my;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
