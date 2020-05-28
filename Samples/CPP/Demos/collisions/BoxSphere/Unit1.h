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
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGLSceneViewer *Viewer;
	TPanel *Panel2;
	TLabel *Label5;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *Label1;
	TLabel *Label7;
	TCheckBox *CheckBox06;
	TCheckBox *CheckBox04;
	TCheckBox *CheckBox05;
	TCheckBox *CheckBox07;
	TEdit *Edit1;
	TEdit *Edit2;
	TEdit *Edit3;
	TUpDown *UpDown1;
	TUpDown *UpDown2;
	TUpDown *UpDown3;
	TEdit *Edit4;
	TEdit *Edit5;
	TEdit *Edit6;
	TUpDown *UpDown4;
	TUpDown *UpDown5;
	TUpDown *UpDown6;
	TEdit *Edit7;
	TEdit *Edit8;
	TEdit *Edit9;
	TUpDown *UpDown7;
	TUpDown *UpDown8;
	TUpDown *UpDown9;
	TEdit *Edit10;
	TUpDown *UpDown10;
	TButton *Button3;
	TButton *Button4;
	TGLScene *GLScene;
	TGLDummyCube *DCCamTarget;
	TGLLightSource *GLLightSource1;
	TGLLightSource *GLLightSource2;
	TGLCube *GLCube1;
	TGLDummyCube *DCCube1;
	TGLSphere *GLSphere1;
	TGLSphere *GLSphere2;
	TGLXYZGrid *GLXYZGrid1;
	TGLLines *GLLines1;
	TGLLines *GLLines3;
	TGLCamera *GLCamera1;
	TGLCadencer *GLCadencer;
	void __fastcall Button4Click(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall CheckBox04Click(TObject *Sender);
	void __fastcall Edit1Change(TObject *Sender);
	void __fastcall Button3Click(TObject *Sender);
	void __fastcall ViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall GLCadencerProgress(TObject *Sender, const double deltaTime, const double newTime);
	void __fastcall ViewerMouseMove(TObject *Sender, TShiftState Shift, int X, int Y);
	void __fastcall FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled);


private:	// User declarations
	int mdx, mdy;
	TAffineVector intersPoint, ResNormal, BoxScale, SpherePos;
	Glvectorgeometry::TMatrix BoxMatrix;
	float SphereRadius;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
