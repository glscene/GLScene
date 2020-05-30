//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H

#include <vcl.h>
#include <tchar.h>

//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <ExtDlgs.hpp>
#include <Menus.hpp>

#include <GLVectorFileObjects.hpp>
#include <GLTexture.hpp>
#include <GLTree.hpp>
#include <GLWin32Viewer.hpp>
#include <GLObjects.hpp>
#include <GLScene.hpp>
#include <ComCtrls.hpp>
#include "GLBaseClasses.hpp"
#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLMaterial.hpp"
#include "GLFileTGA.hpp"
#include "GLUtils.hpp"


//---------------------------------------------------------------------------
class TForm1:public TForm
{
__published:                   // IDE-managed Components
  TGLScene * GLScene1;
  TGLSceneViewer *GLSceneViewer1;
  TGLCamera *GLCamera1;
  TGLDummyCube *GLDummyCube1;
  TGLLightSource *GLLightSource1;
  TGLMaterialLibrary *GLMaterialLibrary1;
  TPanel *Panel1;
  TLabel *Label1;
  TLabel *Label2;
  TTrackBar *TrackBar1;
  TTrackBar *TrackBar2;
  TLabel *Label3;
  TTrackBar *TrackBar3;
  TLabel *Label4;
  TTrackBar *TrackBar4;
  TLabel *Label5;
  TTrackBar *TrackBar5;
  TLabel *Label6;
  TTrackBar *TrackBar6;
  TLabel *Label7;
  TTrackBar *TrackBar7;
  TLabel *Label8;
  TTrackBar *TrackBar8;
  TLabel *Label9;
  TTrackBar *TrackBar9;
  TTrackBar *TrackBar10;
  TLabel *Label10;
  TGLFreeForm *GLFreeForm1;
  TMainMenu *MainMenu1;
  TMenuItem *File1;
  TMenuItem *LoadTree1;
  TMenuItem *SaveTree1;
  TMenuItem *N1;
  TMenuItem *Exit1;
  TMenuItem *NewTree1;
  TMenuItem *Material1;
  TMenuItem *LeafFrontTexture1;
  TMenuItem *LeafBackTexture1;
  TMenuItem *BranchTexture1;
  TMenuItem *N2;
  TMenuItem *ExportMesh1;
  TLabel *Label11;
  TOpenDialog *OpenDialog1;
  TSaveDialog *SaveDialog1;
  TGLPlane *GLPlane1;
  TLabel *Label12;
  TEdit *Edit1;
  TButton *Button1;
  TCheckBox *CheckBox1;
  TTrackBar *TrackBar11;
  TSaveDialog *SaveDialog2;
  TSaveDialog *SaveDialog3;
  TMenuItem *ExportMaterialLibrary1;
  TOpenPictureDialog *OpenPictureDialog1;
  void __fastcall GLSceneViewer1MouseDown(TObject * Sender, TMouseButton Button,
                                          TShiftState Shift, int X, int Y);
  void __fastcall GLSceneViewer1MouseMove(TObject * Sender, TShiftState Shift,
                                          int X, int Y);
  void __fastcall TrackBar1Change(TObject * Sender);
  void __fastcall TrackBar2Change(TObject * Sender);
  void __fastcall TrackBar3Change(TObject * Sender);
  void __fastcall TrackBar4Change(TObject * Sender);
  void __fastcall TrackBar5Change(TObject * Sender);
  void __fastcall TrackBar6Change(TObject * Sender);
  void __fastcall TrackBar7Change(TObject * Sender);
  void __fastcall TrackBar8Change(TObject * Sender);
  void __fastcall TrackBar9Change(TObject * Sender);
  void __fastcall TrackBar10Change(TObject * Sender);
  void __fastcall Button1Click(TObject * Sender);
  void __fastcall CheckBox1Click(TObject * Sender);
  void __fastcall TrackBar11Change(TObject * Sender);
  void __fastcall NewTree1Click(TObject * Sender);
  void __fastcall LoadTree1Click(TObject * Sender);
  void __fastcall SaveTree1Click(TObject * Sender);
  void __fastcall ExportMesh1Click(TObject * Sender);
  void __fastcall ExportMaterialLibrary1Click(TObject * Sender);
  void __fastcall Exit1Click(TObject * Sender);
  void __fastcall LeafFrontTexture1Click(TObject * Sender);
  void __fastcall LeafBackTexture1Click(TObject * Sender);
  void __fastcall BranchTexture1Click(TObject * Sender);
private:                       // User declarations
public:                        // User declarations
    __fastcall TForm1(TComponent * Owner);

  int mx, my;
  TGLTree *GLTree1;
  void AlignControlsToTree();
  void NewTree();
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif

