//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGraph"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma link "GLVectorFileObjects"

#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  SetGLSceneMediaDir();
  // load Cg proggy from project directory
  CgShader1->VertexProgram->LoadFromFile("Shaders\\cg_texture_vp.cg");
  MemoVertCode->Lines->Assign(CgShader1->VertexProgram->Code);
  CgShader1->FragmentProgram->LoadFromFile("Shaders\\cg_texture_fp.cg");
  MemoFragCode->Lines->Assign(CgShader1->FragmentProgram->Code);

  // Load images from media dir
  GLMatLib->Materials->Items[0]->Material->Texture->Image->LoadFromFile("moon.bmp");
  GLMatLib->Materials->Items[1]->Material->Texture->Image->LoadFromFile("clover.jpg");
  GLMatLib->Materials->Items[2]->Material->Texture->Image->LoadFromFile("marbletiles.jpg");
  GLMatLib->Materials->Items[3]->Material->Texture->Image->LoadFromFile("chrome_buckle.bmp");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CgShader1Initialize(TCustomCgShader *CgShader)
{
	// Due to parameter shadowing (ref. Cg Manual), parameters that doesn't change
	// once set can be assigned for once in the OnInitialize event.
	//  with   do begin
	CgShader1->FragmentProgram->ParamByName("Map0")->SetToTextureOf(GLMatLib->Materials->Items[0]);
	CgShader1->FragmentProgram->ParamByName("Map1")->SetToTextureOf(GLMatLib->Materials->Items[1]);
	CgShader1->FragmentProgram->ParamByName("Map2")->SetToTextureOf(GLMatLib->Materials->Items[2]);
	CgShader1->FragmentProgram->ParamByName("Map3")->SetToTextureOf(GLMatLib->Materials->Items[3]);
	// Alternatively, you can set texture parameters using two other methods:
	//CgShader1->FragmentProgram->SetTexture("Map0", GLMatLib->Materials->Items[0]->Material->Texture->Handle);
	//or
	//CgShader1->FragmentProgram->ParamByName("Map0")->SetAsTexture2D(Materials->Items[0]->Material->Texture->Handle);
	// Display profiles used
	LabelVertProfile->Caption = "Using profile: " +
	 CgShader1->VertexProgram->GetProfileStringA();
	LabelFragProfile->Caption = "Using profile: " +
	 CgShader1->FragmentProgram->GetProfileStringA();
}
//---------------------------------------------------------------------------

 float conv1(TTrackBar *TrackBar)
 {
  int half;
  half = TrackBar->Max/2;
  return (TrackBar->Position-half) / half;
 }

void __fastcall TForm1::CgShader1ApplyVP(TCgProgram *CgProgram, TObject *Sender)
{
  Glvectorgeometry::TVector v;
  CgProgram->ParamByName("ModelViewProj")->SetAsStateMatrix(CG_GL_MODELVIEW_PROJECTION_MATRIX, CG_GL_MATRIX_IDENTITY);
// Alternatively, you can set it using:
// CgProgram->SetStateMatrix("ModelViewProj", CG_GL_MODELVIEW_PROJECTION_MATRIX, CG_GL_MATRIX_IDENTITY);

  v = VectorMake(conv1(TrackBar1), conv1(TrackBar2), conv1(TrackBar3), conv1(TrackBar4) );
  CgProgram->ParamByName("shifts")->SetAsVector(v);
}
//---------------------------------------------------------------------------

 float conv2(TTrackBar *TrackBar)
 {
  int half;
  half = TrackBar->Max/2;
  return (TrackBar->Position-half) / half;
 }

void __fastcall TForm1::CgShader1ApplyFP(TCgProgram *CgProgram, TObject *Sender)
{
  Glvectorgeometry::TVector v;
  CgProgram->ParamByName("Map0")->EnableTexture();
  CgProgram->ParamByName("Map1")->EnableTexture();
  CgProgram->ParamByName("Map2")->EnableTexture();
  CgProgram->ParamByName("Map3")->EnableTexture();

  v = VectorMake(conv2(TrackBar5), conv2(TrackBar6), conv2(TrackBar7), conv2(TrackBar8));

  CgProgram->ParamByName("weights")->SetAsVector(v);

}
//---------------------------------------------------------------------------

void __fastcall TForm1::CgShader1UnApplyFP(TCgProgram *CgProgram)
{
	CgProgram->ParamByName("Map0")->DisableTexture();
	CgProgram->ParamByName("Map1")->DisableTexture();
	CgProgram->ParamByName("Map2")->DisableTexture();
	CgProgram->ParamByName("Map3")->DisableTexture();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CBVertexProgramClick(TObject *Sender)
{
   CgShader1->VertexProgram->Enabled = !CBVertexProgram->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CBFragmentProgramClick(TObject *Sender)
{
  CgShader1->FragmentProgram->Enabled = !CBFragmentProgram->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ButtonApplyFPClick(TObject *Sender)
{
  CgShader1->FragmentProgram->Code = MemoFragCode->Lines;
  ButtonApplyFP->Enabled = false;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ButtonApplyVPClick(TObject *Sender)
{
  CgShader1->VertexProgram->Code = MemoVertCode->Lines;
  ButtonApplyVP->Enabled = false;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::MemoFragCodeChange(TObject *Sender)
{
  ButtonApplyFP->Enabled = true;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::MemoVertCodeChange(TObject *Sender)
{
  ButtonApplyVP->Enabled = true;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button1Click(TObject *Sender)
{
  CgShader1->VertexProgram->ListParameters(Memo1->Lines);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button2Click(TObject *Sender)
{
  CgShader1->FragmentProgram->ListParameters(Memo3->Lines);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button3Click(TObject *Sender)
{
  CgShader1->FragmentProgram->ListCompilation(Memo3->Lines);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button4Click(TObject *Sender)
{
  CgShader1->VertexProgram->ListCompilation(Memo1->Lines);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
   mx = X;
   my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
   if (Shift.Contains(ssLeft) || Shift.Contains(ssRight))
   {
	  GLCamera1->MoveAroundTarget(my-Y, mx-X);
	  mx = X;
	  my = Y;
   }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
   GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
	if (Glcrossplatform::PtInRect(ClientRect, ScreenToClient(MousePos)))
	{
	  GLCamera1->SceneScale = GLCamera1->SceneScale * (1000 - WheelDelta) / 1000;
	  Handled = true;
	}
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
	PanelFPS->Caption = Format("%.1f fps",
	   ARRAYOFCONST ((GLSceneViewer1->FramesPerSecond())));
	GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormKeyPress(TObject *Sender, System::WideChar &Key)
{
  if (Key == 0x27) Close();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CheckBox2Click(TObject *Sender)
{
 CgShader1->Enabled = CheckBox2->Checked;
}
//---------------------------------------------------------------------------

