//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCgShader"
#pragma link "GLCrossPlatform"
#pragma link "GLMaterial"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLGraph"
#pragma link "GLObjects"
#pragma link "GLVectorFileObjects"
#pragma link "GLFile3DS"

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
   // Load Cg proggy from project directory
	SetCurrentDir(ExtractFilePath(Application->ExeName));
	CgShader1->VertexProgram->LoadFromFile("simple_vp.cg");
	MemoVertCode->Lines->Assign(CgShader1->VertexProgram->Code);

	CgShader1->FragmentProgram->LoadFromFile("simple_fp.cg");
	MemoFragCode->Lines->Assign(CgShader1->FragmentProgram->Code);

	CgShader1->VertexProgram->Enabled = false;
	CgShader1->FragmentProgram->Enabled = false;

	ButtonApplyFP->Enabled = false;
	ButtonApplyVP->Enabled = false;

	// Bind shader to the material
	GLMaterialLibrary1->Materials->Items[0]->Shader = CgShader1;

	// Load the teapot model from media directory.
	SetGLSceneMediaDir();
	// Note that GLScene will alter the ModelView matrix
	// internally for GLScene objects like TGLCylinder & TGLSphere, and Cg shader
	// is not aware of that. If you apply a vertex shader on those objects, they
	// would appear scaled and/or rotated.
	GLFreeForm1->LoadFromFile("Teapot.3ds");
}
//---------------------------------------------------------------------------


void __fastcall TForm1::CgShader1ApplyVP(TCgProgram *CgProgram, TObject *Sender)
{
  Glvectorgeometry::TVector v;
  TCgParameter *Param;
  // rotate light vector for the "simple lighting" vertex program
  v = ZHmgVector;
  RotateVector(v, YVector, GLCadencer1->CurrentTime);

  Param = CgProgram->ParamByName("LightVec");
  Param->AsVector = v;
  // or using plain Cg API: cgGLSetParameter4fv(Param.Handle, @v);

  // set uniform parameters that change every frame
  CgProgram->ParamByName("ModelViewProj")->SetAsStateMatrix(CG_GL_MODELVIEW_PROJECTION_MATRIX, CG_GL_MATRIX_IDENTITY);
  CgProgram->ParamByName("ModelViewIT")->SetAsStateMatrix(CG_GL_MODELVIEW_MATRIX, CG_GL_MATRIX_INVERSE_TRANSPOSE);
  //  Or, using plain Cg API:
  //  Param = CgProgram->ParamByName("ModelViewIT");
  //  cgGLSetStateMatrixParameter(Param->Handle, CG_GL_MODELVIEW_MATRIX, CG_GL_MATRIX_INVERSE_TRANSPOSE);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CgShader1Initialize(TCustomCgShader *CgShader)
{
  // Shows the profiles to be used. The latest support profiles would be detected
  // if you have CgShader1.VertexProgram.Profile set to vpDetectLatest (similarly
  // for the fragment program).
  LabelVertProfile->Caption = "Using profile: " + CgShader1->VertexProgram->GetProfileStringA();
  LabelFragProfile->Caption = "Using profile: " + CgShader1->FragmentProgram->GetProfileStringA();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CBVertexProgramClick(TObject *Sender)
{
  CgShader1->VertexProgram->Enabled = CBVertexProgram->Checked;
}
//---------------------------------------------------------------------------


void __fastcall TForm1::CBFragmentProgramClick(TObject *Sender)
{
  CgShader1->FragmentProgram->Enabled = CBFragmentProgram->Checked;
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
   mx = X;  my = Y;
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

