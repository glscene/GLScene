//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLAsyncTimer"
#pragma link "GLBaseClasses"
#pragma link "GLBumpShader"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLAsyncTimer"
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
  // Load the bunny mesh and scale for viewing
  Bunny->LoadFromFile("bunny.glsm");
  Bunny->Scale->Scale((float)(2/Bunny->BoundingSphereRadius()));

  // Load the normal map
  GLMaterialLibrary1->Materials->Items[0]->Material->Texture->Image->LoadFromFile("bunnynormals.jpg");

  // Link the lights to their toggles
  CheckBox1->Tag = Integer(WhiteLight);
  CheckBox2->Tag = Integer(RedLight);
  CheckBox3->Tag = Integer(BlueLight);
  Shape1->Tag = Integer(WhiteLight);
  Shape2->Tag = Integer(RedLight);
  Shape3->Tag = Integer(BlueLight);

  ComboBox1->ItemIndex = 0;
  ComboBox1Change(NULL);
  StartHeight = Height;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
  // Orbit the camera
  if ((dx != 0) || (dy != 0))
  {
	Camera->MoveAroundTarget(dy, dx);
	dx = 0;
	dy = 0;
  }

  // Rotate the light sources
  if (CheckBox4->Checked)
	DCLights->Turn(deltaTime*20);

  GLSceneViewer1->Invalidate();

}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox1Click(TObject *Sender)
{
  WhiteLight->Shining = CheckBox1->Checked;
  CheckBox1->Tag = CheckBox1->Checked;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox2Click(TObject *Sender)
{
  RedLight->Shining = CheckBox1->Checked;
  CheckBox2->Tag = CheckBox2->Checked;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::CheckBox3Click(TObject *Sender)
{
  BlueLight->Shining = CheckBox1->Checked;
  CheckBox3->Tag = CheckBox3->Checked;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::Shape1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
		  int X, int Y)
{
  // WhiteLight Color Dialog
  ColorDialog1->Color = Shape1->Brush->Color;
  if (ColorDialog1->Execute())
  {
	Shape1->Brush->Color = ColorDialog1->Color;
	WhiteLight->Diffuse->AsWinColor = ColorDialog1->Color;
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Shape2MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
		  int X, int Y)
{
  // RedLight Color Dialog
  ColorDialog1->Color = Shape2->Brush->Color;
  if (ColorDialog1->Execute())
  {
	Shape2->Brush->Color = ColorDialog1->Color;
	RedLight->Diffuse->AsWinColor = ColorDialog1->Color;
 }
}

//---------------------------------------------------------------------------
void __fastcall TForm1::Shape3MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
		  int X, int Y)
{
  // BlueLight Color Dialog
  ColorDialog1->Color = Shape3->Brush->Color;
  if (ColorDialog1->Execute())
  {
	Shape3->Brush->Color = ColorDialog1->Color;
	BlueLight->Diffuse->AsWinColor = ColorDialog1->Color;
   }
}

//---------------------------------------------------------------------------
void __fastcall TForm1::ComboBox1Change(TObject *Sender)
{
  if (ComboBox1->Text == "Per-Vertex")
	Bunny->Material->LibMaterialName = " ";
  else
  if (ComboBox1->Text == "Dot3 Texture Combiner")
  {
	Bunny->Material->LibMaterialName = "Bump";
	GLBumpShader1->BumpMethod = bmDot3TexCombiner;
  }
  else
  if (ComboBox1->Text == "Basic Fragment Program")
  {
	Bunny->Material->LibMaterialName = "Bump";
	GLBumpShader1->BumpMethod = bmBasicARBFP;
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
  mx = X;
  my = Y;
  dx = 0;
  dy = 0;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
  if (Shift.Contains(ssLeft))
  {
	dx += (mx-X);
	dy += (my-Y);
  }
  else
  {
	dx = 0;
	dy = 0;
  }
  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::AsyncTimer1Timer(TObject *Sender)
{
  LabelFPS->Caption  = GLSceneViewer1->FramesPerSecondText();
  GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormResize(TObject *Sender)
{
  Camera->SceneScale = (float)Height/StartHeight;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1BeforeRender(TObject *Sender)
{
  if (IsInitialized)
	exit;

  if  (GL_ARB_multitexture && GL_ARB_vertex_program && GL_ARB_texture_env_dot3)
	ComboBox1->Items->Add("Dot3 Texture Combiner");
  if  (GL_ARB_multitexture && GL_ARB_vertex_program && GL_ARB_fragment_program)
  {
	ComboBox1->Items->Add("Basic Fragment Program");
	if (GLSceneViewer1->Buffer->LimitOf[limNbTextureUnits] < 3)
	  GLBumpShader1->SpecularMode = smOff;
  }
  IsInitialized = true;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ComboBox2Change(TObject *Sender)
{
 switch (ComboBox2->ItemIndex)
 {
  case 0 : GLBumpShader1->SpecularMode = smOff; break;
  case 1 : GLBumpShader1->SpecularMode = smBlinn; break;
  case 2 : GLBumpShader1->SpecularMode = smPhong; break;
 default: ;
 }
}
//---------------------------------------------------------------------------

