//---------------------------------------------------------------------------

#include <vcl.h>
#include <stdlib.h>
#pragma hdrstop

#include "Unit1.h"
#include "tga.hpp"
#include "jpeg.hpp"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLMaterial"
#pragma link "tga"

#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------

void TForm1::AlignControlsToTree()
{
  TrackBar1->Position = GLTree1->Depth;
  TrackBar2->Position = Round(GLTree1->BranchTwist);
  TrackBar3->Position = Round(GLTree1->BranchAngle * 100);
  TrackBar4->Position = Round(GLTree1->BranchAngleBias * 100);
  TrackBar5->Position = Round(GLTree1->BranchSize * 10);
  TrackBar6->Position = Round(GLTree1->BranchRadius * 25);
  TrackBar7->Position = Round(GLTree1->BranchNoise * 100);
  TrackBar8->Position = Round(GLTree1->LeafSize * 100);
  TrackBar9->Position = Round(GLTree1->LeafThreshold * 100);
  TrackBar10->Position = GLTree1->BranchFacets;
  Edit1->Text = IntToStr(GLTree1->Seed);
  CheckBox1->Checked = GLTree1->CentralLeader;
  TrackBar11->Position = Round(GLTree1->CentralLeaderBias * 100);
}

void TForm1::NewTree()
{
  delete GLTree1;
  GLTree1 = (TGLTree *) GLScene1->Objects->AddNewChild(__classid(TGLTree));

  GLTree1->MaterialLibrary = GLMaterialLibrary1;
  GLTree1->LeafMaterialName = "LeafFront";
  GLTree1->LeafBackMaterialName = "LeafBack";
  GLTree1->BranchMaterialName = "Branch";
  GLTree1->Depth = 8;
  GLTree1->LeafSize = 0.2;
  GLTree1->BranchRadius = 0.08;
  GLTree1->BranchNoise = 0.5;

  Randomize();
  GLTree1->Seed = Round((2 * Random() - 1) * (MaxInt - 1));

  AlignControlsToTree();
}

__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
  SetGLSceneMediaDir();
  // Set up default textures
  TGLLibMaterial *lm =
	GLMaterialLibrary1->AddTextureMaterial("LeafFront", "maple_multi.tga", true);
  lm->Material->BlendingMode = bmAlphaTest50;
  lm->Material->Texture->TextureMode = tmModulate;
  lm->Material->Texture->TextureFormat = tfRGBA;

  lm = GLMaterialLibrary1->AddTextureMaterial("LeafBack", "maple_multi.tga", true);
  lm->Material->BlendingMode = bmAlphaTest50;
  lm->Material->Texture->TextureMode = tmModulate;
  lm->Material->Texture->TextureFormat = tfRGBA;

  lm = GLMaterialLibrary1->AddTextureMaterial("Branch", "zbark_016.jpg", true);
  lm->Material->Texture->TextureMode = tmModulate;

  // Set a up a tree
  NewTree();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseDown(TObject * Sender,
												TMouseButton Button,
												TShiftState Shift, int X, int Y)
{
  mx = X;
  my = Y;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject * Sender,
												TShiftState Shift, int X, int Y)
{
  if(Shift.Contains(ssLeft))
	GLCamera1->MoveAroundTarget(my - Y, mx - X);
  else if(Shift.Contains(ssRight))
	GLCamera1->AdjustDistanceToTarget(1 + (my - Y) * 0.01);
  mx = X;
  my = Y;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar1Change(TObject * Sender)
{
  GLTree1->Depth = (float) TrackBar1->Position;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar2Change(TObject * Sender)
{
  GLTree1->BranchTwist = (float)TrackBar2->Position;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar3Change(TObject * Sender)
{
  GLTree1->BranchAngle = (float)TrackBar3->Position / 100;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar4Change(TObject * Sender)
{
  GLTree1->BranchAngleBias = (float)TrackBar4->Position / 100;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar5Change(TObject * Sender)
{
  GLTree1->BranchSize = (float)TrackBar5->Position / 10;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar6Change(TObject * Sender)
{
  GLTree1->BranchRadius = (float)TrackBar6->Position / 25;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar7Change(TObject * Sender)
{
  GLTree1->BranchNoise = (float)TrackBar7->Position / 100;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar8Change(TObject * Sender)
{
  GLTree1->LeafSize = (float)TrackBar8->Position / 100;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar9Change(TObject * Sender)
{
  GLTree1->LeafThreshold = (float)TrackBar9->Position / 100;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar10Change(TObject * Sender)
{
  GLTree1->BranchFacets = (float)TrackBar10->Position;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::Button1Click(TObject * Sender)
{
  try
  {
	GLTree1->Seed = StrToInt(Edit1->Text);
  }
  catch(Exception * E)
  {
	MessageDlg("Invalid input", mtWarning, TMsgDlgButtons() << mbOK, 0);
	Edit1->Text = IntToStr(GLTree1->Seed);
  }
}

//---------------------------------------------------------------------------

void __fastcall TForm1::CheckBox1Click(TObject * Sender)
{
  GLTree1->CentralLeader = CheckBox1->Checked;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar11Change(TObject * Sender)
{
  GLTree1->CentralLeaderBias = (float)TrackBar11->Position / 100;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::NewTree1Click(TObject * Sender)
{
  NewTree();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::LoadTree1Click(TObject * Sender)
{
  if(!OpenDialog1->Execute())
    return;

  GLTree1->LoadFromFile(OpenDialog1->FileName);
  AlignControlsToTree();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::SaveTree1Click(TObject * Sender)
{
  if(!SaveDialog1->Execute())
    return;

  GLTree1->SaveToFile(SaveDialog1->FileName);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::ExportMesh1Click(TObject * Sender)
{
  if(!SaveDialog2->Execute())
    return;

  GLTree1->BuildMesh(GLFreeForm1);
  GLFreeForm1->SaveToFile(SaveDialog2->FileName);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::ExportMaterialLibrary1Click(TObject * Sender)
{
  if(!SaveDialog3->Execute())
    return;

  GLMaterialLibrary1->SaveToFile(SaveDialog3->FileName);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::Exit1Click(TObject * Sender)
{
  Form1->Close();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::LeafFrontTexture1Click(TObject * Sender)
{
  if(!OpenPictureDialog1->Execute())
    return;

  TGLLibMaterial *lm =
    GLMaterialLibrary1->Materials->GetLibMaterialByName("LeafFront");
  lm->Material->Texture->Image->LoadFromFile(OpenPictureDialog1->FileName);
  GLTree1->StructureChanged();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::LeafBackTexture1Click(TObject * Sender)
{
  if(!OpenPictureDialog1->Execute())
    return;

  TGLLibMaterial *lm =
    GLMaterialLibrary1->Materials->GetLibMaterialByName("LeafBack");
  lm->Material->Texture->Image->LoadFromFile(OpenPictureDialog1->FileName);
  GLTree1->StructureChanged();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::BranchTexture1Click(TObject * Sender)
{
  if(!OpenPictureDialog1->Execute())
    return;

  TGLLibMaterial *lm =
    GLMaterialLibrary1->Materials->GetLibMaterialByName("Branch");
  lm->Material->Texture->Image->LoadFromFile(OpenPictureDialog1->FileName);
  GLTree1->StructureChanged();
}

//---------------------------------------------------------------------------

