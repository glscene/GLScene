//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fProcCloudsC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "Vcl.Dialogs"
#pragma link "GLS.Texture"
#pragma link "GLS.ProcTextures"
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma resource "*.dfm"

TFormCloudsC *FormCloudsC;
TextFile outfile;

//---------------------------------------------------------------------------
__fastcall TFormCloudsC::TFormCloudsC(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormCloudsC::FormCreate(TObject *Sender)
{
  CBFormat->ItemIndex = 3;
  CBCompression->ItemIndex = 0;
}
//---------------------------------------------------------------------------

void __fastcall TFormCloudsC::CBFormatChange(TObject *Sender)
{
  byte aPERM[255];
  String s;
  int i;

  // adjust settings from selection and reload the texture map
  if (UseCloudFileCB->Checked and (FileExists(CloudFileUsedEdit->Text)))
  {
/*
	try {
	  AssignFile(outfile, CloudFileUsedEdit->Text);
	  Reset(outfile);
	  Readln(outfile, s);
	  for (i = 0; 255; i++)
	  {
		Readln(outfile, s);
		aPERM[i] = StrToInt(s);
	  }
	  }
	__finally
	{
	  CloseFile(outfile);
	}
	}
	((TGLProcTextureNoise*)(Plane->Material->Texture->Image))->SetPermFromData(aPERM);
*/
  }
  else
	((TGLProcTextureNoise*)(Plane->Material->Texture->Image))->SetPermToDefault();
  Plane->Material->Texture->TextureFormat = TGLTextureFormat(Integer(tfRGB) + CBFormat->ItemIndex);
  Plane->Material->Texture->Compression = TGLTextureCompression(Integer(tcNone) +
	CBCompression->ItemIndex);
  ((TGLProcTextureNoise*)(Plane->Material->Texture->Image))->MinCut = SpinEdit1->Value;
  ((TGLProcTextureNoise*)(Plane->Material->Texture->Image))->NoiseSharpness = SpinEdit2->Value / 100;
  ((TGLProcTextureNoise*)(Plane->Material->Texture->Image))->Height = StrToInt(CloudImageSizeUsedEdit->Text);
  ((TGLProcTextureNoise*)(Plane->Material->Texture->Image))->Width = StrToInt(CloudImageSizeUsedEdit->Text);
  ((TGLProcTextureNoise*)(Plane->Material->Texture->Image))->NoiseRandSeed =
	StrToInt(CloudRandomSeedUsedEdit->Text);
  ((TGLProcTextureNoise*)(Plane->Material->Texture->Image))->Seamless = CheckBox2->Checked;

  if (RBDefault->Checked)
  {
	Plane->Width = 50;
	Plane->Height = 50;
  }
  else if (RBDouble->Checked)
  {
	Plane->Width = 100;
	Plane->Height = 100;
  }
  else
  {
	Plane->Width = 400;
	Plane->Height = 400;
  };
  newSelection = true;
}
//---------------------------------------------------------------------------

void __fastcall TFormCloudsC::GLCadencer1TotalProgress(TObject *Sender, const double DeltaTime,
		  const double NewTime)
{
  if (CheckBox1->Checked)
	((TGLProcTextureNoise*)(Plane->Material->Texture->Image))->
	  NoiseAnimate(DeltaTime);

}
//---------------------------------------------------------------------------

void __fastcall TFormCloudsC::TrackBar1Change(TObject *Sender)
{
  Plane->XTiles = TrackBar1->Position;
  Plane->YTiles = TrackBar1->Position;
}
//---------------------------------------------------------------------------

void __fastcall TFormCloudsC::Timer1Timer(TObject *Sender)
{
  LabelFPS->Caption = GLSceneViewer1->FramesPerSecondText();
  GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------

void __fastcall TFormCloudsC::GLSceneViewer1AfterRender(TObject *Sender)
{
  int rgb;
  // update compression stats, only the 1st time after a new selection
  if (newSelection)
	rgb = Plane->Material->Texture->Image->Width *
	Plane->Material->Texture->Image->Height * 4;
  LARGB32->Caption = Format("RGBA 32bits would require %d kB",
	(rgb / 1024));
  LAUsedMemory->Caption = Format("Required memory : %d kB",
	ARRAYOFCONST((Plane->Material->Texture->TextureImageRequiredMemory() / 1024)));
  LACompression->Caption = Format("Compression ratio : %d %%",
	(100 - 100 * Plane->Material->Texture->TextureImageRequiredMemory() / rgb));
  newSelection = false;

}
//---------------------------------------------------------------------------

void __fastcall TFormCloudsC::CloudFileOpenBtnClick(TObject *Sender)
{
  OpenDialog1->Filter = "Cloud base (*.clb)|*.clb";
  OpenDialog1->InitialDir = ExtractFilePath(ParamStr(0));
  OpenDialog1->FileName = "*.clb";
  if (OpenDialog1->Execute())
	CloudFileUsedEdit->Text = OpenDialog1->FileName;

}
//---------------------------------------------------------------------------

void __fastcall TFormCloudsC::MakeAndSaveCloudNoiseFileClick(TObject *Sender)
{
  int i;

/*
  void RandomPerm();
  int Id, Count, More, Less, Again;
  {
	MakeAndSaveCloudNoiseFile->Caption = IntToStr(0);
	Application->ProcessMessages();
	for (Id = 0; 255; Id++)
	{
	  aPERM[Id] = Random(256);
	  // Label61->Caption = IntToStr(Id);
	  // Application->ProcessMessages();
	}
	Count = 0;
	do
	  Again = 0;
	  Less = Random(256);
	  for (Id = 0; Count; Id++)
	  {
		More = aPERM[Id];
		if (Less == More)
		  Again++;
	  }
	  Label61->Caption = IntToStr(Again);
	  // these can be removed.. just for debugging
	  Application->ProcessMessages();
	  if (Again == 0)
	  {
		aPERM[Count + 1] = Less;
		Count++;
		MakeAndSaveCloudNoiseFile->Caption = IntToStr(Less) + "," +
		  IntToStr(Count);
		Application->ProcessMessages();
	  }
	while (Count != 255);
  }

  {
  SaveDialog1->Filter = "Cloud base (*.clb)|*.clb";
  SaveDialog1->InitialDir = ExtractFilePath(ParamStr(0));
  SaveDialog1->DefaultExt = "rnd";
  SaveDialog1->Filename = ".clb";
  if (SaveDialog1->Execute())
  {
	if (UpperCase(ExtractFileExt(SaveDialog1->Filename) == ".CLB")
	{
	  Application->ProcessMessages();
	  Randomize();
	  RandomPerm();
	  try
		AssignFile(outfile, SaveDialog1->Filename);
		// File selected in dialog box
		Rewrite(outfile);
		Writeln(outfile, "Cloud Base V2.0");
		for (i = 0; 255; i++)
		  Writeln(outfile, IntToStr(aPERM[i]));
	  finally
		CloseFile(outfile);
	  }
	  Label61->Caption = "Done";
	  MakeAndSaveCloudNoiseFile->Caption = "";
	}
  }
*/
}
//---------------------------------------------------------------------------

