//---------------------------------------------------------------------------

#include <vcl.h>
#include <System.SysUtils.hpp>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLHUDObjects"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
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
	  TSearchRec sr;
	  int i;
	  SetGLSceneMediaDir();
	  // collect JPeg textures from the demos' media directory
	  i=FindFirst("*.jpg", faAnyFile, sr);
	  while (i == 0){
		CBImage->Items->Add(sr.Name);
        i=FindNext(sr);
	  }
	  FindClose(sr);
	  // default selection
	  CBFormat->ItemIndex = 0;
	  CBCompression->ItemIndex = 0;
	  CBImage->ItemIndex = 0;
	  CBImageChange(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CBImageChange(TObject *Sender)
{
		// adjust settings from selection and reload the texture map
	   HUDSprite1->Material->Texture->TextureFormat = TGLTextureFormat((int)tfRGB+CBFormat->ItemIndex);
	   HUDSprite1->Material->Texture->Compression = TGLTextureCompression((int)tcNone+CBCompression->ItemIndex);
	   HUDSprite1->Material->Texture->Image->LoadFromFile(CBImage->Text);
	   LAPicSize->Caption = IntToStr(HUDSprite1->Material->Texture->Image->Width) + " x " + IntToStr(HUDSprite1->Material->Texture->Image->Height);
	   if (RBDefault->Checked){
		   HUDSprite1->Width = HUDSprite1->Material->Texture->Image->Width;
		   HUDSprite1->Height = HUDSprite1->Material->Texture->Image->Height;
	   }else {
		   if (RBDouble->Checked){
			   HUDSprite1->Width = HUDSprite1->Material->Texture->Image->Width * 2;
			   HUDSprite1->Height = HUDSprite1->Material->Texture->Image->Height * 2;
		   } else {
			   HUDSprite1->Width = HUDSprite1->Material->Texture->Image->Width * 4;
			   HUDSprite1->Height = HUDSprite1->Material->Texture->Image->Height * 4;
		   }
	   }
	   FormResize(Sender);
	   newSelection = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormResize(TObject *Sender)
{
   // re-center the HUDSprite
     HUDSprite1->Position->X = GLSceneViewer1->Width / 2;
	 HUDSprite1->Position->Y = GLSceneViewer1->Height / 2;
     GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1AfterRender(TObject *Sender)
{
	 int rgb;
   // update compression stats, only the 1st time after a new selection
	 if (newSelection){
		 rgb = HUDSprite1->Material->Texture->Image->Width * HUDSprite1->Material->Texture->Image->Height * 4;
		 LARGB32->Caption = Format("RGBA 32bits would require %d kB", ARRAYOFCONST((rgb / 1024)));
		 LAUsedMemory->Caption =  Format("Required memory : %d kB",
								   ARRAYOFCONST((HUDSprite1->Material->Texture->TextureImageRequiredMemory() / 1024)));
		 LACompression->Caption=Format("Compression ratio : %d %%",
									ARRAYOFCONST((100-100*HUDSprite1->Material->Texture->TextureImageRequiredMemory() / rgb)));
         newSelection = false;
	 }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::RBDefaultClick(TObject *Sender)
{
	CBImageChange(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::RBDoubleClick(TObject *Sender)
{
	CBImageChange(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::RBQuadClick(TObject *Sender)
{
	CBImageChange(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CBCompressionChange(TObject *Sender)
{
	CBImageChange(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CBFormatChange(TObject *Sender)
{
	CBImageChange(Sender);
}
//---------------------------------------------------------------------------
