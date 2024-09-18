//---------------------------------------------------------------------------

#include <vcl.h>
#include <System.SysUtils.hpp>
#pragma hdrstop

#include "fTexFormatC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Coordinates"

#pragma link "GLS.HUDObjects"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma resource "*.dfm"
TFormFormats* FormFormats;
//---------------------------------------------------------------------------
__fastcall TFormFormats::TFormFormats(TComponent* Owner) : TForm(Owner) {}
//---------------------------------------------------------------------------
void __fastcall TFormFormats::FormCreate(TObject* Sender)
{
    TSearchRec sr;
    int i;

    PathToData = GetCurrentAssetPath();
    SetCurrentDir(PathToData + "\\texture");
    // collect JPeg textures from the demos' media directory
    i = FindFirst("*.jpg", faAnyFile, sr);
    while (i == 0) {
		CBImage->Items->Add(sr.Name);
        i = FindNext(sr);
    }
    FindClose(sr);
    // default selection
    CBFormat->ItemIndex = 0;
    CBCompression->ItemIndex = 0;
    CBImage->ItemIndex = 0;
    CBImageChange(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TFormFormats::CBImageChange(TObject* Sender)
{
    // adjust settings from selection and reload the texture map
	HUDSprite1->Material->Texture->TextureFormat =
        TGLTextureFormat((int)tfRGB + CBFormat->ItemIndex);
    HUDSprite1->Material->Texture->Compression =
        TGLTextureCompression((int)tcNone + CBCompression->ItemIndex);
    HUDSprite1->Material->Texture->Image->LoadFromFile(CBImage->Text);
    LAPicSize->Caption = IntToStr(HUDSprite1->Material->Texture->Image->Width) +
                         " x " +
                         IntToStr(HUDSprite1->Material->Texture->Image->Height);
    if (RBDefault->Checked) {
        HUDSprite1->Width = HUDSprite1->Material->Texture->Image->Width;
        HUDSprite1->Height = HUDSprite1->Material->Texture->Image->Height;
    } else {
        if (RBDouble->Checked) {
            HUDSprite1->Width = HUDSprite1->Material->Texture->Image->Width * 2;
            HUDSprite1->Height =
                HUDSprite1->Material->Texture->Image->Height * 2;
        } else {
			HUDSprite1->Width = HUDSprite1->Material->Texture->Image->Width * 4;
			HUDSprite1->Height =
                HUDSprite1->Material->Texture->Image->Height * 4;
        }
    }
    FormResize(Sender);
    newSelection = true;
}
//---------------------------------------------------------------------------
void __fastcall TFormFormats::FormResize(TObject* Sender)
{
    // re-center the HUDSprite
    HUDSprite1->Position->X = GLSceneViewer1->Width / 2;
    HUDSprite1->Position->Y = GLSceneViewer1->Height / 2;
    GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TFormFormats::GLSceneViewer1AfterRender(TObject* Sender)
{
    int rgb;
    // update compression stats, only the 1st time after a new selection
    if (newSelection) {
        rgb = HUDSprite1->Material->Texture->Image->Width *
              HUDSprite1->Material->Texture->Image->Height * 4;
        LARGB32->Caption = Format(
            "RGBA 32bits would require %d kB", ARRAYOFCONST((rgb / 1024)));
        LAUsedMemory->Caption = Format("Required memory : %d kB",
            ARRAYOFCONST(
                (HUDSprite1->Material->Texture->TextureImageRequiredMemory() /
                    1024)));
        LACompression->Caption = Format("Compression ratio : %d %%",
            ARRAYOFCONST((100 - 100 *
									HUDSprite1->Material->Texture
										->TextureImageRequiredMemory() /
                                    rgb)));
        newSelection = false;
    }
}
//---------------------------------------------------------------------------
void __fastcall TFormFormats::RBDefaultClick(TObject* Sender)
{
    CBImageChange(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TFormFormats::RBDoubleClick(TObject* Sender)
{
    CBImageChange(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TFormFormats::RBQuadClick(TObject* Sender)
{
    CBImageChange(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TFormFormats::CBCompressionChange(TObject* Sender)
{
    CBImageChange(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TFormFormats::CBFormatChange(TObject* Sender)
{
    CBImageChange(Sender);
}
//---------------------------------------------------------------------------

