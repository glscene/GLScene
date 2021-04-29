//---------------------------------------------------------------------------

#ifndef fMandelCH
#define fMandelCH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GR32_ExtImage.hpp"
#include "GR32_Image.hpp"
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtDlgs.hpp>
#include <Vcl.Menus.hpp>
//---------------------------------------------------------------------------

typedef
/*
  TRasterizerKind = (rkRegular, rkProgressive, rkSwizzling, rkTesseral,
   rkContour, rkMultithreadedRegularRasterizer);
  TSamplerKind = (skDefault, skSS2X, skSS3X, skSS4X, skPattern2, skPattern3,
	skPattern4);
  TMandelbrotPalette = (mpGR32, mpRainbow, mpMonochrome, mpSimple);
*/

  class TMandelbrotSampler : public TCustomSampler
  {
  private:
	FPalette: array of TColor32;
	FWidthInv, FHeightInv: Single;
  protected:
	void CalculatePalette();
  public:
	int MaxIterations;
	TFloatRect Bounds;
	TCustomPaintBox32 *Image;
	TMandelbrotPalette *Palette;
	constructor Create(AImage: TCustomPaintBox32);
	TColor32 GetSampleFloat(TFloat X, Y); override;
	void PrepareSampling; override;
  };


class TFormMandelC : public TForm
{
__published:	// IDE-managed Components
	TSyntheticImage32 *Img;
	TMainMenu *MainMenu;
	TMenuItem *miFile;
	TMenuItem *miSave;
	TMenuItem *N3;
	TMenuItem *miExit;
	TMenuItem *miRasterizer;
	TMenuItem *miRegularSampling;
	TMenuItem *miProgressive;
	TMenuItem *miSwizzling;
	TMenuItem *miTesseral;
	TMenuItem *miContour;
	TMenuItem *miMultithreadedRegularRasterizer;
	TMenuItem *miSuperSampler;
	TMenuItem *miDefault;
	TMenuItem *N5;
	TMenuItem *miSuperSampler2x;
	TMenuItem *miSuperSampler3x;
	TMenuItem *miSuperSampler4x;
	TMenuItem *miAdaptive;
	TMenuItem *N2;
	TMenuItem *miPatternSampler2x;
	TMenuItem *miPatternSampler3x;
	TMenuItem *miPatternSampler4x;
	TMenuItem *miMaxIterations;
	TMenuItem *miMaxIterations50;
	TMenuItem *miMaxIterations160;
	TMenuItem *miMaxIterations256;
	TMenuItem *miMaxIterations320;
	TMenuItem *miMaxIterations512;
	TMenuItem *miPalette;
	TMenuItem *miPaletteDefault;
	TMenuItem *miPaletteRainbow;
	TMenuItem *miPaletteMonochrome;
	TMenuItem *miPaletteSimple;
	TSavePictureDialog *SavePictureDialog;
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
	TRasterizer *Rasterizer;
	TCustomSampler *Sampler;
	TMandelbrotSampler *MandelSampler;
	TSuperSampler *SuperSampler;
	TAdaptiveSuperSampler *AdaptiveSampler;
	TPatternSampler *JitteredSampler;
	TSamplerKind *SamplerKind;
	void __fastcall SelectRasterizer(TRasterizerKind *RasterizerKind);
	void __fastcall SelectSampler(TSamplerKind *ASamplerKind);

	__fastcall TFormMandelC(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormMandelC *FormMandelC;
//---------------------------------------------------------------------------
#endif
