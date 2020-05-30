//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLHeightData"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLTerrainRenderer"
#pragma link "GLWin32Viewer"

#pragma link "GLHeightData"
#pragma link "GLHeightData"
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
   int i;
   TBitmap *bmp;

   // 8 MB height data cache
   // Note this is the data size in terms of elevation samples, it does not
   // take into account all the data required/allocated by the renderer
   GLCustomHDS->MaxPoolSize = 8*1024*1024;
   // Move camera starting point to an interesting hand-picked location
   DummyCube1->Position->X = 50;
   DummyCube1->Position->Z = 150;
   // Initial camera height offset (controled with pageUp/pageDown)
   FCamHeight = 20;
   // We build several basic 1D textures which are just color ramps
   // all use automatic texture mapping corodinates, in ObjectLinear method
   // (ie. texture coordinates for a vertex depend on that vertex coordinates)
   bmp = new TBitmap;
   bmp->PixelFormat = pf24bit;
   bmp->Width = 256;
   bmp->Height = 1;
   // Black-White ramp, autotexture maps to Z coordinate
   // This one changes with altitude, this is a quick way to obtain
   // altitude-dependant coloring
   for (i=0; i < 255; i++)
	  bmp->Canvas->Pixels[i][0] = RGB(i, i, i);
   GLMaterialLibrary1->AddTextureMaterial("BW", bmp)->Material->Texture->MappingMode = tmmObjectLinear;
   GLMaterialLibrary1->AddTextureMaterial("BW", bmp)->Material->Texture->MappingSCoordinates->AsVector = VectorMake(0, 0, 0.0001, 0);
   // Red, Blue map linearly to X and Y axis respectively
   for (i=0; i < 255; i++)
	  bmp->Canvas->Pixels[i][0] = RGB(i, 0, 0);
   GLMaterialLibrary1->AddTextureMaterial("Red", bmp)->Material->Texture->MappingMode = tmmObjectLinear;
   GLMaterialLibrary1->AddTextureMaterial("Red", bmp)->Material->Texture->MappingSCoordinates->AsVector = VectorMake(0.1, 0, 0, 0);
   for (i=0; i < 255; i++)
	  bmp->Canvas->Pixels[i][0] = RGB(0, 0, i);

   GLMaterialLibrary1->AddTextureMaterial("Blue", bmp)->Material->Texture->MappingMode = tmmObjectLinear;
   GLMaterialLibrary1->AddTextureMaterial("Blue", bmp)->Material->Texture->MappingSCoordinates->AsVector = VectorMake(0, 0.1, 0, 0);
   bmp->Free();
   TerrainRenderer1->MaterialLibrary = GLMaterialLibrary1;
}

//---------------------------------------------------------------------------
//
// The beef : this event does all the interesting elevation data stuff
//

void __fastcall TForm1::GLCustomHDSStartPreparingData(TGLHeightData *HeightData)
{
   int x, y;
   TByteRaster rasterLine;
   TGLHeightDataType oldType;
   TByteVector * b;
   float d, dy;

   HeightData->DataState = hdsPreparing;
   // retrieve data
   oldType = HeightData->DataType;
   HeightData->Allocate(hdtByte);
   // Cheap texture changed (32 is our tileSize = 2^5)
   // This basicly picks a texture for each tile depending on the tile's position
   switch ((((HeightData->XLeft ^ HeightData->YTop) << 5) && 3))
   {
   case  0, 3 : HeightData->MaterialName = "BW"; break;
   case  1 : HeightData->MaterialName = "Blue";  break;
   case  2 : HeightData->MaterialName = "Red";  break;
   default:
	   ;
   }
	// 'Cheap' elevation data : this is just a formula z=f(x, y)
	for (y=HeightData->YTop; y < HeightData->YTop+HeightData->Size-1; y++)
	{
	 rasterLine = HeightData->ByteRaster[y-HeightData->YTop];
	 dy = y*y;
	 for (x = HeightData->XLeft; x < HeightData->XLeft+HeightData->Size-1; x++)
	 {
		d = sqrt(x*x+dy);
		b->data[0,0,0] = (float) Round(128+128*Sin(d*0.2)/(d*0.1+1));
		rasterLine[x-HeightData->XLeft] = b;
	 }
	}
	if (oldType != hdtByte)
	 HeightData->DataType = oldType;

   ///inherited;
}
//---------------------------------------------------------------------------
// Movement, mouse handling etc.

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
   if (Shift.Contains(ssLeft))
   {
	  GLCamera1->MoveAroundTarget(my-Y, mx-X);
	  mx = X;
	  my = Y;
   }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
   Caption = "Synthetic Terrain " + Format("%.1f FPS - %d",
	   ARRAYOFCONST ((GLSceneViewer1->FramesPerSecond(),
		  TerrainRenderer1->LastTriangleCount)));
   GLSceneViewer1->ResetPerformanceMonitor();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::FormKeyPress(TObject *Sender, System::WideChar &Key)
{
   switch (Key)
   {
   case '+' : if (GLCamera1->DepthOfView < 4000)
		 {
		 GLCamera1->DepthOfView = GLCamera1->DepthOfView*1.2;
		 GLSceneViewer1->Buffer->FogEnvironment->FogEnd =
			GLSceneViewer1->Buffer->FogEnvironment->FogEnd*1.2;
		 GLSceneViewer1->Buffer->FogEnvironment->FogStart =
			GLSceneViewer1->Buffer->FogEnvironment->FogStart*1.2;
		 } break;
	case '-' : if (GLCamera1->DepthOfView > 300)
		 {
		 GLCamera1->DepthOfView = GLCamera1->DepthOfView/1.2;
		 GLSceneViewer1->Buffer->FogEnvironment->FogEnd =
		   GLSceneViewer1->Buffer->FogEnvironment->FogEnd/1.2;
		 GLSceneViewer1->Buffer->FogEnvironment->FogStart =
		   GLSceneViewer1->Buffer->FogEnvironment->FogStart/1.2;
		 } break;
   case  '*' : if (TerrainRenderer1->CLODPrecision > 5)
	   TerrainRenderer1->CLODPrecision = Round(TerrainRenderer1->CLODPrecision*0.8);
	   break;
   case  '/' : if (TerrainRenderer1->CLODPrecision<500)
		 TerrainRenderer1->CLODPrecision = Round(TerrainRenderer1->CLODPrecision*1.2);
		 break;
   case  '8' : if (TerrainRenderer1->QualityDistance>40)
		 TerrainRenderer1->QualityDistance = Round(TerrainRenderer1->QualityDistance*0.8);
		 break;
   case  '9' : if (TerrainRenderer1->QualityDistance<1000)
		 TerrainRenderer1->QualityDistance = Round(TerrainRenderer1->QualityDistance*1.2);
		 break;
   default:
	   ;
   }

   Key = 0x0;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
   float speed;

   // handle keypresses
   if (IsKeyDown(VK_SHIFT))
	 speed = 5*deltaTime;
   else
	 speed = deltaTime;


	  if (IsKeyDown(VK_RIGHT))
		 DummyCube1->Translate(GLCamera1->Position->Z*speed, 0,
							  -GLCamera1->Position->X*speed);
	  if (IsKeyDown(VK_LEFT))
		 DummyCube1->Translate(-GLCamera1->Position->Z*speed, 0,
								GLCamera1->Position->X*speed);
	  if (IsKeyDown(VK_UP))
		 DummyCube1->Translate(-GLCamera1->Position->X*speed, 0,
							  -GLCamera1->Position->Z*speed);
	  if (IsKeyDown(VK_DOWN))
		 DummyCube1->Translate(GLCamera1->Position->X*speed, 0,
		                       GLCamera1->Position->Z*speed);
	  if (IsKeyDown(VK_PRIOR))
		 FCamHeight = FCamHeight+10*speed;
	  if (IsKeyDown(VK_NEXT))
		 FCamHeight = FCamHeight-10*speed;
	  if (IsKeyDown(VK_ESCAPE))
		 Close();
   // don't drop through terrain!
   DummyCube1->Position->Y = TerrainRenderer1->InterpolatedHeight(DummyCube1->Position->AsVector)+FCamHeight;
}
//---------------------------------------------------------------------------

