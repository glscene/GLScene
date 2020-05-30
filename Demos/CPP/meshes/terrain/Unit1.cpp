//---------------------------------------------------------------------------

#include <vcl.h>
#include <GLKeyboard.hpp>
#include <stdlib.h>

#pragma hdrstop

#include "Unit1.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLLensFlare"
#pragma link "GLVectorGeometry"
#pragma link "GLSMBASS"
#pragma link "GLSound"
#pragma link "GLWin32Viewer"
#pragma link "GLSkydome"
#pragma link "GLBitmapFont"
#pragma link "GLHUDObjects"
#pragma link "GLTexture"
#pragma link "GLCadencer"
#pragma link "GLHeightData"
#pragma link "GLObjects"
#pragma link "GLTerrainRenderer"
#pragma link "GLScene"
#pragma link "GLKeyboard"
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLMaterial"
#pragma link "GLFileMP3"
#pragma resource "*.dfm"
TForm1 *Form1;

float random(void)
{
  return (float)(rand() & 0x1FFF) / (float)0x1FFF;
}

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
  SetGLSceneMediaDir();
  // 8 MB height data cache
  // Note this is the data size in terms of elevation samples, it does not
  // take into account all the data required/allocated by the renderer
  GLBitmapHDS1->MaxPoolSize = 8 * 1024 * 1024;
  // specify height map data
  GLBitmapHDS1->Picture->LoadFromFile("terrain.bmp");
  // load the texture maps
  GLMaterialLibrary1->Materials->Items[0]->Material->Texture->Image->
	LoadFromFile("snow512.jpg");
  GLMaterialLibrary1->Materials->Items[1]->Material->Texture->Image->
	LoadFromFile("detailmap.jpg");
  SPMoon->Material->Texture->Image->LoadFromFile("moon.bmp");
  SPSun->Material->Texture->Image->LoadFromFile("flare1.bmp");
  // apply texture map scale (our heightmap size is 256)
  TerrainRenderer1->TilesPerTexture = 256.0 / TerrainRenderer1->TileSize;
  // load Bitmap Font
  BitmapFont1->Glyphs->LoadFromFile("darkgold_font.bmp");
  // load and setup sound samples
  GLSoundLibrary->Samples->Add()->LoadFromFile("ChillyWind.mp3");
  GLSoundLibrary->Samples->Add()->LoadFromFile("howl.mp3");
  // Could've been done at design time, but then it hurts the eyes ;)
  GLSceneViewer1->Buffer->BackgroundColor = clWhite;
  // Move camera starting point to an interesting hand-picked location
  DummyCube1->Position->X = 570;
  DummyCube1->Position->Z = -385;
  DummyCube1->Turn(90);
  // Initial camera height offset (controled with pageUp/pageDown)
  FCamHeight = 10;

  randomize();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject * Sender,
											const double deltaTime,
											const double newTime)
{
  float speed;

  // handle keypresses
  if(IsKeyDown(VK_SHIFT))
	speed = 5 * deltaTime;
  else
	speed = deltaTime;
  TGLCoordinates *c = GLCamera1->Position;
  if(IsKeyDown(VK_UP))
	DummyCube1->Translate(c->Z * speed, 0, -c->X * speed);
  if(IsKeyDown(VK_DOWN))
	DummyCube1->Translate(-c->Z * speed, 0, c->X * speed);
  if(IsKeyDown(VK_LEFT))
	DummyCube1->Translate(-c->X * speed, 0, -c->Z * speed);
  if(IsKeyDown(VK_RIGHT))
	DummyCube1->Translate(c->X * speed, 0, c->Z * speed);
  if(IsKeyDown(VK_PRIOR))
	FCamHeight = FCamHeight + 10 * speed;
  if(IsKeyDown(VK_NEXT))
	FCamHeight = FCamHeight - 10 * speed;
  if(IsKeyDown(VK_ESCAPE))
	Close();

  // don't drop through terrain!
  DummyCube1->Position->Y =
	TerrainRenderer1->InterpolatedHeight(DummyCube1->Position->AsVector) +
	FCamHeight;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseDown(TObject * Sender,
                                                TMouseButton Button,
                                                TShiftState Shift, int X, int Y)
{
  my = Y;
  mx = X;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject * Sender,
												TShiftState Shift, int X, int Y)
{
  if(Shift.Contains(ssLeft))
  {
	GLCamera1->MoveAroundTarget((my - Y) * 0.5, (mx - X) * 0.5);
	mx = X;
	my = Y;
  }
}

//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject * Sender)
{
  String s;
//  s.printf("%.1f FPS - %d", GLSceneViewer1->FramesPerSecond(),
//		   TerrainRenderer1->LastTriangleCount());
  HUDText1->Text = s;
  GLSceneViewer1->ResetPerformanceMonitor();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::FormKeyPress(TObject * Sender, char &Key)
{
  TGLMaterial *fp;
  TGLFogEnvironment *fe;
  TGIFColor  Color;

  switch (Key)
  {
  case 'w':
  case 'W':
	fp = GLMaterialLibrary1->Materials->Items[0]->Material;
	if(fp->PolygonMode == pmLines)
	  fp->PolygonMode = pmFill;
	else
	  fp->PolygonMode = pmLines;
	break;
  case '+':
	if(GLCamera1->DepthOfView < 2000)
	{
	  GLCamera1->DepthOfView = GLCamera1->DepthOfView * 1.2;
	  fe = GLSceneViewer1->Buffer->FogEnvironment;
	  fe->FogEnd = fe->FogEnd * 1.2;
	  fe->FogStart = fe->FogStart * 1.2;
	}
	break;
  case '-':
	if(GLCamera1->DepthOfView > 300)
    {
      GLCamera1->DepthOfView = GLCamera1->DepthOfView / 1.2;
      fe = GLSceneViewer1->Buffer->FogEnvironment;
      fe->FogEnd = fe->FogEnd / 1.2;
      fe->FogStart = fe->FogStart / 1.2;
	}
    break;
  case '*':
	if(TerrainRenderer1->CLODPrecision > 20)
	  TerrainRenderer1->CLODPrecision =
		Round(TerrainRenderer1->CLODPrecision * 0.8);
	break;
  case '/':
	if(TerrainRenderer1->CLODPrecision < 1000)
	  TerrainRenderer1->CLODPrecision =
		Round(TerrainRenderer1->CLODPrecision * 1.2);
	break;
  case '8':
	if(TerrainRenderer1->QualityDistance > 40)
	  TerrainRenderer1->QualityDistance =
		Round(TerrainRenderer1->QualityDistance * 0.8);
	break;
  case '9':
	if(TerrainRenderer1->QualityDistance < 1000)
	  TerrainRenderer1->QualityDistance =
		Round(TerrainRenderer1->QualityDistance * 1.2);
	break;
  case 'n':
  case 'N':
	if(SkyDome1->Stars->Count == 0)
	{
	  // turn on 'night' mode
	  Color.Red = 0; Color.Green = 0; Color.Blue = 8;
	  SkyDome1->Bands->Items[0]->StopColor->AsWinColor = TGIFColorMap::RGB2Color(Color);
	  Color.Red = 0; Color.Green = 0; Color.Blue = 0;
	  SkyDome1->Bands->Items[0]->StartColor->AsWinColor = TGIFColorMap::RGB2Color(Color);
	  Color.Red = 0; Color.Green = 0; Color.Blue = 16;
	  SkyDome1->Bands->Items[1]->StopColor->AsWinColor =  TGIFColorMap::RGB2Color(Color);
	  Color.Red = 0; Color.Green = 0; Color.Blue = 8;
	  SkyDome1->Bands->Items[1]->StartColor->AsWinColor =  TGIFColorMap::RGB2Color(Color);

	  SkyDome1->Stars->AddRandomStars(700, clWhite, True);      // many white stars
	  Color.Red = 255; Color.Green = 100; Color.Blue = 100;
	  SkyDome1->Stars->AddRandomStars(100, TGIFColorMap::RGB2Color(Color), True); // some redish ones
	  Color.Red = 100; Color.Green = 100; Color.Blue = 255;
	  SkyDome1->Stars->AddRandomStars(100, TGIFColorMap::RGB2Color(Color), True); // some blueish ones
	  Color.Red = 255; Color.Green = 255; Color.Blue = 100;
	  SkyDome1->Stars->AddRandomStars(100, TGIFColorMap::RGB2Color(Color), True); // some yellowish ones

	  GLSceneViewer1->Buffer->BackgroundColor = Graphics::clBlack;
	  fe = GLSceneViewer1->Buffer->FogEnvironment;
	  fe->FogColor->AsWinColor = Graphics::clBlack;
	  fe->FogStart = -fe->FogStart;     // Fog is used to make things darker

	  SPMoon->Visible = True;
	  SPSun->Visible = False;
	  GLLensFlare->Visible = False;
	}
	break;
  case 'd':
  case 'D':
    if(SkyDome1->Stars->Count > 0)
	{
      // turn on 'day' mode
      SkyDome1->Bands->Items[1]->StopColor->Color = clrNavy;
      SkyDome1->Bands->Items[1]->StartColor->Color = clrBlue;
      SkyDome1->Bands->Items[0]->StopColor->Color = clrBlue;
      SkyDome1->Bands->Items[0]->StartColor->Color = clrWhite;
      SkyDome1->Stars->Clear();
      GLSceneViewer1->Buffer->BackgroundColor = clWhite;
      fe = GLSceneViewer1->Buffer->FogEnvironment;
	  fe->FogColor->AsWinColor = clWhite;
      fe->FogStart = -fe->FogStart;

	  GLSceneViewer1->Buffer->FogEnvironment->FogStart = 0;
      SPMoon->Visible = False;
      SPSun->Visible = True;
    }
    break;
  case 't':
    if(SkyDome1->Options.Contains(sdoTwinkle))
      SkyDome1->Options = SkyDome1->Options << sdoTwinkle;
	else
      SkyDome1->Options = SkyDome1->Options >> sdoTwinkle;
    break;
  case 'l':
    GLLensFlare->Visible = (!GLLensFlare->Visible) && SPSun->Visible;
  }
  Key = '\0';
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TISoundTimer(TObject * Sender)
{
  Glvectorgeometry::TVector wolfPos;
  float c, s;
  TGLBSoundEmitter *be;

  if(!GLSMBASS1->Active)
	return;
  if(SkyDome1->Stars->Count == 0)
  {
    // wind blows around camera
    be = GetOrCreateSoundEmitter(GLCamera1);
    be->Source->SoundLibrary = GLSoundLibrary;
    be->Source->SoundName = GLSoundLibrary->Samples->Items[0]->Name;
    be->Source->Volume = random() * 0.5 + 0.5;
    be->Playing = True;
  }
  else
  {
    // wolf howl at some distance, at ground level
    wolfPos = GLCamera1->AbsolutePosition;
    SinCosine(random() * Glvectorgeometry::c2PI, 100 + random(1000), s, c);
	wolfPos.X = wolfPos.X + c;
	wolfPos.Z = wolfPos.Z + s;
	wolfPos.Y = TerrainRenderer1->InterpolatedHeight(wolfPos);
	DCSound->Position->AsVector = wolfPos;
	be = GetOrCreateSoundEmitter(DCSound);
	be->Source->SoundLibrary = GLSoundLibrary;
	be->Source->SoundName = GLSoundLibrary->Samples->Items[1]->Name;
	be->Source->MinDistance = 100;
	be->Source->MaxDistance = 4000;
	be->Playing = True;
  }
  TISound->Enabled = False;
  TISound->Interval = 10000 + random(10000);
  TISound->Enabled = True;
}

//---------------------------------------------------------------------------

