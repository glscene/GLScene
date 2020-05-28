//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLMaterial"

#pragma resource "*.dfm"
TForm1 *Form1;

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
  GLMaterialLibrary1->LibMaterialByName("details")->Material->Texture->Image->
	LoadFromFile("detailmap.jpg");
  SPSun->Material->Texture->Image->LoadFromFile("flare1.bmp");

  // apply texture map scale (our heightmap size is 256)
  TerrainRenderer1->TilesPerTexture = 1;        //256/TerrainRenderer1.TileSize;
  TerrainRenderer1->MaterialLibrary = GLMaterialLibrary1;

  // Could've been done at design time, but then it hurts the eyes ;)
  GLSceneViewer1->Buffer->BackgroundColor = clWhite;
  // Initial camera height offset (controled with pageUp/pageDown)
  FCamHeight = 10;

  // initialize intensity texture
  TBIntensityChange(this);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::FormShow(TObject * Sender)
{
  TBSubSamplingChange(this);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLBumpmapHDS1NewTilePrepared(TGLBumpmapHDS * Sender,
													 TGLHeightData * heightData,
													 TGLLibMaterial *
													 normalMapMaterial)
{
  Glvectorgeometry::TVector n;

  heightData->MaterialName = normalMapMaterial->Name;
  normalMapMaterial->Texture2Name = "contrast";
  normalMapMaterial->Shader = GLTexCombineShader1;
  normalMapMaterial->Material->MaterialOptions =
	normalMapMaterial->Material->MaterialOptions << moNoLighting;
  n = VectorNormalize(SPSun->AbsolutePosition);
  ScaleVector(n, 0.5);
  n.Y = -n.Y;
  n.Z = -n.Z;
  AddVector(n, 0.5);
  normalMapMaterial->Material->FrontProperties->Diffuse->Color = n;
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

  TGLCoordinates *p = GLCamera1->Position;
  if(IsKeyDown(VK_UP))
	DummyCube1->Translate(-p->X * speed, 0, -p->Z * speed);
  if(IsKeyDown(VK_DOWN))
	DummyCube1->Translate(p->X * speed, 0, p->Z * speed);
  if(IsKeyDown(VK_LEFT))
	DummyCube1->Translate(-p->Z * speed, 0, p->X * speed);
  if(IsKeyDown(VK_RIGHT))
	DummyCube1->Translate(p->Z * speed, 0, -p->X * speed);
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
  GLSceneViewer1->SetFocus();
  mx = X;
  my = Y;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject * Sender,
                                                TShiftState Shift, int X, int Y)
{
  if(Shift.Contains(ssLeft))
  {
    GLCamera1->MoveAroundTarget((my - Y) * 0.5, (mx - X) * 0.5);
    my = Y;
    mx = X;
  }
}

//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject * Sender)
{
  Caption = GLSceneViewer1->FramesPerSecondText(1);
  GLSceneViewer1->ResetPerformanceMonitor();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::FormKeyPress(TObject * Sender, char &Key)
{
  TGLMaterial *fp;
  TGLFogEnvironment *fe;
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
    if(GLCamera1->DepthOfView < 2000.0)
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
	if(TerrainRenderer1->CLODPrecision > 10)
      TerrainRenderer1->CLODPrecision =
        Round(TerrainRenderer1->CLODPrecision * 0.8);
    break;
  case '/':
    if(TerrainRenderer1->CLODPrecision < 1000)
      TerrainRenderer1->CLODPrecision =
        Round(TerrainRenderer1->CLODPrecision * 1.2);
    break;
  case 'l':
    GLLensFlare->Visible = (!GLLensFlare->Visible) && SPSun->Visible;
    break;
  }
  Key = '\0';
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1BeforeRender(TObject * Sender)
{
  GLLensFlare->PreRender((TGLSceneBuffer *) Sender);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TBSubSamplingChange(TObject * Sender)
{
  GLBumpmapHDS1->SubSampling = (1 << TBSubSampling->Position);
  String s;
  s.printf(L"(%d) -> BumpMaps are %dx",
           GLBumpmapHDS1->SubSampling,
           TerrainRenderer1->TileSize / GLBumpmapHDS1->SubSampling);
  LASubFactor->Caption = s;
  // don't leave the focus to the trackbar, otherwise it'll keep some keystrokes
  // for itself, like the arrow keys
  SetFocus();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TBIntensityChange(TObject * Sender)
{
  int i;
  Graphics::TBitmap * bmp;

  TGLMaterial *m = GLMaterialLibrary1->LibMaterialByName("contrast")->Material;
  bmp = new Graphics::TBitmap;
  try
  {
    bmp->PixelFormat = pf24bit;
    bmp->Width = 1;
    bmp->Height = 1;
    i = 255;
	bmp->Canvas->Pixels[0][0] = (Graphics::TColor) RGB(i, i, i);
	m->Texture->Image->Assign(bmp);
  }
  __finally
  {
    delete bmp;
  }
  i = (TBIntensity->Position * 255) / 100;
  m->Texture->EnvColor->AsWinColor = (Graphics::TColor) RGB(i, i, i);

  LABumpIntensity->Caption = IntToStr(TBIntensity->Position) + " %";
}

//---------------------------------------------------------------------------

