//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

#pragma link "GLFile3DS"
#pragma link "GLFileMD2"
#pragma link "GLProxyObjects"

#pragma link "GLBaseClasses"
#pragma link "GLBitmapFont"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLDCE"
#pragma link "GLHeightData"
#pragma link "GLHUDObjects"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLTerrainRenderer"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLWindowsFont"
#pragma resource "*.dfm"
TForm1 *Form1;

const
  float cForce = 250.0;
const
  int cSpread = 200;
const
  int cNbMushrooms = 20;

float random(void)
{
  return (float)(rand() & 0x1FFF) / (float)0x1FFF;
}

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------

void TForm1::Load()
{
  SetGLSceneMediaDir();
  //Load Materials
  GLMatLib->AddTextureMaterial("Terrain", "snow512.jpg");
  GLMatLib->AddTextureMaterial("Actor", "waste.jpg");

  //Load Terrain
  GLBitmapHDS1->MaxPoolSize = 8 * 1024 * 1024;
  GLBitmapHDS1->Picture->LoadFromFile("terrain.bmp");
  Terrain->Direction->SetVector(0.0, 1.0, 0.0);
  Terrain->Material->LibMaterialName = "Terrain";
  Terrain->TilesPerTexture = 256 / Terrain->TileSize;
  Terrain->Scale->SetVector(1.0, 1.0, 0.02);

  Ground->Material->LibMaterialName = "Terrain";

  // Load mushroom mesh
  //Always use AutoScaling property or you may get some problems
  moMushroom->AutoScaling->SetPoint(0.1, 0.1, 0.1);
  moMushroom->LoadFromFile("Mushroom.3ds");
  moMushroom->Direction->SetVector(0.0, 1.0, 0.0);
  moMushroom->BuildOctree();

  //Load player
  Player->Position->SetPoint(0.0, 3.0, 0.0);
  //Actor
  GLActor1->LoadFromFile("Waste.md2");
  GLActor1->Direction->SetVector(0.0, 1.0, 0.0);
  GLActor1->Up->SetVector(1.0, 0.0, 0.0);
  GLActor1->Scale->SetVector(0.05, 0.05, 0.05);
  GLActor1->Material->LibMaterialName = "Actor";
  GLActor1->Animations->LoadFromFile("Quake2Animations.aaf");
  // Define animation properties
  GLActor1->AnimationMode = aamLoop;
  GLActor1->SwitchToAnimation("stand");
  GLActor1->FrameInterpolation = afpLinear;

  //DCE Behaviour
  GLSphere1->Scale->Assign(GetOrCreateDCEDynamic(Player)->Size);
//  GetOrCreateDCEDynamic(Player)->OnCollision = PlayerBehaviours0Collision;
}
//---------------------------------------------------------------------------
void TForm1::AddBall()
{
  TGLSphere *Ball;
  float S;
  float pX,pY,pZ;
  float cR,cG,cB;

  Ball = (TGLSphere *)(Balls->AddNewChild(__classid(TGLSphere)));
  //with Ball do
  Ball->Tag = 1; //set the identifier of a ball
  Ball->Radius = 1.0;
  S = (float)(100.0 + random(900.0))/ 500.0;
  Ball->Scale->SetVector(S, S, S);
  pX = random(40.0) - random(40.0);
  pY = 4.0 + random(10.0);
  pZ = random(40.0) - random(40.0);
  Ball->Position->SetPoint(pX, pY, pZ);
  cR = (float)(100.0 + random(900.0))/1000;
  cG = (float)(100.0 + random(900.0))/1000;
  cB = (float)(100.0 + random(900.0))/1000;
  Ball->Material->FrontProperties->Diffuse->SetColor(cR, cG, cB);
  GetOrCreateDCEDynamic(Ball)->Manager = GLDCEManager1;
  GetOrCreateDCEDynamic(Ball)->BounceFactor = 0.75;
  GetOrCreateDCEDynamic(Ball)->Friction = 0.1;
  GetOrCreateDCEDynamic(Ball)->SlideOrBounce =   csbBounce;
  GetOrCreateDCEDynamic(Ball)->Size->Assign(Ball->Scale);
}

//---------------------------------------------------------------------------

void TForm1::AddMushrooms()
{
  int i;
  TGLFreeFormProxy *Proxy;
  Glvectorgeometry::TVector s;
  float f;

  // spawn some more mushrooms using proxy objects
  for (i = 0; i < cNbMushrooms - 1; i++) {
	// create a new Proxy and set its MasterObject property
	Proxy = (TGLFreeFormProxy *)(Mushrooms->AddNewChild(__classid(TGLFreeFormProxy)));
	Proxy->ProxyOptions = Proxy->ProxyOptions << pooObjects;
	Proxy->MasterObject = moMushroom;
	// retrieve reference attitude
	Proxy->Direction = moMushroom->Direction;
	Proxy->Up = moMushroom->Up;
	// randomize scale
	s = moMushroom->Scale->AsVector;
	f = 2.0 * random() + 1.0;
	ScaleVector(s, f);
	Proxy->Scale->AsVector = s;
	// randomize position
	Proxy->Position->SetPoint(
		 (random(cSpread) - (float)(cSpread / 2.0)),
		 (moMushroom->Position->Z + 1.5 * f),
		 (random(cSpread) - (float)(cSpread / 2.0)));
	// randomize orientation
	Proxy->RollAngle = random(360.0);
	Proxy->TransformationChanged();

	GetOrCreateDCEStatic(Proxy)->Manager = GLDCEManager1;
	GetOrCreateDCEStatic(Proxy)->BounceFactor = 0.75;
	GetOrCreateDCEStatic(Proxy)->Friction = 10.0;
	GetOrCreateDCEStatic(Proxy)->Shape = csFreeform;
  }
}

//---------------------------------------------------------------------------

void TForm1::HandleAnimation()
{
  String anim;
  if (VectorNorm(GetOrCreateDCEDynamic(Player)->Speed) > 0.1)
	anim = "run";
  else
	anim = "stand";

  if (Jumped==true)
  {
	if (!GetOrCreateDCEDynamic(Player)->InGround)
	  anim = "jump";
	else
	  Jumped = false;
  }
  if (anim == "jump")
	GLActor1->Interval = 500;
  else
	GLActor1->Interval = 100;
  if (GLActor1->CurrentAnimation() != anim)
	GLActor1->SwitchToAnimation(anim);
}


//---------------------------------------------------------------------------

void TForm1::HandleKeys()
{
  TAffineVector Force;
  Force = NullVector;

  if (IsKeyDown(VK_ESCAPE)) Close();

  if (IsKeyDown('w') || IsKeyDown('W') || IsKeyDown(VK_UP))
	Force.Z = cForce;
  if (IsKeyDown('s') || IsKeyDown('S') || IsKeyDown(VK_DOWN))
	Force.Z = -cForce;
  if (IsKeyDown('a') || IsKeyDown('A') || IsKeyDown(VK_LEFT))
	Force.X = cForce;
  if (IsKeyDown('d') || IsKeyDown('D') || IsKeyDown(VK_RIGHT))
	Force.X = -cForce;
  GetOrCreateDCEDynamic(Player)->ApplyAccel(Force);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift)

{
  int i;

  if (Key == VK_F1)
	AddBall();
  if (Key == VK_F2)
	for (i = 1; i <= 10; i++)
	  AddBall();
  if (Key == VK_F3)
	AddMushrooms();
  if (Key == VK_SPACE)
  {
	GetOrCreateDCEDynamic(Player)->Jump(1.0, 20.0);
	Jumped = true;
  }
  if (Key == VK_F4)
  {
	Terrain->Visible = false;
	Ground->Visible = true;
	GetOrCreateDCEStatic(Terrain)->Active = false;
	GetOrCreateDCEStatic(Ground)->Active = true;
  }
  if (Key == VK_F5)
	GLDCEManager1->ManualStep = !GLDCEManager1->ManualStep;

  if (Key = VK_RETURN) {
	Player->Position->SetPoint(0.0, 3.0, 0.0);
	Balls->DeleteChildren();
	Mushrooms->DeleteChildren();
	Help->ModulateColor->Alpha = 1.0;
	Terrain->Visible = true;
	Ground->Visible = false;
	GetOrCreateDCEStatic(Terrain)->Active = true;
	GetOrCreateDCEStatic(Ground)->Active = false;
  }
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
  //Mouse look
  if (Shift.Contains(ssLeft))
  {
	GLCamera1->MoveAroundTarget((my - Y), 0);
	Player->Turn(-(mx - X));
  }
  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
  HandleKeys();
  HandleAnimation();
  //This shows the manual progress, don't need this if you use the automatic mode
  if (GLDCEManager1->ManualStep)
	GLDCEManager1->Step(deltaTime);

  Help->ModulateColor->Alpha = Help->ModulateColor->Alpha - (deltaTime * 0.05);
  if (Help->ModulateColor->Alpha < 0.25)
	Help->ModulateColor->Alpha = 0.25;
  HelpShadow->ModulateColor->Alpha = Help->ModulateColor->Alpha;
  HelpShadow->Text = Help->Text;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormShow(TObject *Sender)
{
  Load();
  GLCadencer1->Enabled = true;
  Help->Text =
   "Mouse Drag - Look\n\r\
	A,W,S,D - movement\n\r\
	SPACE - Jump\n\r\
	F1 - Add one ball\n\r\
	F2 - Add 10 balls\n\r\
	F3 - Add 20 mushrooms\n\r\
	F4 - Change ground to box\n\r\
	F5 - Toggle step mode\n\r\
	RETURN - Reset";
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  String s;
  if (GLDCEManager1->ManualStep)
	s = "Manual";
  else
	s = "Automatic";
  GLHUDText1->Text = Format("FPS: %.1f - Dynamics: %d - Statics: %d - Step mode: %s",
	 ARRAYOFCONST((GLSceneViewer1->FramesPerSecond(),GLDCEManager1->DynamicCount,GLDCEManager1->StaticCount,s)));
  GLSceneViewer1->ResetPerformanceMonitor();

}
//---------------------------------------------------------------------------

void __fastcall TForm1::PlayerBehaviours0Collision(TObject *Sender,
	  TGLBaseSceneObject *ObjectCollided, TDCECollision &CollisionInfo)
{
  //Use some kind of identifier to know what object you are colliding
  //You can use the Tag, TagFloat, Name, Class
  TAffineVector v;
  if (ObjectCollided->Tag == 1)
  {
	v = AffineVectorMake(VectorSubtract(ObjectCollided->AbsolutePosition, Player->AbsolutePosition));
	NormalizeVector(v);
	ScaleVector(v, 400.0);
	GetOrCreateDCEDynamic(ObjectCollided)->StopAbsAccel();
	GetOrCreateDCEDynamic(ObjectCollided)->ApplyAbsAccel(v);
  }
}
//---------------------------------------------------------------------------


void __fastcall TForm1::GLDirectOpenGL1Render(TObject *Sender, TGLRenderContextInfo &rci)

{
  int i;
  TAffineVector p, n;

  //To use this you will need to enable the debug define in the
  //GLEllipseCollision.pas, if you do, don't forget to clear the
  //triangle list! -> SetLength(debug_tri,0);

  rci.GLStates->PointSize = 5.0;
  glColor3f(0.0, 1.0, 0.0);

  for (i = 0; i < debug_tri.High; i++) {
	glColor3f(0.0, 0.0, 0.0);
	glBegin(GL_LINE_STRIP);
	glVertex3f(debug_tri[i].p1.X, debug_tri[i].p1.Y, debug_tri[i].p1.Z);
	glVertex3f(debug_tri[i].p2.X, debug_tri[i].p2.Y, debug_tri[i].p2.Z);
	glVertex3f(debug_tri[i].p3.X, debug_tri[i].p3.Y, debug_tri[i].p3.Z);
	glEnd;
	CalcPlaneNormal(debug_tri[i].p1, debug_tri[i].p2, debug_tri[i].p3, n);
	ScaleVector(n, 0.25);
	p.X = (debug_tri[i].p1.X + debug_tri[i].p2.X + debug_tri[i].p3.X) / 3.0;
	p.Y = (debug_tri[i].p1.Y + debug_tri[i].p2.Y + debug_tri[i].p3.Y) / 3.0;
	p.Z = (debug_tri[i].p1.Z + debug_tri[i].p2.Z + debug_tri[i].p3.Z) / 3.0;
	glColor3f(0.0, 0.0, 1.0);
	glBegin(GL_LINE_STRIP);
	glVertex3f(p.X, p.Y, p.Z);
	glVertex3f(p.X + n.X, p.Y + n.Y, p.Z + n.Z);
	glEnd;
	glBegin(GL_POINTS);
	glVertex3f(p.X + n.X, p.Y + n.Y, p.Z + n.Z);
	glEnd;
	}
  debug_tri.Length = 0.0;
}
//---------------------------------------------------------------------------


