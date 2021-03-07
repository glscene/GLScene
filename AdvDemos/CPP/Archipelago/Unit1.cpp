// ---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "Unit1.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)

#pragma link "GLS.BaseClasses"
#pragma link "GLS.BitmapFont"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"
#pragma link "GLS.HeightData"
#pragma link "GLS.HeightTileFileHDS"

#pragma link "GLS.Material"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SkyDome"
#pragma link "GLS.TerrainRenderer"
#pragma link "GLS.VectorFileObjects"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.WindowsFont"
#pragma link "GLS.HUDObjects"

#pragma link "GLS.BaseClasses"
#pragma link "GLS.BitmapFont"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"
#pragma link "GLS.HeightData"
#pragma link "GLS.HeightTileFileHDS"
#pragma link "GLS.HUDObjects"
#pragma link "GLS.Material"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.SkyDome"
#pragma link "GLS.TerrainRenderer"
#pragma link "GLS.VectorFileObjects"
#pragma link "GLS.WindowsFont"
#pragma resource "*.dfm"
TForm1 *Form1;

#define cWaterLevel -10000
#define cWaterOpaqueDepth 2000
#define cWaveAmplitude 120

// ---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner) : TForm(Owner) {
}

// ---------------------------------------------------------------------------

void __fastcall TForm1::FormCreate(TObject *Sender) {
	int i, j;
	String name;
	TGLLibMaterial *libMat;
	String DataPath;

//	DataPath = ExtractFilePath(ParamStr(0));
	DataPath = ExtractFilePath(Application->ExeName);
	DataPath += "Data\\";
	SetCurrentDir(DataPath);
	MaterialLibrary->TexturePaths = DataPath;

	GLCustomHDS1->MaxPoolSize = 8 * 1024 * 1024;
	GLCustomHDS1->DefaultHeight = cWaterLevel;

	// load texmaps
	for (i = 0; i <= 3; i++)
		for (j = 0; j <= 3; j++) {
			name = Format("Tex_%d_%d.bmp", ARRAYOFCONST((i, j)));
			if (!FileExists(name)) {
				ShowMessage("Texture file " + name + " not found...\r\
					  Did you run " "splitter->exe"
					" as said in the readme->txt?");
				Application->Terminate();
				Abort();
			}
			libMat = MaterialLibrary->AddTextureMaterial(name, name, false);
			libMat->Material->Texture->TextureMode = tmReplace;
			libMat->Material->Texture->TextureWrap = twNone;
			// comment out to turn off texture compression
			libMat->Material->Texture->Compression = tcStandard;
			libMat->Material->Texture->FilteringQuality = tfAnisotropic;

			libMat->Texture2Name = "detail";
		}

	// Initial camera height offset (controled with pageUp/pageDown)
	CamHeight = 20;

	// Water plane active
	WaterPlane = true;

	// load the sailboat
	FFSailBoat->LoadFromFile("sailboat.glsm");
	MLSailBoat->LoadFromFile("sailboat.glml");
	FFSailBoat->Position->SetPoint(-125*TerrainRenderer->Scale->X, 0,
		-100*TerrainRenderer->Scale->Z);
	FFSailBoat->TurnAngle = -30;
	// boost ambient
	for (i = 0; i < MLSailBoat->Materials->Count; i++)
		MLSailBoat->Materials->Items[i]
			->Material->FrontProperties->Ambient->Color =
			MLSailBoat->Materials->Items[i]
			->Material->FrontProperties->Diffuse->Color;

	// Move camera starting point near the sailboat
	DCCamera->Position = FFSailBoat->Position;
	DCCamera->Translate(25, 0, -15);
	DCCamera->Turn(200);

	// Help text
	HTHelp->Text = "Archipelago Demo\r\
			  * : Increase CLOD precision\r\
			  / : decrease CLOD precision\r\
			  W : wireframe on/off\r\
			  S : sea surface on/off\r\
			  B : sailboat visible on/off\r\
			  Num4 & Num6 : steer the sailboat\r\
			  F1: show this help";
	HTHelp->Position->SetPoint(Screen->Width / 2 - 100,
		Screen->Height / 2 - 150, 0);
	HelpOpacity = 4;
	GLSceneViewer->Cursor = crNone;
}

// ---------------------------------------------------------------------------
void TForm1::ResetMousePos(void) {
	if (GLSceneViewer->Cursor == crNone)
		SetCursorPos(Screen->Width / 2, Screen->Height / 2);
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencerProgress(TObject *Sender,
	const double deltaTime, const double newTime) {
	float Speed, Alpha, f;
	float TerrainHeight, SurfaceHeight;
	TGLVector Sbp;
	POINT NewMousePos;

	// Handle keypresses
	if (IsKeyDown(VK_SHIFT))
		Speed = 100 * deltaTime;
	else
		Speed = 20 * deltaTime;

	if (IsKeyDown(VK_UP))
		DCCamera->Position->AddScaledVector(Speed,
		GLCamera->AbsoluteVectorToTarget());
	if (IsKeyDown(VK_DOWN))
		DCCamera->Position->AddScaledVector(-Speed,
		GLCamera->AbsoluteVectorToTarget());
	if (IsKeyDown(VK_LEFT))
		DCCamera->Position->AddScaledVector(-Speed,
		GLCamera->AbsoluteRightVectorToTarget());
	if (IsKeyDown(VK_RIGHT))
		DCCamera->Position->AddScaledVector(Speed,
		GLCamera->AbsoluteRightVectorToTarget());
	if (IsKeyDown(VK_PRIOR))
		CamHeight = CamHeight + Speed;
	if (IsKeyDown(VK_NEXT))
		CamHeight = CamHeight - Speed;
	if (IsKeyDown(VK_ESCAPE))
		Close();

	if (IsKeyDown(VK_F1))
		HelpOpacity = ClampValue(HelpOpacity + deltaTime * 5, 3, 5);
	if (IsKeyDown(VK_NUMPAD4))
		FFSailBoat->Turn(-deltaTime*3);
	if (IsKeyDown(VK_NUMPAD6))
		FFSailBoat->Turn(deltaTime*3);

	// Mouse movements and actions
	if (IsKeyDown(VK_LBUTTON)) {
		Alpha = DCCamera->Position->Y;
		DCCamera->Position->AddScaledVector(Speed,
			GLCamera->AbsoluteVectorToTarget());
		CamHeight = CamHeight + DCCamera->Position->Y - Alpha;
	}
	if (IsKeyDown(VK_RBUTTON)) {
		Alpha = DCCamera->Position->Y;
		DCCamera->Position->AddScaledVector(-Speed,
			GLCamera->AbsoluteVectorToTarget());
		CamHeight = CamHeight + DCCamera->Position->Y - Alpha;
	}
	GetCursorPos(&NewMousePos);
	GLCamera->MoveAroundTarget((Screen->Height / 2 - NewMousePos.y)*0.25,
		(Screen->Width / 2 - NewMousePos.x)*0.25);
	ResetMousePos();

	// Don"t drop our target through terrain!
	TerrainHeight = TerrainRenderer->InterpolatedHeight
		(DCCamera->Position->AsVector);
	SurfaceHeight = TerrainRenderer->Scale->Z * cWaterLevel / 128;
	if (TerrainHeight < SurfaceHeight)
		TerrainHeight = SurfaceHeight;
	DCCamera->Position->Y = TerrainHeight + CamHeight;

	// Adjust fog distance/color for air/water
	if ((GLCamera->AbsolutePosition.Y > SurfaceHeight) || (!WaterPlane)) {
		if (!WasAboveWater) {
			SkyDome->Visible = true;

			GLSceneViewer->Buffer->FogEnvironment->FogColor->Color = clrWhite;
			GLSceneViewer->Buffer->FogEnvironment->FogEnd = 1000;
			GLSceneViewer->Buffer->FogEnvironment->FogStart = 500;

			GLSceneViewer->Buffer->BackgroundColor = clWhite;
			GLCamera->DepthOfView = 1000;
			WasAboveWater = true;
		}
	}
	else {
		if (WasAboveWater) {
			SkyDome->Visible = false;

			GLSceneViewer->Buffer->FogEnvironment->FogColor->AsWinColor =
				clNavy;
			GLSceneViewer->Buffer->FogEnvironment->FogEnd = 100;
			GLSceneViewer->Buffer->FogEnvironment->FogStart = 0;

   			GLSceneViewer->Buffer->BackgroundColor = clNavy;
			GLCamera->DepthOfView = 100;
			WasAboveWater = false;
		}
	}

	// Visibility of Help
	if (HelpOpacity > 0) {
		HelpOpacity = HelpOpacity - deltaTime;
		Alpha = ClampValue(HelpOpacity, 0, 1);
		if (Alpha > 0) {
			HTHelp->Visible = true;
			HTHelp->ModulateColor->Alpha = Alpha;
		}
		else
			HTHelp->Visible = false;
	}

	// Rock the sailboat
	Sbp = TerrainRenderer->AbsoluteToLocal(FFSailBoat->AbsolutePosition);
	Alpha = WaterPhase(Sbp.X + TerrainRenderer->TileSize * 0.5,
		Sbp.Y + TerrainRenderer->TileSize * 0.5);
	FFSailBoat->Position->Y = (cWaterLevel + sin(Alpha) * cWaveAmplitude) *
		(TerrainRenderer->Scale->Z / 128) - 1.5;
	f = cWaveAmplitude * 0.01;
	FFSailBoat->Up->SetVector(cos(Alpha)*0.02*f, 1,
		(sin(Alpha)*0.02 - 0.005)*f);
	FFSailBoat->Move(deltaTime*2);
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender) {
	HTFPS->Text = Format("%.1f FPS - %d - %d",
		ARRAYOFCONST((GLSceneViewer->FramesPerSecond(),
		TerrainRenderer->LastTriangleCount, WaterPolyCount)));
	GLSceneViewer->ResetPerformanceMonitor();
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::FormKeyPress(TObject *Sender, System::WideChar &Key) {
	int i;
	TGLPolygonMode pm;

	switch (Key) {
	case 'w':
	case 'W': {
		if (MaterialLibrary->Materials->Items[0]->Material->PolygonMode ==
			pmLines)
			pm = pmFill;
		else
			pm = pmLines;
		for (i = 0; i < MaterialLibrary->Materials->Count; i++)
			MaterialLibrary->Materials->Items[i]
				->Material->PolygonMode = pm;

		for (i = 0; i < MLSailBoat->Materials->Count; i++)
			MLSailBoat->Materials->Items[i]->Material->PolygonMode = pm;
			FFSailBoat->StructureChanged();
		break;
		}
	case 's':
	case 'S':
		WaterPlane = !WaterPlane;
		break;
	case 'b':
	case 'B':
		FFSailBoat->Visible = !FFSailBoat->Visible;
		break;
	case '*':
		if (TerrainRenderer->CLODPrecision > 1)
			TerrainRenderer->CLODPrecision =
				(int)(TerrainRenderer->CLODPrecision * 0.8);
		break;
	case '/':
		if (TerrainRenderer->CLODPrecision < 1000)
			TerrainRenderer->CLODPrecision =
				(int)(TerrainRenderer->CLODPrecision * 1.2 + 1);
		break;
	}
	Key = 0x0;
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::GLCustomHDS1MarkDirtyEvent(const TRect &area) {
	GLHeightTileFileHDS1->MarkDirty(area);
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::GLCustomHDS1StartPreparingData(TGLHeightData *heightData)
{
	TGLHeightData *htfHD;
	int i, j, n;
	TTexPoint offset;

	htfHD = GLHeightTileFileHDS1->GetData(heightData->XLeft, heightData->YTop,
		heightData->Size, heightData->DataType);
	if ((htfHD->DataState == hdsNone))
		// or (htfHD->HeightMax<=cWaterLevel-cWaterOpaqueDepth)
			heightData->DataState = hdsNone;
	else {
		i = (heightData->XLeft / 128);
		j = (heightData->YTop / 128);
		if ((Cardinal(i) < 4) & (Cardinal(j) < 4)) {
			heightData->MaterialName =
				Format("Tex_%d_%d.bmp", ARRAYOFCONST((i, j)));
			heightData->TextureCoordinatesMode = tcmLocal;
			n = ((heightData->XLeft / 32) & 3);
			offset.S = n * 0.25;
			n = ((heightData->YTop / 32) & 3);
			offset.T = -n * 0.25;
			heightData->TextureCoordinatesOffset = offset;
			heightData->TextureCoordinatesScale = TexPointMake(0.25, 0.25);
			heightData->DataType = hdtSmallInt;
			htfHD->DataType = hdtSmallInt;
			heightData->Allocate(hdtSmallInt);
			Move(htfHD->SmallIntData, heightData->SmallIntData,
				htfHD->DataSize);
			heightData->DataState = hdsReady;
			heightData->HeightMin = htfHD->HeightMin;
			heightData->HeightMax = htfHD->HeightMax;
		}
		else
			heightData->DataState = hdsNone;
	}
	GLHeightTileFileHDS1->Release(htfHD);
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewerBeforeRender(TObject *Sender) {
	int i, n;

	PAProgress->Left = (Width - PAProgress->Width) / 2;
	PAProgress->Visible = true;
	n = MaterialLibrary->Materials->Count;
	ProgressBar->Max = n - 1;
	try {
		for (i = 0; i < n; i++) {
			ProgressBar->Position = i;
			MaterialLibrary->Materials->Items[i]->Material->Texture->Handle;
			PAProgress->Repaint();
		}
	}
	__finally {
		ResetMousePos();
		PAProgress->Visible = false;
		GLSceneViewer->BeforeRender = NULL;
	}
}

// ---------------------------------------------------------------------------
float TForm1::WaterPhase(const float px, const float py) {
	return GLCadencer->CurrentTime * 1.0 + px * 0.16 + py * 0.09;
}

// ---------------------------------------------------------------------------
float TForm1::WaterHeight(const float px, const float py) {
	float alpha;
	alpha = WaterPhase(px + TerrainRenderer->TileSize * 0.5,
		py + TerrainRenderer->TileSize * 0.5);
	return (cWaterLevel + sin(alpha) * cWaveAmplitude) *
		(TerrainRenderer->Scale->Z * (1.0 / 128));
}

// ---------------------------------------------------------------------------
void TForm1::IssuePoint(TGLHeightData *hd, int x, int y, int s2, float t, int rx,
	int ry) {
	const float r = 0.75;
	const float g = 0.75;
	const float b = 1.0;

	float px, py;
	float alpha, colorRatio, ca, sa;

	px = x + rx + s2;
	py = y + ry + s2;
	if (hd->DataState == hdsNone) {
		alpha = 1;
	}
	else {
		alpha = (cWaterLevel - hd->SmallIntHeight(rx, ry)) *
			(1 / cWaterOpaqueDepth);
		alpha = ClampValue(alpha, 0.5, 1.0);
	}
	SinCos(WaterPhase(px, py), sa, ca);
	colorRatio = 1 - alpha * 0.1;
	glColor4f(r*colorRatio, g*colorRatio, b, alpha);
	glTexCoord2f(px*0.01 + 0.002*sa, py*0.01 + 0.0022*ca - t*0.002);
	glVertex3f(px, py, cWaterLevel + cWaveAmplitude*sa);
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::TerrainRendererHeightDataPostRender
	(TGLRenderContextInfo &rci, TList *&HeightDatas) {
	int i, x, y, s, s2;
	float t;
	TGLHeightData *hd;

	if (WaterPlane) {
		t = GLCadencer->CurrentTime;
		MaterialLibrary->ApplyMaterial("water", rci);
		do {
			if (!WasAboveWater)
				rci.GLStates->InvertGLFrontFace();
			glPushAttrib(GL_ENABLE_BIT);

			glDisable(GL_LIGHTING);
			glDisable(GL_NORMALIZE);

			glStencilFunc(GL_ALWAYS, 1, 255);
			glStencilMask(255);
			glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);
			glEnable(GL_STENCIL_TEST);
			glNormal3f(0, 0, 1);

			for (i = 0; i < HeightDatas->Count; i++) {
				hd = (TGLHeightData*)(HeightDatas->List[i]);

				if ((hd->DataState == hdsReady) && (hd->HeightMin >
					cWaterLevel))
					continue;
				x = hd->XLeft;
				y = hd->YTop;
				s = hd->Size - 1;
				s2 = s / 2;
				glBegin(GL_TRIANGLE_FAN);

				IssuePoint(hd, x, y, s2, t, s2, s2);
				IssuePoint(hd, x, y, s2, t, 0, 0);
				IssuePoint(hd, x, y, s2, t, s2, 0);
				IssuePoint(hd, x, y, s2, t, s, 0);
				IssuePoint(hd, x, y, s2, t, s, s2);
				IssuePoint(hd, x, y, s2, t, s, s);
				IssuePoint(hd, x, y, s2, t, s2, s);
				IssuePoint(hd, x, y, s2, t, 0, s);
				IssuePoint(hd, x, y, s2, t, 0, s2);
				IssuePoint(hd, x, y, s2, t, 0, 0);

				glEnd();
			}
			glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
			glPopAttrib();
			if (!WasAboveWater)
				rci.GLStates->InvertGLFrontFace();
			WaterPolyCount = HeightDatas->Count * 8;
		}
		while (MaterialLibrary->UnApplyMaterial(rci));
	}
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::DOWakeProgress(TObject *Sender, const double deltaTime,
	const double newTime) {
	int i;
	TGLVector sbp, sbr;

	if (WakeVertices == NULL) {
		WakeVertices = new TAffineVectorList();
		WakeStretch = new TAffineVectorList();
		WakeTime = new TSingleList();
	}

	// enlarge current vertices
	i = 0;
	while (i < WakeVertices->Count) {
		WakeVertices->CombineItem(i, WakeStretch->Items[i >> 1],
			-0.45*deltaTime);
		WakeVertices->CombineItem(i + 1, WakeStretch->Items[i >> 1],
			0.45*deltaTime);
		i += 2;
	}
	// Progress wake
	if (newTime > DOWake->TagFloat) {
		if (DOWake->TagFloat == 0) {
			DOWake->TagFloat = newTime + 0.2;
		}
		else {
			DOWake->TagFloat = newTime + 1;
			sbp = VectorCombine(FFSailBoat->AbsolutePosition,
				FFSailBoat->AbsoluteDirection, 1, 3);
			sbr = FFSailBoat->AbsoluteRight();
			// add new
			WakeVertices->Add(VectorCombine(sbp, sbr, 1, -2));
			WakeVertices->Add(VectorCombine(sbp, sbr, 1, 2));
			WakeStretch->Add(VectorScale(sbr, (0.95 + Random()*0.1)));
			WakeTime->Add(newTime*0.1);
			if (WakeVertices->Count >= 80) {
				WakeVertices->Delete(0);
				WakeVertices->Delete(0);
				WakeStretch->Delete(0);
				WakeTime->Delete(0);
			}
		}
	}
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::DOWakeRender(TObject *Sender, TGLRenderContextInfo &rci) {
	int i, n;
	Gls::Vectortypes::TVector3f p;
	TGLVector sbp;
	float c;

	if (!(WakeVertices) && (!((FFSailBoat->Visible) || (WaterPlane)))) {
		MaterialLibrary->ApplyMaterial("wake", rci);
		do {
			glPushAttrib(GL_ENABLE_BIT);

			glDisable(GL_LIGHTING);
			glDisable(GL_FOG);

			glEnable(GL_BLEND);
			glBlendFunc(GL_ONE, GL_ONE);

			glStencilFunc(GL_EQUAL, 1, 255);
			glStencilMask(255);
			glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
			glEnable(GL_STENCIL_TEST);
			glDisable(GL_DEPTH_TEST);

			if (!WasAboveWater)
				rci.GLStates->InvertGLFrontFace();

			glBegin(GL_TRIANGLE_STRIP);
			n = WakeVertices->Count;
			for (i = 0; i < n; i++) {
				p = WakeVertices->Items[i ^ 1];
				sbp = TerrainRenderer->AbsoluteToLocal(VectorMake(p, 0));
				if ((i & 1) == 0) {
					c = (i & 0xFFE) * 0.2 / n;
					glColor3f(c, c, c);
					glTexCoord2f(0, WakeTime->Items[i / 2]);
				}
				else
					glTexCoord2f(1, WakeTime->Items[i / 2]);
				glVertex3f(p.X, WaterHeight(sbp.X, sbp.Y), p.Z);
			}
			glEnd();

			if (!WasAboveWater)
				rci.GLStates->InvertGLFrontFace();

			glPopAttrib();
			glDisable(stStencilTest);
		}
		while (MaterialLibrary->UnApplyMaterial(rci));
	}
}
// ---------------------------------------------------------------------------
