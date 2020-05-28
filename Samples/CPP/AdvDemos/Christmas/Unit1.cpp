// ---------------------------------------------------------------------------

#include <system.hpp>
#include <vcl.h>
#include <math.h>
#include <tchar.h>
#pragma hdrstop

#include "Unit1.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLBitmapFont"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGeomObjects"
#pragma link "GLHUDObjects"
#pragma link "GLLensFlare"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLParticleFX"
#pragma link "GLScene"
#pragma link "GLShadowPlane"
#pragma link "GLSound"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLWindowsFont"
#pragma link "GLScreenSaver"
#pragma link "GLSMBASS"
#pragma link "GLFile3DS"
#pragma link "GLFileWAV"
#pragma link "GLScreenSaver"

#pragma link "GLFileMP3"
#pragma link "GLThorFX"
#pragma link "GLFireFX"
#pragma resource "*.dfm"
TForm1 *Form1;

// ---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner) : TForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender) {
	String DataPath;
	Randomize();
	DataPath = ExtractFilePath(ParamStr(0)) + "data";
	SetCurrentDir(DataPath);
	FFFirTree->LoadFromFile("firtree.3ds");
	FFFirePlace->LoadFromFile("fireplace.3ds");
	fireLight = 0.5;
	FTYear->Text = "";
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::ViewerMouseDown(TObject *Sender, TMouseButton Button,
	TShiftState Shift, int X, int Y) {
	mx = X;
	my = Y;
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::ViewerMouseMove(TObject *Sender, TShiftState Shift,
	int X, int Y) {
	if (Shift.Contains(ssLeft)) {
		Camera->MoveAroundTarget(my - Y, mx - X);
		mx = X;
		my = Y;
	}
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::TimerTimer(TObject *Sender) {
	int i;
	TDateTime t;
	String buf;
	Word y, m, d;

	Caption = Format("%.1f FPS", ARRAYOFCONST((Viewer->FramesPerSecond())));
	Viewer->ResetPerformanceMonitor();

	if ((GLSMBASS->Active) && (bStream == 0)) {
		bStream = BASS_StreamCreateFile(false, PAnsiChar("Jingle_Bells_64.mp3"),
			0, 0, BASS_STREAM_AUTOFREE);
		BASS_ChannelPlay(bStream, false);
	}
	DecodeDate(Now(), y, m, d);
	///   t = EncodeDate(y, 12, 25) - Now(); //Merry Christmas
	///t = EncodeDate(y+1, 01, 01) - Now(); //Happy New Year!
	if (miMerryCristmas->Checked) {
		t = EncodeDate(y, 12, 25) - Now();
		FTCongratulations->Text = "Merry Christmas!";
	}
	else {
		t = EncodeDate(y + 1, 01, 01) - Now();
		FTCongratulations->Text  = "Happy New Year!";
		FTYear->Text  = IntToStr(y + 1);
	}

	if ((double)t < 0)
		FTCountDown->Text = "Merry Christmas!";
	if (((double)t < 1) && ((double)t > -1))
		DCGifts->Visible = true;
	if ((double)t >= 2) {
		buf = IntToStr(Floor((double)t)) + " days, ";
		i = (Int)(Frac((double)t) * 24);
		if (i > 1)
			buf = buf + IntToStr(i) + " hours...";
		else
			buf = buf + IntToStr(i) + " hour...";
		FTCountDown->Text = buf;
	}
	else {
		t = (double)t * 24;
		if ((double)t > 1) {
			buf = IntToStr((int)t) + " hours, ";
			i = RoundInt(Frac((double)t) * 60);
			if (i > 1)
				buf = buf + IntToStr(i) + " minutes...";
			else
				buf = buf + IntToStr(i) + " minute...";
			FTCountDown->Text = buf;
		}
		else {
			t = (double)t * 60;
			i = RoundInt(((double)t - Floor(t)) * 60);
			FTCountDown->Text = IntToStr((int)t) + " minutes, " +
				IntToStr(i) + " seconds...";
		}
	}
}

// ---------------------------------------------------------------------------

void __fastcall TForm1::CadencerProgress(TObject *Sender,
	const double deltaTime, const double newTime) {
	fireLight = ClampValue(fireLight + Random() * 0.4 - 0.2, 0, 1);
	LSFire->Diffuse->Color = VectorLerp(clrYellow, VectorMake(0.5, 0, 0, 1),
		fireLight);
	LSFire->Position->Y = fireLight * 0.1;

	if (inPreview)
		HUDSprite->Visible = false;
	if (Visible) {
		HUDSprite->Material->FrontProperties->Diffuse->Alpha =
			HUDSprite->Material->FrontProperties->Diffuse->Alpha -
			deltaTime * 0.05;
		if (HUDSprite->Material->FrontProperties->Diffuse->Alpha < 0.01)
			HUDSprite->Visible = false;
	}
	DCFirTree->Turn(deltaTime);
	Viewer->Invalidate();
}

// ---------------------------------------------------------------------------

void __fastcall TForm1::FormResize(TObject *Sender) {
	Camera->SceneScale = (float)Width / 640;
	if (Visible)
		HUDSprite->Position->X = Width - 200;
	if (Width >= Screen->Width)
		ViewerDblClick(Sender);
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::miMerryCristmasClick(TObject *Sender) {
	miMerryCristmas->Checked = True;
	miHappyNewYear->Checked = False;
	FTYear->Text = "";
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::miHappyNewYearClick(TObject *Sender) {
	miHappyNewYear->Checked = True;
	miMerryCristmas->Checked = False;
}

// ---------------------------------------------------------------------------

void __fastcall TForm1::FormKeyPress(TObject *Sender, System::WideChar &Key) {
	Key = '\0';
	Application->Terminate();
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::ViewerDblClick(TObject *Sender) {
	if ((!inPreview) && (!inSaver) && (!Application->Terminated) &&
		(BorderStyle != bsNone)) {
		BorderStyle = bsNone;
		FormStyle = fsStayOnTop;
		Align = alClient;
	}
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::ScreenSaverCloseQuery(TObject *Sender, bool &CanClose)

{
	Application->Terminate();
	CanClose = false;
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::ScreenSaverExecute(TObject *Sender) {
	inSaver = true;
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::ScreenSaverPreview(TObject *Sender, HWND previewHwnd) {
	inPreview = true;
}
// ---------------------------------------------------------------------------
void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
 Camera->AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
}
//---------------------------------------------------------------------------

