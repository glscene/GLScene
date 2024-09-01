// ---------------------------------------------------------------------------

#include <system.hpp>
#include <vcl.h>
#include <math.h>
#include <tchar.h>
#pragma hdrstop

#include "fChrismasC.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BitmapFont"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.GeomObjects"
#pragma link "GLS.HUDObjects"
#pragma link "GLS.Material"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.ShadowPlane"
#pragma link "GLS.VectorFileObjects"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.WindowsFont"
#pragma link "GLS.ScreenSaver"
#pragma link "GLS.File3DS"
#pragma link "GLS.FileWAV"
#pragma link "GLS.ScreenSaver"

#pragma link "GLS.ThorFX"
#pragma link "GLS.FireFX"
#pragma link "GLS.Coordinates"
#pragma link "GLS.FileWAV"
#pragma link "GLS.LensFlare"
#pragma link "GLS.ParticleFX"
#pragma link "GLS.SoundManager"
#pragma link "GLS.Sounds.BASS"
#pragma resource "*.dfm"
TForm1* Form1;

// ---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner) : TForm(Owner) {}
// ---------------------------------------------------------------------------

void __fastcall TForm1::FormCreate(TObject* Sender)
{
	AssetPath = GetCurrentAssetPath();
	Randomize();

	SetCurrentDir(AssetPath + "\\model");
	ffFirTree->LoadFromFile("firtree.3ds");
	ffFirePlace->LoadFromFile("fireplace.3ds");
	fireLight = 0.5;
	ftYear->Text = "";

	// Set current dir for audio files
	SetCurrentDir(AssetPath + "\\audio");
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::ViewerMouseDown(
    TObject* Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
    mx = X;
    my = Y;
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::ViewerMouseMove(
    TObject* Sender, TShiftState Shift, int X, int Y)
{
    if (Shift.Contains(ssLeft)) {
        Camera->MoveAroundTarget(my - Y, mx - X);
        mx = X;
        my = Y;
    }
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::TimerTimer(TObject* Sender)
{
	int i;
	TDateTime t;
	String buf;
	Word y, m, d;
	bool TheChristmas;

	// if (miMerryCristmas->Checked)
	TheChristmas = false; // Merry Christmas or Happy New Year!
	Caption = Viewer->FramesPerSecond();
	Viewer->ResetPerformanceMonitor();

	if ((GLSMBASS->Active) && (bStream == 0)) {
		bStream = BASS_StreamCreateFile(false, PAnsiChar("Jingle_Bells_64.mp3"),
			0, 0, BASS_STREAM_AUTOFREE);
		BASS_ChannelPlay(bStream, false);
	}
	DecodeDate(Now(), y, m, d);
	t = EncodeDate(y, 12, 25) - Now(); // Merry Christmas
	// t = EncodeDate(y+1, 01, 01) - Now(); //Happy New Year!
	if (TheChristmas) {
		t = EncodeDate(y, 12, 25) - Now();
		ftCongratulations->Text = "Merry Christmas!";
    } else {
        t = EncodeDate(y + 1, 01, 01) - Now();
		ftCongratulations->Text = "Happy New Year!";
		ftYear->Text = IntToStr(y + 1);
	}
	if ((double)t < 0)
		ftCountDown->Text = "Merry Christmas!";

	// t = 0; // true Christmas or New Year event
	if (((double)t < 1) && ((double)t > -1))
		dcGifts->Visible = true;
    if ((double)t >= 2) {
        buf = IntToStr(Floor((double)t)) + " days, ";
        i = (Int)(Frac((double)t) * 24);
        if (i > 1)
            buf = buf + IntToStr(i) + " hours...";
        else
            buf = buf + IntToStr(i) + " hour...";
		ftCountDown->Text = buf;
    } else {
		t = (double)t * 24;
        if ((double)t > 1) {
			buf = IntToStr((int)t) + " hours, ";
            i = RoundInt(Frac((double)t) * 60);
            if (i > 1)
                buf = buf + IntToStr(i) + " minutes...";
            else
				buf = buf + IntToStr(i) + " minute...";
			ftCountDown->Text = buf;
		} else {
			t = (double)t * 60;
			i = RoundInt((double)t - Frac((double)t) * 60); // was  Floor(t) - ambiguous
			ftCountDown->Text =
				IntToStr((int)t) + " minutes, " + IntToStr(i) + " seconds...";
		}
	}
}

// ---------------------------------------------------------------------------

void __fastcall TForm1::CadencerProgress(
	TObject* Sender, const double deltaTime, const double newTime)
{
    fireLight = ClampValue(fireLight + Random() * 0.4 - 0.2, 0, 1);
    LSFire->Diffuse->Color =
        VectorLerp(clrYellow, VectorMake(0.5, 0, 0, 1), fireLight);
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
	dcFirTree->Turn(deltaTime);
	Viewer->Invalidate();
}

// ---------------------------------------------------------------------------

void __fastcall TForm1::FormResize(TObject* Sender)
{
    Camera->SceneScale = (float)Width / 640;
    if (Visible)
        HUDSprite->Position->X = Width - 200;
    if (Width >= Screen->Width)
        ViewerDblClick(Sender);
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::FormKeyPress(TObject* Sender, System::WideChar &Key)
{
    Key = '\0';
    Application->Terminate();
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::ViewerDblClick(TObject* Sender)
{
    if ((!inPreview) && (!inSaver) && (!Application->Terminated) &&
        (BorderStyle != bsNone))
    {
        BorderStyle = bsNone;
		FormStyle = fsStayOnTop;
        Align = alClient;
    }
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::ScreenSaverCloseQuery(TObject* Sender, bool &CanClose)

{
    Application->Terminate();
    CanClose = false;
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::ScreenSaverExecute(TObject* Sender)
{
    inSaver = true;
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::ScreenSaverPreview(TObject* Sender, HWND previewHwnd)
{
    inPreview = true;
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::FormMouseWheel(TObject* Sender, TShiftState Shift,
    int WheelDelta, TPoint &MousePos, bool &Handled)
{
    Camera->AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
}
// ---------------------------------------------------------------------------

