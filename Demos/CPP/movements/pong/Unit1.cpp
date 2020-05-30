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
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLShadowVolume"
#pragma link "GLSpaceText"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ResetGame()
{
	float angle;
	// places the ball in the mat center, resets score and ball speed
	angle = DegToRad((float)(45+Random(90)));
	MakeVector(ballVector, 4*cos(angle), 4*sin(angle), 0);
	score = 0;
	gameOver = false;
	Ball->Position->AsVector = NullHmgPoint;
}

void __fastcall TForm1::FormCreate(TObject *Sender)
{
	Randomize();
	GLSceneViewer1->Cursor = crNone;
	ResetGame();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
const float
	cPadMinMax = 6.25;
	float px;

	// the pad's position is directly calculated from the mouse position
	px = (X-(GLSceneViewer1->Width/2))*0.035;
	if (px<-cPadMinMax)
		px = -cPadMinMax;
	else
	if (px>cPadMinMax)
		px = cPadMinMax;
	Pad->Position->X = px;
//   GLCadencer1.Reset;
   // update the whole stuff now!
   GLCadencer1->Progress();

}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
	Glvectorgeometry::TVector newBallPos;
	// gameOver is True as soon as the ball is behind the pad, but we don't end
	// the game immediately so the user can realize he has lost
	if ((!gameOver) && (deltaTime>0))
	{
		// calc expected new ball pos (if no bump occurs)
		// ( note : VectorCombine(v1, v2, f1, f2)=v1*f1+v2*f2 )
		newBallPos = VectorCombine(Ball->Position->AsVector, ballVector, 1, deltaTime);
		// check collision with edges
		if (newBallPos.X<-7.05)
			ballVector.X = -ballVector.X;
		else
		if (newBallPos.X>7.05)
			ballVector.X = -ballVector.X;
		else
		if (newBallPos.Y>4.55)
			ballVector.Y = -ballVector.Y;
		// check collision with pad
		if (newBallPos.Y<-4)
		{
			if ((newBallPos.X>Pad->Position->X-1.25) && (newBallPos.X<Pad->Position->X+1.25))
			{
				// when ball bumps the pad, it is accelerated and the vector
				// is slightly randomized
				ballVector.Y = -ballVector.Y;
				ballVector.X = ballVector.X+(Random(100)-50)/50;
				ballVector.Y = ballVector.Y+0.1;
				// ...and of course a point is scored !
				score++;
				SpaceText1->Text = Format("%.3d", ARRAYOFCONST((score)));
			}
			else
			{
			  // ball missed !
			  gameOver = true;
			  exit;
			}
		}
	}
	// move the ball
	Ball->Position->AsVector = VectorCombine(Ball->Position->AsVector, ballVector, 1, deltaTime);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
	// update performance monitor
  //%s : Name,
	Caption = Format("%.2f FPS", ARRAYOFCONST((GLSceneViewer1->FramesPerSecond())));
	GLSceneViewer1->ResetPerformanceMonitor();
	// display score window when game is over and the ball is well out of the board
	if (gameOver && (Ball->Position->Y<-6))
	{
		// stop the timer to avoid stacking up Timer events
		// while the user makes up his mind...
		Timer1->Enabled = false;
		if (MessageDlg("Score : "+IntToStr(score)+ " Play again ?",
			   mtInformation, TMsgDlgButtons() << mbYes, 0)==mrYes) //esc - mbNo ?
		{
		  ResetGame();
		  Timer1->Enabled = true;
		}
		else Close();
	}
}
//---------------------------------------------------------------------------
