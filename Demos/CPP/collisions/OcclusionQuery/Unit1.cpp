//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGeomObjects"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormDestroy(TObject *Sender)
{
  // Delete the queries
  TimerQuery->Free();
  OcclusionQuery->Free();
  bOcclusionQuery->Free();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  // Move some of the scene objects around
  GLDummyCube1->Position->X = Sin(newTime);
  dcTestObjects->Turn(deltaTime * 50);
  dcTestObjects->Position->Z = 2 * Sin(newTime);
  GLDummyCube2->Position->X = - Sin(newTime);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1BeforeRender(TObject *Sender)
{
  // Occlusion queries are supported by extensions with lower version of OpenGL.
  // To use them, you'd need to check if GL_NV_occlusion_query or GL_ARB_occlusion_query
  // extensions are present, and makes the appropriate calls to the functions/procedures
  // they provide.
  // in Delphi was:   if (not TGLOcclusionQueryHandle.IsSupported) then
  if (!(GL_NV_occlusion_query || GL_ARB_occlusion_query))
  {
	MessageDlg("Requires hardware that supports occlusion queries to run",
	  mtError, TMsgDlgButtons() << mbOK, 0);
	Close();
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::OGLBeginQueriesRender(TObject *Sender, TGLRenderContextInfo &rci)

{
  // Generate the queries, if not already created
  if (!queriesCreated)
  {
	OcclusionQuery = new TGLOcclusionQueryHandle;
	bOcclusionQuery = new TGLBooleanOcclusionQueryHandle;
	TimerQuery = new TGLTimerQueryHandle;
	queriesCreated = true;
  }
  // Begin the timer + occlusion queries
  TimerQuery->BeginQuery();
  if (CheckBox1->Checked)
	bOcclusionQuery->BeginQuery();
  else
	OcclusionQuery->BeginQuery();

}
//---------------------------------------------------------------------------
void __fastcall TForm1::OGLEndQueriesRender(TObject *Sender, TGLRenderContextInfo &rci)

{
  TGLQueryHandle *lQuery;
  // End the timer + occlusion queries
  if (CheckBox1->Checked)
	lQuery = bOcclusionQuery;
  else
	lQuery = OcclusionQuery;
  lQuery->EndQuery();
  TimerQuery->EndQuery();

  // Most of the frame rate is lost waiting for results to become available
  //  + updating the captions every frame, but as this is a demo, we want to
  // see what is going on.

  while (! lQuery->IsResultAvailable()) ; // wait
  // would normally do something in this period before checking if
  // result is available

  samplesPassed = OcclusionQuery->PixelCount();

  while (! TimerQuery->IsResultAvailable()) ;// wait
	// would normally do something in this period before checking if
	// result is available
  timeTaken = TimerQuery->Time();
	// Use this line instead of the one above to use 64 bit timer, to allow
	// recording time periods more than a couple of seconds (requires Delphi 7+)
	// timeTaken := TimerQuery.QueryResultUInt64;

  switch (CheckBox1->Checked)
  {
  case true:
	  Label3->Visible = ! lQuery->QueryResultBool(); break;
  case false:
	{
	  Label3->Visible = (samplesPassed = 0);
	  Label2->Caption = "Number of test pixels visible: " + IntToStr(samplesPassed);
	  break;
	}
  default:
	  ;
  }


}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  // Convert time taken from ns => ms & display
  if (timerQuerySupported)
	Label1->Caption = "Time taken: " + FloatToStr(timeTaken / 1000000) + " ms" ;
  else
	Label1->Caption = "Time query unavailable, requires hardware support";

  LabelFPS->Caption = GLSceneViewer1->FramesPerSecondText(0);
  GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
