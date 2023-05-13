//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "fCanvasC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

#pragma link "GLS.BitmapFont"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.WindowsFont"

#pragma resource "*.dfm"
TFormCanvas *FormCanvas;

enum
  TWhat {wLines=0, wEllipses=1, wRects, wPoints, wTextOut, wArcs};

  TWhat vWhat;
  int vPenWidth;

const int
   cNbLines = 20000;
const int
   cNbEllipses = 20000;
const int
   cNbRects = 5000;
const int
   cNbPoints = 200000;
const int
   cNbTextOuts = 20000;
const int
   cNbArcs     = 20000;


//---------------------------------------------------------------------------
__fastcall TFormCanvas::TFormCanvas(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormCanvas::BULinesClick(TObject *Sender)
{
   vWhat = wLines;
   Bench();
}

void __fastcall TFormCanvas::BUEllipsesClick(TObject *Sender)
{
   vWhat = wEllipses;
   Bench();
}

//---------------------------------------------------------------------------
void __fastcall TFormCanvas::BUArcClick(TObject *Sender)
{
   vWhat = wArcs;
   Bench();
}

//---------------------------------------------------------------------------
void __fastcall TFormCanvas::BURectsClick(TObject *Sender)
{
   vWhat = wRects;
   Bench();
}

//---------------------------------------------------------------------------
void __fastcall TFormCanvas::BUPointsClick(TObject *Sender)
{
   vWhat = wPoints;
   Bench();
}
//---------------------------------------------------------------------------
void __fastcall TFormCanvas::BUTextOutClick(TObject *Sender)
{
   vWhat = wTextOut;
   Bench();
}

//---------------------------------------------------------------------------
void TFormCanvas::Bench()
{
   __int64 t; // TVarRec t;
   if (RBPenWidth1->Checked)
	  vPenWidth =1;
   else vPenWidth =2;

   Application->ProcessMessages();
   RandSeed = 0;

   t = StartPrecisionTimer();
   GLSceneViewer->Refresh();
///   LAGLCanvas->Caption = Format("GLCanvas: %.2f msec", ARRAYOFCONST((StopPrecisionTimer(t)*1000)));


   Application->ProcessMessages();
   RandSeed = 0;

   t = StartPrecisionTimer();
   PaintTheBox();
///   LAGDI->Caption = Format("GDI: %.1f msec",  ARRAYOFCONST((StopPrecisionTimer(t)*1000)));
}

//---------------------------------------------------------------------------

void __fastcall TFormCanvas::GLDirectOpenGL1Render(TObject *Sender, TGLRenderContextInfo &rci)

{
   int i, x, y;
   TGLCanvas *glc;
   TRect r;
   TColor Color;

   glc = new TGLCanvas(256, 256);
   glc->PenWidth = vPenWidth;

   switch (vWhat) {
	 case wLines :
			for (i=1; i< cNbLines; i++) {
			   glc->PenColor = clRed; // Random(256*256*256);
			   glc->MoveTo(random(256), random(256));
			   glc->LineTo(Random(256), Random(256));
			}
	 case wEllipses :
			for (i =1; i<cNbEllipses; i++){
			   glc->PenColor = clGreen; // Random(256*256*256);
			   glc->EllipseBB(Random(256), Random(256),
					   Random(256), Random(256));
			}
	 case wRects :{
			for (i =1; i< cNbRects; i++) {
			   glc->PenColor = clBlue; // Random(256*256*256);
			   r = Rect(Random(256), Random(256),
					   Random(256), Random(256));
			   glc->FillRect((int)r.left, (int)r.top, (int)r.right, (int)r.bottom);
			}
		 }
	 case wPoints : for (i=1; i<cNbPoints; i++) {
			 glc->PenColor = clGray; // Random(256*256*256);
			 glc->PlotPixel(Random(256), Random(256));
			 }
	 case wTextOut : {
			for (i=1; i<cNbTextOuts; i++) {
			   Color = clMaroon; // Random(256*256*256);
			   x = Random(256);
			   y = Random(256);
//	   WindowsBitmapFont->TextOut(rci, x, y, "Hello", Color);

			}
		 }
	  case wArcs :
		for (i = 1; i< cNbEllipses; i++) {
		   glc->PenColor = clCream; // Random(256*256*256);
		   glc->Arc(
				   Random(256), Random(256),
				   Random(256), Random(256),
				   Random(256), Random(256),
				   Random(256), Random(256));
		 }
  }
  delete glc;
}
//---------------------------------------------------------------------------
void TFormCanvas::PaintTheBox()
{
   int i, x, y;
   TRect r;
   Vcl::Graphics::TBitmap *b;
   //to be fair, use offscreen painting...
   b = new TBitmap;
   b->Width = 256;
   b->Height = 256;
   b->Canvas->Brush->Style = bsClear;
   b->Canvas->Pen->Width = vPenWidth;
	  switch (vWhat) {
		case wLines :	for (i=1; i< cNbLines; i++) {
			   b->Canvas->Pen->Color = clRed; // Random(256*256*256);
			   b->Canvas->MoveTo(Random(256), Random(256));
			   b->Canvas->LineTo(Random(256), Random(256));
			}
		case wEllipses :
			for (i =1; i<cNbEllipses; i++){
			   b->Canvas->Pen->Color = clGreen; // Random(256*256*256);
			   b->Canvas->Ellipse(Random(256), Random(256),
					   Random(256), Random(256));
			}
		case wRects : {
			b->Canvas->Brush->Style = bsSolid;
			for (i =1; i< cNbRects; i++) {
			   b->Canvas->Brush->Color = clBlue; // Random(256*256*256);
			   r = Rect(Random(256), Random(256),
					   Random(256), Random(256));
			   b->Canvas->FillRect(r);
			}
		 }
		 case wPoints : for (i=1; i<cNbPoints; i++)
			   b->Canvas->Pixels[Random(256)][Random(256)] = clGray; // Random(256*256*256);

		 case wTextOut : {
			Font = WindowsBitmapFont->Font;
			for (i=1; i<cNbTextOuts; i++) {
			   Font->Color = clMaroon; // Random(256*256*256);
			   x = Random(256);
			   y = Random(256);
			   b->Canvas->TextOutW(x, y, "Hello");
			}
		 }
		 case wArcs :
			for (i = 1; i< cNbEllipses; i++) {
			   b->Canvas->Pen->Color = clCream; // Random(256*256*256);
			   b->Canvas->Arc(
				   Random(256), Random(256),
				   Random(256), Random(256),
				   Random(256), Random(256),
				   Random(256), Random(256));
		 }
	  default: ;
   }
   b->Canvas->Draw(0, 0, b);
   delete b;
}

//---------------------------------------------------------------------------

