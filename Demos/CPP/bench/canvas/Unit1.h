//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Graphics.hpp>

#include "GLCoordinates.hpp"
#include "GLCrossPlatform.hpp"
#include "GLScene.hpp"
#include "GLWin32Viewer.hpp"
#include "GLCanvas.hpp"
#include "GLRenderContextInfo.hpp"
#include "GLBitmapFont.hpp"
#include "GLWindowsFont.hpp"
#include "GLBaseClasses.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TBevel *Bevel1;
	TGLSceneViewer *GLSceneViewer;
	TGLScene *GLScene1;
	TGLDirectOpenGL *GLDirectOpenGL1;
	TGLCamera *GLCamera1;
	TGLWindowsBitmapFont *WindowsBitmapFont;
	TPanel *Panel1;
	TButton *BULines;
	TButton *BUEllipses;
	TButton *BURects;
	TButton *BUArc;
	TButton *BUPoints;
	TButton *BUTextOut;
	TRadioButton *RBPenWidth1;
	TRadioButton *RBPenWidth2;
	TLabel *LAGLCanvas;
	TLabel *LAGDI;
	TPaintBox *PaintBox;
	void __fastcall BULinesClick(TObject *Sender);
	void __fastcall BUEllipsesClick(TObject *Sender);
	void __fastcall BUArcClick(TObject *Sender);
	void __fastcall BURectsClick(TObject *Sender);
	void __fastcall BUPointsClick(TObject *Sender);
	void __fastcall BUTextOutClick(TObject *Sender);
	void __fastcall GLDirectOpenGL1Render(TObject *Sender, TGLRenderContextInfo &rci);

private:	// User declarations
	void PaintTheBox();
	void Bench();

public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
