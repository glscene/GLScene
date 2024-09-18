// ---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#include <System.Math.hpp>

#pragma hdrstop

#include "fTentaclesC.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Extrusion"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma resource "*.dfm"
TFormTentacles* FormTentacles;

// ---------------------------------------------------------------------------
__fastcall TFormTentacles::TFormTentacles(TComponent* Owner) : TForm(Owner) {}
// ---------------------------------------------------------------------------
const int cNbNodes = 32;

void __fastcall TFormTentacles::FormCreate(TObject* Sender)
{
	int i, k;
    TGLPipe* pipe;

    // prepare the TGLPipe objects (add node, set props...)
    for (k = 0; k < DCBase->Count - 1; k++) {
        if (dynamic_cast<TGLPipe*>(DCBase->Children[k])) {
			pipe = (TGLPipe*)(DCBase->Children[k]);
            pipe->Nodes->Clear();
            for (i = 0; i < cNbNodes - 1; i++)
                pipe->Nodes->AddNode(0, i / 8, 0);
            pipe->Radius = 0.1;
            // enable per-node coloring in the TGLPipe
            pipe->NodesColorMode = pncmDiffuse;
            // divisions between nodes (for spline interpolation)
            pipe->Division = 3;
            // No geometry compilation/cacheing, render directly
            // (geometry changes completely from frame to frame)
            pipe->ObjectStyle = pipe->ObjectStyle << osDirectDraw;
        }
    }
}

// ---------------------------------------------------------------------------
void __fastcall TFormTentacles::GLCadencer1Progress(
    TObject* Sender, const double deltaTime, const double newTime)
{
    int i, k;
    float t, t1, t2, r;
    TGLPipe* pipe;

    t = newTime;
    for (k = 0; k < DCBase->Count - 1; k++)
        if (dynamic_cast<TGLPipe*>(DCBase->Children[k])) {
            pipe = (TGLPipe*)(DCBase->Children[k]);
            pipe->Nodes->BeginUpdate();
            for (i = 0; i < pipe->Nodes->Count - 1; i++) {
              // don't search any hidden logic behind the formulaes below:
              // they're just here to induce this sickening weirdo movement

                t1 = -t + i * 0.1 + k * (2 * M_PI) / 5;
                r = (Sin(3 * t + k) + 2) * 0.5 * ((2 * i + pipe->Nodes->Count) / pipe->Nodes->Count);
                pipe->Nodes->Items[i]->X = Cos(t1) * r;
                pipe->Nodes->Items[i]->Z = Sin(t1) * r;
                t2 = 2 * (t + i / (pipe->Nodes->Count - 1) + k);
                pipe->Material->FrontProperties->Ambient->Color =
                    VectorLerp(clrAqua, clrYellow, Sin(t2));
                pipe->Radius = (1 + (Sin(t2) * 0.5)) * Ln((pipe->Nodes->Count - i)) * 0.5;
            }
            pipe->Nodes->EndUpdate();
        }
	Sphere1->Radius = 1.4 + Sin(2 * t) * 0.1;
}

// ---------------------------------------------------------------------------
void __fastcall TFormTentacles::Timer1Timer(TObject* Sender)
{
/*
	PanelFPS->Caption =
		Format("%.1f FPS", ARRAYOFCONST((GLSceneViewer1->FramesPerSecond())));
*/
	PanelFPS->Caption = GLSceneViewer1->FramesPerSecond();
    GLSceneViewer1->ResetPerformanceMonitor();
}
// ---------------------------------------------------------------------------


