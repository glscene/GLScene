#ifndef fWarpingCH
#define fWarpingCH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Dialogs.hpp>
#include <ExtDlgs.hpp>
#include <Menus.hpp>

#include "GLS.Graph.hpp"
#include "GLS.Scene.hpp"
#include "GLS.SceneViewer.hpp"
#include "GLS.BaseClasses.hpp"
#include "GLS.Coordinates.hpp"

#include "GLS.FileJPEG.hpp"
#include "jpeg.hpp"

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TGLSceneViewer *GLSceneViewer;
        TMainMenu *MainMenu1;
        TMenuItem *MIFile;
        TMenuItem *MIOpenImageFile;
        TMenuItem *MISaveCurrentImage;
        TMenuItem *N1;
        TMenuItem *MIExit;
        TMenuItem *MIQuality;
        TMenuItem *N1toomuch1;
        TMenuItem *N4highquality1;
        TMenuItem *N8mediumquality1;
        TMenuItem *N16lowquality1;
        TMenuItem *MIQualityOption;
        TMenuItem *MIRadius;
        TMenuItem *N10small1;
        TMenuItem *N20medium1;
        TMenuItem *MIRadiusSetting;
        TMenuItem *N80extra1;
        TMenuItem *MIEffect;
        TMenuItem *MIZoomEffect;
        TMenuItem *MISpin;
        TOpenPictureDialog *OpenPictureDialog;
        TGLScene *GLScene;
        TGLHeightField *HeightField;
        TGLCamera *GLCamera;
        TSaveDialog *SaveDialog;
        void __fastcall HeightFieldGetHeight(const float x, const float y,
          float &z, TVector4f &color, TTexPoint &texPoint);
        void __fastcall MIOpenImageFileClick(TObject *Sender);
        void __fastcall MISaveCurrentImageClick(TObject *Sender);
        void __fastcall MIExitClick(TObject *Sender);
        void __fastcall GLSceneViewerMouseDown(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
        void __fastcall GLSceneViewerMouseMove(TObject *Sender,
          TShiftState Shift, int X, int Y);
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall MIQualityOptionClick(TObject *Sender);
        void __fastcall MIRadiusSettingClick(TObject *Sender);
		void __fastcall MIZoomEffectClick(TObject *Sender);
private:	// User declarations
		int warpX, warpY, warpRadius, warpEffect;
		TFileName PathToTexture;
public:		// User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
