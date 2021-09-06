// ---------------------------------------------------------------------------

#ifndef fProcCloudsCH
#define fProcCloudsCH
// ---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "GLS.Texture.hpp"
#include "GLS.BaseClasses.hpp"
#include "GLS.Cadencer.hpp"
#include "GLS.Coordinates.hpp"
#include "GLS.Objects.hpp"
#include "GLS.Scene.hpp"
#include "GLS.ProcTextures.hpp"
#include "GLS.SceneViewer.hpp"
#include <Vcl.Buttons.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Samples.Spin.hpp>

// ---------------------------------------------------------------------------
class TFormClouds : public TForm {
__published: // IDE-managed Components
	TGLSceneViewer *GLSceneViewer1;
	TPanel *Panel1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label5;
	TLabel *LAUsedMemory;
	TLabel *LARGB32;
	TLabel *LACompression;
	TLabel *Label4;
	TLabel *Label6;
	TSpeedButton *CloudFileOpenBtn;
	TSpeedButton *MakeAndSaveCloudNoiseFile;
	TLabel *Label61;
	TLabel *LabelFPS;
	TComboBox *CBFormat;
	TComboBox *CBCompression;
	TRadioButton *RBDefault;
	TRadioButton *RBDouble;
	TRadioButton *RBQuad;
	TCheckBox *CheckBox1;
	TSpinEdit *SpinEdit1;
	TSpinEdit *SpinEdit2;
	TCheckBox *CheckBox2;
	TTrackBar *TrackBar1;
	TEdit *CloudRandomSeedUsedEdit;
	TEdit *CloudImageSizeUsedEdit;
	TCheckBox *UseCloudFileCB;
	TEdit *CloudFileUsedEdit;
	TGLScene *GLScene1;
	TGLPlane *Plane;
	TGLCamera *Camera;
	TGLCadencer *GLCadencer1;
	TTimer *Timer1;
	TOpenDialog *OpenDialog1;
	TSaveDialog *SaveDialog1;

	void __fastcall FormCreate(TObject *Sender);

public: // User declarations
	__fastcall TFormClouds(TComponent* Owner);
};

// ---------------------------------------------------------------------------
extern PACKAGE TFormClouds *FormClouds;
// ---------------------------------------------------------------------------
#endif
