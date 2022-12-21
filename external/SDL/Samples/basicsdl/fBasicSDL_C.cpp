//---------------------------------------------------------------------------

#pragma hdrstop

#include "fBasicSDL_C.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma classgroup "Vcl.Controls.TControl"
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Coordinates"
#pragma link "GLS.GeomObjects"
#pragma link "GLS.Scene"
#pragma link "GLS.SDLContext"

#pragma resource "*.dfm"
TDataModule2* DataModule2;
//---------------------------------------------------------------------------
__fastcall TDataModule2::TDataModule2(TComponent* Owner) : TDataModule(Owner) {}
//---------------------------------------------------------------------------
void __fastcall TDataModule2::DataModuleCreate(TObject* Sender)
{
	// When using SDL, the standard VCL message queue is no longer operational,
	// so you must have/make your own loop to prevent the application from
	// terminating immediately
	GLSDLViewer1->Render();
	while (GLSDLViewer1->Active()) {
		// Message queue is not operational, but there may still be some messages
		Forms::Application->ProcessMessages();
		// Relinquish some of that CPU time
		SDL_Delay(1);
		// Slowly rotate the teapot
		Teapot1->RollAngle = 4 * Frac(24 * double(Now())) * 3600;
	}
}
//---------------------------------------------------------------------------
void __fastcall TDataModule2::GLSDLViewer1EventPollDone(TObject* Sender)
{
	TFileName Path = GetCurrentAssetPath();
	TFileName PathCM = GetCurrentDir() + "\\Cubemaps";
    SetCurrentDir(PathCM);

	if (!firstPassDone) {
        // Loads a texture map for the teapot
        // (see materials/cubemap for details on that)
        //
        // The odd bit is that it must be done upon first render, otherwise
        // SDL OpenGL support has not been initialized and things like checking
        // an extension support (cube maps here) would fail...
        // Something less clunky will be introduced, someday...
        firstPassDone = true;
        GLSDLViewer1->Buffer->RenderingContext->Activate();
        try {
            if (!GL_ARB_texture_cube_map)
                ShowMessage("Your graphics board does not support cube maps");
            else {
                TGLTexture* tex = Teapot1->Material->Texture;
                tex->ImageClassName = __classid(TGLCubeMapImage)->ClassName();
                TGLCubeMapImage* img = (TGLCubeMapImage*)tex->Image;

                img->Picture[CmtPX]->LoadFromFile("cm_left.jpg");
                img->Picture[CmtNX]->LoadFromFile("cm_right.jpg");
                img->Picture[CmtPY]->LoadFromFile("cm_top.jpg");
                img->Picture[CmtNY]->LoadFromFile("cm_bottom.jpg");
                img->Picture[CmtPZ]->LoadFromFile("cm_back.jpg");
                img->Picture[CmtNZ]->LoadFromFile("cm_front.jpg");

                Teapot1->Material->Texture->MappingMode = tmmCubeMapReflection;
                Teapot1->Material->Texture->Disabled = false;
            }
        } __finally
        {
            GLSDLViewer1->Buffer->RenderingContext->Deactivate();
        }
    }
    GLSDLViewer1->Render();
}
//---------------------------------------------------------------------------
void __fastcall TDataModule2::GLSDLViewer1Resize(TObject* Sender)
{
    // Zoom if SDL window gets smaller/bigger
    GLCamera1->SceneScale = GLSDLViewer1->Width / 160;
}
//---------------------------------------------------------------------------

