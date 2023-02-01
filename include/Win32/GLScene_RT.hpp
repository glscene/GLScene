// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLScene_RT.dpk' rev: 35.00 (Windows)

#ifndef Glscene_rtHPP
#define Glscene_rtHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// (rtl)
#include <SysInit.hpp>
#include <Formats.m3DSConst.hpp>
#include <Formats.m3DSTypes.hpp>
#include <Formats.m3DSUtils.hpp>
#include <Formats.DDSImage.hpp>
#include <Formats.DXTC.hpp>
#include <Formats.m3DS.hpp>
#include <Formats.HDRImage.hpp>
#include <Formats.Q3BSP.hpp>
#include <Formats.B3D.hpp>
#include <Formats.GL2.hpp>
#include <Formats.LWO.hpp>
#include <Formats.MD2.hpp>
#include <Formats.MD3.hpp>
#include <Formats.OCT.hpp>
#include <Formats.TGA.hpp>
#include <Formats.X.hpp>
#include <Formats.VFW.hpp>
#include <Formats.VRML.hpp>
#include <GLS.AVIRecorder.hpp>
#include <GLS.AnimatedSprite.hpp>
#include <GLS.AnimationUtils.hpp>
#include <GLS.ApplicationFileIO.hpp>
#include <GLS.AsyncHDS.hpp>
#include <GLS.AsyncTimer.hpp>
#include <GLS.Atmosphere.hpp>
#include <GLS.ArchiveManager.hpp>
#include <GLS.BaseClasses.hpp>
#include <GLS.BaseMeshSilhouette.hpp>
#include <GLS.Behaviours.hpp>
#include <GLS.BitmapFont.hpp>
#include <GLS.Blur.hpp>
#include <GLS.BumpmapHDS.hpp>
#include <GLS.Cadencer.hpp>
#include <GLS.CameraController.hpp>
#include <GLS.Canvas.hpp>
#include <GLS.Collision.hpp>
#include <GLS.Color.hpp>
#include <GLS.CompositeImage.hpp>
#include <GLS.Console.hpp>
#include <GLS.Context.hpp>
#include <GLS.Coordinates.hpp>
#include <GLS.CurvesAndSurfaces.hpp>
#include <GLS.DCE.hpp>
#include <GLS.DynamicTexture.hpp>
#include <GLS.EParticleMasksManager.hpp>
#include <GLS.EllipseCollision.hpp>
#include <GLS.ExplosionFx.hpp>
#include <GLS.Extrusion.hpp>
#include <GLS.FBORenderer.hpp>
#include <GLS.FPSMovement.hpp>
#include <GLS.Feedback.hpp>
#include <GLS.File3DS.hpp>
#include <GLS.File3DSSceneObjects.hpp>
#include <GLS.File3DPDF.hpp>
#include <GLS.FileASE.hpp>
#include <GLS.FileB3D.hpp>
#include <GLS.FileBMP.hpp>
#include <GLS.FileDDS.hpp>
#include <GLS.FileDXF.hpp>
#include <GLS.FileGL2.hpp>
#include <GLS.FileGLTF.hpp>
#include <GLS.FileGRD.hpp>
#include <GLS.FileGTS.hpp>
#include <GLS.FileHDR.hpp>
#include <GLS.FileJPEG.hpp>
#include <GLS.FileLMTS.hpp>
#include <GLS.FileLWO.hpp>
#include <GLS.FileMD2.hpp>
#include <GLS.FileMD3.hpp>
#include <GLS.FileMD5.hpp>
#include <GLS.FileMDC.hpp>
#include <GLS.FileMP3.hpp>
#include <GLS.FileMS3D.hpp>
#include <GLS.FileNMF.hpp>
#include <GLS.FileNurbs.hpp>
#include <GLS.FileO3TC.hpp>
#include <GLS.FileO3TCImage.hpp>
#include <GLS.FileOBJ.hpp>
#include <GLS.FileOCT.hpp>
#include <GLS.FilePAK.hpp>
#include <GLS.FilePLY.hpp>
#include <GLS.FilePNG.hpp>
#include <GLS.FileQ3BSP.hpp>
#include <GLS.FileQ3MD3.hpp>
#include <GLS.FileSMD.hpp>
#include <GLS.FileSTL.hpp>
#include <GLS.FileTGA.hpp>
#include <GLS.FileTIN.hpp>
#include <GLS.FileVfsPAK.hpp>
#include <GLS.FileVRML.hpp>
#include <GLS.FileWAV.hpp>
#include <GLS.FileX.hpp>
#include <GLS.FireFX.hpp>
#include <GLS.FileZLIB.hpp>
#include <GLS.FileDEL.hpp>
#include <GLS.FileVOR.hpp>
#include <GLS.FullScreenViewer.hpp>
#include <GLS.Keyboard.hpp>
#include <GLS.GameMenu.hpp>
#include <GLS.GeomObjects.hpp>
#include <GLS.GeometryBB.hpp>
#include <GLS.Generics.hpp>
#include <GLS.Gizmo.hpp>
#include <GLS.GizmoEx.hpp>
#include <GLS.Graph.hpp>
#include <GLS.Graphics.hpp>
#include <GLS.Gui.hpp>
#include <GLS.HUDObjects.hpp>
#include <GLS.HeightData.hpp>
#include <GLS.HeightTileFileHDS.hpp>
#include <GLS.ImageUtils.hpp>
#include <GLS.Imposter.hpp>
#include <GLS.Isolines.hpp>
#include <GLS.Isosurface.hpp>
#include <GLS.Joystick.hpp>
#include <GLS.LensFlare.hpp>
#include <GLS.LinePFX.hpp>
#include <GLS.Logger.hpp>
#include <GLS.Manager.hpp>
#include <GLS.Material.hpp>
#include <GLS.MaterialEx.hpp>
#include <GLS.MaterialMultiProxy.hpp>
#include <GLS.MaterialScript.hpp>
#include <GLS.Mesh.hpp>
#include <GLS.MeshBSP.hpp>
#include <GLS.MeshBuilder.hpp>
#include <GLS.MeshCSG.hpp>
#include <GLS.MeshLines.hpp>
#include <GLS.MeshUtils.hpp>
#include <GLS.Mirror.hpp>
#include <GLS.Movement.hpp>
#include <GLS.ModuleLoader.hpp>	// (<weak>)
#include <GLS.MultiPolygon.hpp>
#include <GLS.MultiProxy.hpp>
#include <GLS.MultiSampleImage.hpp>
#include <GLS.Navigator.hpp>
#include <GLS.Nodes.hpp>
#include <GLS.ObjectManager.hpp>
#include <GLS.Objects.hpp>
#include <GLS.Octree.hpp>
#include <GLS.ParametricSurfaces.hpp>
#include <GLS.ParticleFX.hpp>
#include <GLS.Particles.hpp>
#include <GLS.Perlin.hpp>
#include <GLS.PerlinPFX.hpp>
#include <GLS.PolygonTesselation.hpp>
#include <GLS.Portal.hpp>
#include <GLSL.PostEffects.hpp>
#include <GLS.ProcTextures.hpp>
#include <GLS.ProxyObjects.hpp>
#include <GLS.Ragdoll.hpp>
#include <GLS.RandomHDS.hpp>
#include <GLS.RGBE.hpp>
#include <GLS.RenderContextInfo.hpp>
#include <GLS.ROAMPatch.hpp>
#include <GLSL.AsmShader.hpp>
#include <GLSL.BumpShaders.hpp>
#include <GLSL.DiffuseSpecularShader.hpp>
#include <GLSL.CustomShader.hpp>
#include <GLSL.LineShaders.hpp>
#include <GLSL.MultiMaterialShader.hpp>
#include <GLSL.PostShaders.hpp>
#include <GLSL.ProjectedTextures.hpp>
#include <GLSL.PhongShader.hpp>
#include <GLSL.ShaderCombiner.hpp>
#include <GLSL.TextureShaders.hpp>
#include <GLSL.UserShader.hpp>
#include <GLSL.ShapeShaders.hpp>
#include <GLSL.ShaderParameter.hpp>
#include <GLSL.Shader.hpp>
#include <GLS.Language.hpp>
#include <GLS.Memo.hpp>
#include <GLS.ProjectedTextures.hpp>
#include <GLS.Scene.hpp>
#include <GLS.SceneForm.hpp>
#include <GLS.Screen.hpp>
#include <GLS.ScreenSaver.hpp>
#include <GLS.Selection.hpp>
#include <GLS.ShadowHDS.hpp>
#include <GLS.ShadowPlane.hpp>
#include <GLS.ShadowVolume.hpp>
#include <GLS.SimpleNavigation.hpp>
#include <GLS.SkyDome.hpp>
#include <GLS.SmoothNavigator.hpp>
#include <GLS.SoundManager.hpp>
#include <GLS.SoundFileObjects.hpp>
#include <GLS.SpacePartition.hpp>
#include <GLS.State.hpp>
#include <GLS.SpaceText.hpp>
#include <GLS.SceneViewer.hpp>
#include <GLS.SmartObjects.hpp>
#include <GLS.TerrainRenderer.hpp>
#include <GLS.TexLensFlare.hpp>
#include <GLS.Texture.hpp>
#include <GLS.TextureCombiners.hpp>
#include <GLS.TextureFormat.hpp>
#include <GLS.TextureImageEditors.hpp>
#include <GLS.TexturedHDS.hpp>
#include <GLS.ThorFX.hpp>
#include <GLS.TilePlane.hpp>
#include <GLS.TimeEventsMgr.hpp>
#include <GLS.Trail.hpp>
#include <GLS.Tree.hpp>
#include <GLS.Triangulation.hpp>
#include <GLS.Utils.hpp>
#include <GLS.VectorFileObjects.hpp>
#include <GLS.VerletTypes.hpp>
#include <GLS.VerletClothify.hpp>
#include <GLS.WaterPlane.hpp>
#include <GLS.Windows.hpp>
#include <GLS.WindowsContext.hpp>
#include <GLS.WindowsFont.hpp>
#include <GLS.zBuffer.hpp>
#include <GLS.OpenGLAdapter.hpp>
#include <GLS.OpenGLTokens.hpp>
#include <GLS.PersistentClasses.hpp>
#include <GLS.PipelineTransformation.hpp>
#include <GLS.Polynomials.hpp>
#include <GLS.CrossXML.hpp>
#include <GLS.Silhouette.hpp>
#include <GLS.PlugInManager.hpp>
#include <GLS.VectorTypesExt.hpp>
#include <GLS.VectorGeometry.hpp>
#include <GLS.VectorLists.hpp>
#include <GLS.VectorTypes.hpp>
#include <GLS.ScriptBase.hpp>
#include <GLS.Spline.hpp>
#include <GLS.Strings.hpp>
#include <GLS.XCollection.hpp>
#include <GLS.XOpenGL.hpp>
#include <OpenGL.InitVCL.hpp>
#include <PasGLTF.hpp>
#include <PasJSON.hpp>
#include <PasDblStrUtils.hpp>
#include <System.UITypes.hpp>	// (rtl)
#include <Winapi.Windows.hpp>	// (rtl)
#include <Winapi.PsAPI.hpp>	// (rtl)
#include <System.Character.hpp>	// (rtl)
#include <System.Internal.ExcUtils.hpp>	// (rtl)
#include <System.SysUtils.hpp>	// (rtl)
#include <System.VarUtils.hpp>	// (rtl)
#include <System.Variants.hpp>	// (rtl)
#include <System.TypInfo.hpp>	// (rtl)
#include <System.Math.hpp>	// (rtl)
#include <System.Generics.Defaults.hpp>	// (rtl)
#include <System.Rtti.hpp>	// (rtl)
#include <System.TimeSpan.hpp>	// (rtl)
#include <System.Classes.hpp>	// (rtl)
#include <Winapi.ShellAPI.hpp>	// (rtl)
#include <System.DateUtils.hpp>	// (rtl)
#include <System.IOUtils.hpp>	// (rtl)
#include <System.IniFiles.hpp>	// (rtl)
#include <System.Win.Registry.hpp>	// (rtl)
#include <System.UIConsts.hpp>	// (rtl)
#include <Vcl.Graphics.hpp>	// (vcl)
#include <Winapi.OpenGL.hpp>	// (rtl)
#include <System.SyncObjs.hpp>	// (rtl)
#include <Vcl.Imaging.pngimage.hpp>	// (vclimg)
#include <System.Messaging.hpp>	// (rtl)
#include <System.Actions.hpp>	// (rtl)
#include <Winapi.UxTheme.hpp>	// (rtl)
#include <Vcl.ActnList.hpp>	// (vcl)
#include <System.AnsiStrings.hpp>	// (rtl)
#include <System.Win.ComObj.hpp>	// (rtl)
#include <Winapi.MsCTF.hpp>	// (rtl)
#include <Vcl.GraphUtil.hpp>	// (vcl)
#include <Vcl.Controls.hpp>	// (vcl)
#include <Vcl.StdCtrls.hpp>	// (vcl)
#include <Vcl.Clipbrd.hpp>	// (vcl)
#include <Vcl.Printers.hpp>	// (vcl)
#include <Vcl.ComCtrls.hpp>	// (vcl)
#include <System.HelpIntfs.hpp>	// (rtl)
#include <Vcl.Dialogs.hpp>	// (vcl)
#include <Vcl.ExtCtrls.hpp>	// (vcl)
#include <Vcl.Themes.hpp>	// (vcl)
#include <Vcl.Menus.hpp>	// (vcl)
#include <Winapi.FlatSB.hpp>	// (rtl)
#include <Vcl.Forms.hpp>	// (vcl)
#include <Vcl.Buttons.hpp>	// (vcl)
#include <Vcl.ExtDlgs.hpp>	// (vcl)
#include <Vcl.Imaging.jpeg.hpp>	// (vclimg)
#include <Xml.Win.msxmldom.hpp>	// (xmlrtl)
#include <Xml.xmldom.hpp>	// (xmlrtl)
#include <Xml.XMLSchema.hpp>	// (xmlrtl)
#include <Xml.xmlutil.hpp>	// (xmlrtl)
// PRG_EXT: .bpl
// BPI_DIR: ..\lib\Win32
// OBJ_DIR: ..\lib\Win32
// OBJ_EXT: .obj

//-- user supplied -----------------------------------------------------------

namespace Glscene_rt
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glscene_rt */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCENE_RT)
using namespace Glscene_rt;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glscene_rtHPP
