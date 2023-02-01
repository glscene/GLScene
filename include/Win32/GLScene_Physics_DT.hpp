// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLScene_Physics_DT.dpk' rev: 35.00 (Windows)

#ifndef Glscene_physics_dtHPP
#define Glscene_physics_dtHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// (rtl)
#include <SysInit.hpp>
#include <Physics.Register.hpp>
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
#include <Winapi.OpenGL.hpp>	// (rtl)
#include <GLS.VectorGeometry.hpp>	// (GLScene_RT)
#include <Winapi.ShellAPI.hpp>	// (rtl)
#include <System.Messaging.hpp>	// (rtl)
#include <System.Actions.hpp>	// (rtl)
#include <System.DateUtils.hpp>	// (rtl)
#include <System.IOUtils.hpp>	// (rtl)
#include <System.IniFiles.hpp>	// (rtl)
#include <System.Win.Registry.hpp>	// (rtl)
#include <System.UIConsts.hpp>	// (rtl)
#include <Vcl.Graphics.hpp>	// (vcl)
#include <System.SyncObjs.hpp>	// (rtl)
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
#include <GLS.Utils.hpp>	// (GLScene_RT)
#include <GLS.PersistentClasses.hpp>	// (GLScene_RT)
#include <GLS.VectorLists.hpp>	// (GLScene_RT)
#include <GLS.Logger.hpp>	// (GLScene_RT)
#include <GLS.Manager.hpp>	// (GLScene_RT)
#include <GLS.XCollection.hpp>	// (GLScene_RT)
#include <GLS.OpenGLAdapter.hpp>	// (GLScene_RT)
#include <GLS.Context.hpp>	// (GLScene_RT)
#include <GLS.Color.hpp>	// (GLScene_RT)
#include <GLS.XOpenGL.hpp>	// (GLScene_RT)
#include <Vcl.Imaging.pngimage.hpp>	// (vclimg)
#include <GLS.Graphics.hpp>	// (GLScene_RT)
#include <Vcl.Imaging.jpeg.hpp>	// (vclimg)
#include <GLS.Coordinates.hpp>	// (GLScene_RT)
#include <GLS.Texture.hpp>	// (GLScene_RT)
#include <GLS.Material.hpp>	// (GLScene_RT)
#include <GLS.Scene.hpp>	// (GLScene_RT)
#include <GLS.Objects.hpp>	// (GLScene_RT)
#include <GLS.HeightData.hpp>	// (GLScene_RT)
#include <GLS.Mesh.hpp>	// (GLScene_RT)
#include <GLS.MeshUtils.hpp>	// (GLScene_RT)
#include <GLS.VectorFileObjects.hpp>	// (GLScene_RT)
#include <GLS.GeomObjects.hpp>	// (GLScene_RT)
#include <GLS.MultiPolygon.hpp>	// (GLScene_RT)
#include <GLS.SpaceText.hpp>	// (GLScene_RT)
#include <GLS.Isolines.hpp>	// (GLScene_RT)
#include <GLS.ROAMPatch.hpp>	// (GLScene_RT)
#include <GLS.TerrainRenderer.hpp>	// (GLScene_RT)
#include <GLS.Graph.hpp>	// (GLScene_RT)
#include <Physics.ODEImport.hpp>	// (GLScene_Physics_RT)
#include <GLS.WindowsContext.hpp>	// (GLScene_RT)
#include <GLS.SceneViewer.hpp>	// (GLScene_RT)
#include <GLS.VerletTypes.hpp>	// (GLScene_RT)
#include <Physics.ODEManager.hpp>	// (GLScene_Physics_RT)
#include <Physics.NGDManager.hpp>	// (GLScene_Physics_RT)
#include <GLS.Behaviours.hpp>	// (GLScene_RT)
#include <Physics.SPIInertias.hpp>	// (GLScene_Physics_RT)
#include <Physics.SPIForces.hpp>	// (GLScene_Physics_RT)
#include <Physics.SPIManager.hpp>	// (GLScene_Physics_RT)
// PRG_EXT: .bpl
// BPI_DIR: ..\lib\Win32
// OBJ_DIR: ..\lib\Win32
// OBJ_EXT: .obj

//-- user supplied -----------------------------------------------------------

namespace Glscene_physics_dt
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glscene_physics_dt */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCENE_PHYSICS_DT)
using namespace Glscene_physics_dt;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glscene_physics_dtHPP
