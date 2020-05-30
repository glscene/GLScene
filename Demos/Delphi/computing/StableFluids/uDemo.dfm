object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'GLScene CUDA Fluids'
  ClientHeight = 512
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 512
    Height = 512
    Camera = GLCamera1
    VSync = vsmSync
    Buffer.BackgroundColor = clBlack
    Buffer.AntiAliasing = csa16xHQ
    FieldOfView = 157.897079467773400000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    OnMouseUp = GLSceneViewer1MouseUp
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 40
    Top = 104
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
    end
    object ParticleRenderer: TGLFeedBackMesh
      Attributes = <
        item
          Name = 'Position'
          GLSLType = GLSLType2F
          KernelFunction = advectParticles
          OnBeforeKernelLaunch = BeforeKernelLaunch
        end>
      Shader = FluidShader
      Launching = fblOnePerAtttribute
    end
    object ResetButton: TGLButton
      Autosize = False
      RedrawAtOnce = False
      GuiLayout = GLGuiLayout1
      NoZWrite = False
      DoChangesOnProgress = False
      Width = 45.000000000000000000
      Height = 50.000000000000000000
      Left = 30.000000000000000000
      Top = 20.000000000000000000
      Position.Coordinates = {0000F0410000A041000000000000803F}
      BitmapFont = GLWindowsBitmapFont1
      DefaultColor = clWhite
      Caption = 'Reset'
      Focused = False
      FocusedColor = clWhite
      Group = -1
      BitBtn.BlendingMode = bmTransparency
      BitBtn.Texture.Image.Picture.Data = {
        07544269746D6170F6070000424DF60700000000000036040000280000002D00
        0000140000000100080000000000C00300000000000000000000000100000000
        000000000000000080000080000000808000800000008000800080800000C0C0
        C000C0DCC000F0CAA6000020400000206000002080000020A0000020C0000020
        E00000400000004020000040400000406000004080000040A0000040C0000040
        E00000600000006020000060400000606000006080000060A0000060C0000060
        E00000800000008020000080400000806000008080000080A0000080C0000080
        E00000A0000000A0200000A0400000A0600000A0800000A0A00000A0C00000A0
        E00000C0000000C0200000C0400000C0600000C0800000C0A00000C0C00000C0
        E00000E0000000E0200000E0400000E0600000E0800000E0A00000E0C00000E0
        E00040000000400020004000400040006000400080004000A0004000C0004000
        E00040200000402020004020400040206000402080004020A0004020C0004020
        E00040400000404020004040400040406000404080004040A0004040C0004040
        E00040600000406020004060400040606000406080004060A0004060C0004060
        E00040800000408020004080400040806000408080004080A0004080C0004080
        E00040A0000040A0200040A0400040A0600040A0800040A0A00040A0C00040A0
        E00040C0000040C0200040C0400040C0600040C0800040C0A00040C0C00040C0
        E00040E0000040E0200040E0400040E0600040E0800040E0A00040E0C00040E0
        E00080000000800020008000400080006000800080008000A0008000C0008000
        E00080200000802020008020400080206000802080008020A0008020C0008020
        E00080400000804020008040400080406000804080008040A0008040C0008040
        E00080600000806020008060400080606000806080008060A0008060C0008060
        E00080800000808020008080400080806000808080008080A0008080C0008080
        E00080A0000080A0200080A0400080A0600080A0800080A0A00080A0C00080A0
        E00080C0000080C0200080C0400080C0600080C0800080C0A00080C0C00080C0
        E00080E0000080E0200080E0400080E0600080E0800080E0A00080E0C00080E0
        E000C0000000C0002000C0004000C0006000C0008000C000A000C000C000C000
        E000C0200000C0202000C0204000C0206000C0208000C020A000C020C000C020
        E000C0400000C0402000C0404000C0406000C0408000C040A000C040C000C040
        E000C0600000C0602000C0604000C0606000C0608000C060A000C060C000C060
        E000C0800000C0802000C0804000C0806000C0808000C080A000C080C000C080
        E000C0A00000C0A02000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0
        E000C0C00000C0C02000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0
        A000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFF4F4F4F4F4F4F4F4F4F
        4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4FFFFFFFFFFFFF00
        0000FFFFFF4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F
        4F4F4F4F4F4F4F4F4F4F4F4FFFFFFF000000FFFF4F4F4F4F4F4F4F4F4F4F4F4F
        4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4FFFFF00
        0000FF4F4F4F4F4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFF4F4F4F4F4FFF000000FF4F4F4FFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4F4F4FFF00
        0000FF4F4F4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFF4F4F4FFF000000FF4F4F4FFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4F4F4FFF00
        0000FF4F4F4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFF4F4F4FFF000000FF4F4F4FFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4F4F4FFF00
        0000FF4F4F4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFF4F4F4FFF000000FF4F4F4FFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4F4F4FFF00
        0000FF4F4F4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFF4F4F4FFF000000FF4F4F4FFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4F4F4FFF00
        0000FF4F4F4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFF4F4F4FFF000000FF4F4F4F4F4FFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4F4F4F4F4FFF00
        0000FFFF4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F
        4F4F4F4F4F4F4F4F4F4F4F4F4FFFFF000000FFFFFF4F4F4F4F4F4F4F4F4F4F4F
        4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4FFFFFFF00
        0000FFFFFFFFFF4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F
        4F4F4F4F4F4F4F4F4FFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000}
      BitBtn.Texture.ImageAlpha = tiaInverseLuminance
      BitBtn.Texture.MagFilter = maNearest
      BitBtn.Texture.MinFilter = miNearest
      BitBtn.Texture.TextureMode = tmReplace
      BitBtn.Texture.Disabled = False
      Pressed = False
      OnButtonClick = ResetButtonButtonClick
      LogicWidth = 50.000000000000000000
      LogicHeight = 21.000000000000000000
      XOffset = 24.000000000000000000
      YOffset = 10.000000000000000000
      AllowUp = False
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 40
    Top = 48
  end
  object GLSCUDADevice1: TGLSCUDADevice
    SelectDevice = 'GeForce GTX 260 (1)'
    Left = 448
    Top = 48
  end
  object GLSCUDA1: TGLSCUDA
    ComputingDevice = GLSCUDADevice1
    OnOpenGLInteropInit = GLSCUDA1OpenGLInteropInit
    Left = 448
    Top = 104
    object MainModule: TCUDAModule
      Code.Strings = (
        #9'.version 1.4'
        #9'.target sm_13'
        
          #9'// compiled with C:\Program Files\NVIDIA GPU Computing Toolkit\' +
          'CUDA\v3.2\\bin/../open64/lib//be.exe'
        #9'// nvopencc 3.2 built on 2010-11-06'
        ''
        #9'//-----------------------------------------------------------'
        
          #9'// Compiling C:/Users/YARUNA~1/AppData/Local/Temp/tmpxft_000009' +
          'd4_00000000-11_temp.cpp3.i (C:/Users/YARUNA~1/AppData/Local/Temp' +
          '/ccBI#.a02624)'
        #9'//-----------------------------------------------------------'
        ''
        #9'//-----------------------------------------------------------'
        #9'// Options:'
        #9'//-----------------------------------------------------------'
        #9'//  Target:ptx, ISA:sm_13, Endian:little, Pointer Size:32'
        #9'//  -O3'#9'(Optimization level)'
        #9'//  -g0'#9'(Debug level)'
        #9'//  -m2'#9'(Report advisories)'
        #9'//-----------------------------------------------------------'
        ''
        
          #9'.file'#9'1'#9'"C:/Users/YARUNA~1/AppData/Local/Temp/tmpxft_000009d4_0' +
          '0000000-10_temp.cudafe2.gpu"'
        
          #9'.file'#9'2'#9'"C:\Program Files\Microsoft Visual Studio 9.0\VC\INCLUD' +
          'E\crtdefs.h"'
        
          #9'.file'#9'3'#9'"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v3.' +
          '2\include\crt/device_runtime.h"'
        
          #9'.file'#9'4'#9'"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v3.' +
          '2\include\host_defines.h"'
        
          #9'.file'#9'5'#9'"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v3.' +
          '2\include\builtin_types.h"'
        
          #9'.file'#9'6'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3.' +
          '2\include\device_types.h"'
        
          #9'.file'#9'7'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3.' +
          '2\include\driver_types.h"'
        
          #9'.file'#9'8'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3.' +
          '2\include\surface_types.h"'
        
          #9'.file'#9'9'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3.' +
          '2\include\texture_types.h"'
        
          #9'.file'#9'10'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\vector_types.h"'
        
          #9'.file'#9'11'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\builtin_types.h"'
        
          #9'.file'#9'12'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\host_defines.h"'
        
          #9'.file'#9'13'#9'"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v3' +
          '.2\include\device_launch_parameters.h"'
        
          #9'.file'#9'14'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\crt\storage_class.h"'
        
          #9'.file'#9'15'#9'"C:\Program Files\Microsoft Visual Studio 9.0\VC\INCLU' +
          'DE\time.h"'
        #9'.file'#9'16'#9'"C:/Users/YARUNA~1/AppData/Local/Temp/temp.cu"'
        
          #9'.file'#9'17'#9'"C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v3' +
          '.2\include\common_functions.h"'
        
          #9'.file'#9'18'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\math_functions.h"'
        
          #9'.file'#9'19'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\math_constants.h"'
        
          #9'.file'#9'20'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\device_functions.h"'
        
          #9'.file'#9'21'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\sm_11_atomic_functions.h"'
        
          #9'.file'#9'22'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\sm_12_atomic_functions.h"'
        
          #9'.file'#9'23'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\sm_13_double_functions.h"'
        
          #9'.file'#9'24'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\sm_20_atomic_functions.h"'
        
          #9'.file'#9'25'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\sm_20_intrinsics.h"'
        
          #9'.file'#9'26'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\surface_functions.h"'
        
          #9'.file'#9'27'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\texture_fetch_functions.h"'
        
          #9'.file'#9'28'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\math_functions_dbl_ptx3.h"'
        ''
        ''
        #9'.entry addForces_k ('
        #9#9'.param .u32 __cudaparm_addForces_k_v,'
        #9#9'.param .s32 __cudaparm_addForces_k_dx,'
        #9#9'.param .s32 __cudaparm_addForces_k_dy,'
        #9#9'.param .s32 __cudaparm_addForces_k_spx,'
        #9#9'.param .s32 __cudaparm_addForces_k_spy,'
        #9#9'.param .f32 __cudaparm_addForces_k_fx,'
        #9#9'.param .f32 __cudaparm_addForces_k_fy,'
        #9#9'.param .s32 __cudaparm_addForces_k_r,'
        #9#9'.param .u32 __cudaparm_addForces_k_pitch)'
        #9'{'
        #9'.reg .u32 %r<24>;'
        #9'.reg .f32 %f<14>;'
        #9'.loc'#9'16'#9'28'#9'0'
        '$LDWbegin_addForces_k:'
        #9'.loc'#9'16'#9'38'#9'0'
        #9'cvt.s32.u16 '#9'%r1, %tid.x;'
        #9'mul24.lo.u32 '#9'%r2, %r1, 8;'
        #9'cvt.s32.u16 '#9'%r3, %tid.y;'
        #9'ld.param.s32 '#9'%r4, [__cudaparm_addForces_k_spy];'
        #9'add.s32 '#9'%r5, %r4, %r3;'
        #9'ld.param.u32 '#9'%r6, [__cudaparm_addForces_k_spx];'
        #9'mul.lo.u32 '#9'%r7, %r6, 8;'
        #9'ld.param.s32 '#9'%r8, [__cudaparm_addForces_k_r];'
        #9'sub.s32 '#9'%r9, %r3, %r8;'
        #9'sub.s32 '#9'%r10, %r1, %r8;'
        #9'ld.param.u32 '#9'%r11, [__cudaparm_addForces_k_pitch];'
        #9'mul.lo.u32 '#9'%r12, %r5, %r11;'
        #9'mul.lo.s32 '#9'%r13, %r9, %r9;'
        #9'mul.lo.s32 '#9'%r14, %r10, %r10;'
        #9'add.u32 '#9'%r15, %r2, %r12;'
        #9'mul.lo.s32 '#9'%r16, %r9, %r13;'
        #9'mul.lo.s32 '#9'%r17, %r10, %r14;'
        #9'ld.param.u32 '#9'%r18, [__cudaparm_addForces_k_v];'
        #9'add.u32 '#9'%r19, %r18, %r15;'
        #9'mul.lo.s32 '#9'%r20, %r9, %r16;'
        #9'mul.lo.s32 '#9'%r21, %r10, %r17;'
        #9'add.u32 '#9'%r22, %r7, %r19;'
        #9'cvt.rn.f32.s32 '#9'%f1, %r20;'
        #9'cvt.rn.f32.s32 '#9'%f2, %r21;'
        #9'mov.f32 '#9'%f3, 0f3f800000;     '#9'// 1'
        #9'add.f32 '#9'%f4, %f2, %f3;'
        #9'add.f32 '#9'%f5, %f1, %f4;'
        #9'rcp.approx.f32 '#9'%f6, %f5;'
        #9'ld.global.v2.f32 '#9'{%f7,%f8}, [%r22+0];'
        #9'ld.param.f32 '#9'%f9, [__cudaparm_addForces_k_fy];'
        #9'mad.f32 '#9'%f10, %f9, %f6, %f8;'
        #9'.loc'#9'16'#9'39'#9'0'
        #9'ld.param.f32 '#9'%f11, [__cudaparm_addForces_k_fx];'
        #9'mad.f32 '#9'%f12, %f11, %f6, %f7;'
        #9'st.global.v2.f32 '#9'[%r22+0], {%f12,%f10};'
        #9'.loc'#9'16'#9'40'#9'0'
        #9'exit;'
        '$LDWend_addForces_k:'
        #9'} // addForces_k'
        #9'.tex .u32 texref;'
        ''
        #9'.entry advectVelocity_k ('
        #9#9'.param .u32 __cudaparm_advectVelocity_k_vx,'
        #9#9'.param .u32 __cudaparm_advectVelocity_k_vy,'
        #9#9'.param .s32 __cudaparm_advectVelocity_k_dx,'
        #9#9'.param .s32 __cudaparm_advectVelocity_k_pdx,'
        #9#9'.param .s32 __cudaparm_advectVelocity_k_dy,'
        #9#9'.param .f32 __cudaparm_advectVelocity_k_dt,'
        #9#9'.param .s32 __cudaparm_advectVelocity_k_lb)'
        #9'{'
        #9'.reg .u16 %rh<4>;'
        #9'.reg .u32 %r<28>;'
        #9'.reg .f32 %f<41>;'
        #9'.reg .pred %p<6>;'
        #9'.loc'#9'16'#9'48'#9'0'
        '$LDWbegin_advectVelocity_k:'
        #9'mov.u16 '#9'%rh1, %ctaid.x;'
        #9'mov.u16 '#9'%rh2, %ntid.x;'
        #9'mul.wide.u16 '#9'%r1, %rh1, %rh2;'
        #9'cvt.u32.u16 '#9'%r2, %tid.x;'
        #9'add.u32 '#9'%r3, %r2, %r1;'
        #9'ld.param.s32 '#9'%r4, [__cudaparm_advectVelocity_k_dx];'
        #9'setp.le.s32 '#9'%p1, %r4, %r3;'
        #9'@%p1 bra '#9'$Lt_1_2818;'
        #9'ld.param.u32 '#9'%r5, [__cudaparm_advectVelocity_k_lb];'
        #9'mov.u32 '#9'%r6, 0;'
        #9'setp.le.s32 '#9'%p2, %r5, %r6;'
        #9'@%p2 bra '#9'$Lt_1_3330;'
        #9'ld.param.u32 '#9'%r5, [__cudaparm_advectVelocity_k_lb];'
        #9'mov.s32 '#9'%r7, %r5;'
        #9'cvt.u32.u16 '#9'%r8, %tid.y;'
        #9'mul.lo.u32 '#9'%r9, %r8, %r5;'
        #9'cvt.u32.u16 '#9'%r10, %ntid.y;'
        #9'mul.lo.u32 '#9'%r11, %r10, %r5;'
        #9'cvt.u32.u16 '#9'%r12, %ctaid.y;'
        #9'mul.lo.u32 '#9'%r13, %r12, %r11;'
        #9'add.s32 '#9'%r14, %r9, %r13;'
        #9'ld.param.s32 '#9'%r15, [__cudaparm_advectVelocity_k_dy];'
        #9'mov.s32 '#9'%r16, 0;'
        #9'mov.s32 '#9'%r17, %r7;'
        '$Lt_1_3842:'
        
          ' //<loop> Loop body line 48, nesting depth: 1, estimated iterati' +
          'ons: unknown'
        #9'add.s32 '#9'%r18, %r14, %r16;'
        #9'ld.param.s32 '#9'%r15, [__cudaparm_advectVelocity_k_dy];'
        #9'setp.ge.s32 '#9'%p3, %r18, %r15;'
        #9'@%p3 bra '#9'$Lt_1_4098;'
        ' //<loop> Part of loop body line 48, head labeled $Lt_1_3842'
        #9'cvt.rn.f32.s32 '#9'%f1, %r3;'
        #9'cvt.rn.f32.s32 '#9'%f2, %r18;'
        #9'mov.f32 '#9'%f3, %f1;'
        #9'mov.f32 '#9'%f4, %f2;'
        #9'mov.f32 '#9'%f5, 0f00000000;     '#9'// 0'
        #9'mov.f32 '#9'%f6, %f5;'
        #9'mov.f32 '#9'%f7, 0f00000000;     '#9'// 0'
        #9'mov.f32 '#9'%f8, %f7;'
        
          #9'tex.2d.v4.f32.f32 {%f9,%f10,%f11,%f12},[texref,{%f3,%f4,%f6,%f8' +
          '}];'
        ' //<loop> Part of loop body line 48, head labeled $Lt_1_3842'
        #9'.loc'#9'16'#9'63'#9'0'
        #9'mov.f32 '#9'%f13, %f9;'
        #9'mov.f32 '#9'%f14, %f10;'
        #9'ld.param.f32 '#9'%f15, [__cudaparm_advectVelocity_k_dt];'
        #9'mov.f32 '#9'%f16, 0f3f000000;    '#9'// 0.5'
        #9'add.f32 '#9'%f17, %f1, %f16;'
        #9'.loc'#9'16'#9'48'#9'0'
        #9'ld.param.s32 '#9'%r4, [__cudaparm_advectVelocity_k_dx];'
        #9'.loc'#9'16'#9'63'#9'0'
        #9'cvt.rn.f32.s32 '#9'%f18, %r4;'
        #9'mul.f32 '#9'%f19, %f13, %f15;'
        #9'mul.f32 '#9'%f20, %f18, %f19;'
        #9'sub.f32 '#9'%f21, %f17, %f20;'
        #9'mov.f32 '#9'%f22, %f21;'
        #9'mov.f32 '#9'%f23, 0f3f000000;    '#9'// 0.5'
        #9'add.f32 '#9'%f24, %f2, %f23;'
        #9'.loc'#9'16'#9'48'#9'0'
        #9'ld.param.s32 '#9'%r15, [__cudaparm_advectVelocity_k_dy];'
        #9'.loc'#9'16'#9'63'#9'0'
        #9'cvt.rn.f32.s32 '#9'%f25, %r15;'
        #9'mul.f32 '#9'%f26, %f14, %f15;'
        #9'mul.f32 '#9'%f27, %f25, %f26;'
        #9'sub.f32 '#9'%f28, %f24, %f27;'
        #9'mov.f32 '#9'%f29, %f28;'
        #9'mov.f32 '#9'%f30, 0f00000000;    '#9'// 0'
        #9'mov.f32 '#9'%f31, %f30;'
        #9'mov.f32 '#9'%f32, 0f00000000;    '#9'// 0'
        #9'mov.f32 '#9'%f33, %f32;'
        
          #9'tex.2d.v4.f32.f32 {%f34,%f35,%f36,%f37},[texref,{%f22,%f29,%f31' +
          ',%f33}];'
        ' //<loop> Part of loop body line 48, head labeled $Lt_1_3842'
        #9'.loc'#9'16'#9'66'#9'0'
        #9'mov.f32 '#9'%f38, %f34;'
        #9'mov.f32 '#9'%f39, %f35;'
        #9'.loc'#9'16'#9'68'#9'0'
        #9'ld.param.s32 '#9'%r19, [__cudaparm_advectVelocity_k_pdx];'
        #9'mul.lo.s32 '#9'%r20, %r19, %r18;'
        #9'add.s32 '#9'%r21, %r20, %r3;'
        #9'mul.lo.u32 '#9'%r22, %r21, 4;'
        #9'ld.param.u32 '#9'%r23, [__cudaparm_advectVelocity_k_vx];'
        #9'add.u32 '#9'%r24, %r23, %r22;'
        #9'st.global.f32 '#9'[%r24+0], %f38;'
        #9'.loc'#9'16'#9'69'#9'0'
        #9'ld.param.u32 '#9'%r25, [__cudaparm_advectVelocity_k_vy];'
        #9'add.u32 '#9'%r26, %r25, %r22;'
        #9'st.global.f32 '#9'[%r26+0], %f39;'
        '$Lt_1_4098:'
        ' //<loop> Part of loop body line 48, head labeled $Lt_1_3842'
        #9'add.s32 '#9'%r16, %r16, 1;'
        #9'.loc'#9'16'#9'48'#9'0'
        #9'ld.param.u32 '#9'%r5, [__cudaparm_advectVelocity_k_lb];'
        #9'.loc'#9'16'#9'69'#9'0'
        #9'setp.ne.s32 '#9'%p4, %r16, %r5;'
        #9'@%p4 bra '#9'$Lt_1_3842;'
        '$Lt_1_3330:'
        '$Lt_1_2818:'
        #9'.loc'#9'16'#9'73'#9'0'
        #9'exit;'
        '$LDWend_advectVelocity_k:'
        #9'} // advectVelocity_k'
        ''
        #9'.entry diffuseProject_k ('
        #9#9'.param .u32 __cudaparm_diffuseProject_k_vx,'
        #9#9'.param .u32 __cudaparm_diffuseProject_k_vy,'
        #9#9'.param .s32 __cudaparm_diffuseProject_k_dx,'
        #9#9'.param .s32 __cudaparm_diffuseProject_k_dy,'
        #9#9'.param .f32 __cudaparm_diffuseProject_k_dt,'
        #9#9'.param .f32 __cudaparm_diffuseProject_k_visc,'
        #9#9'.param .s32 __cudaparm_diffuseProject_k_lb)'
        #9'{'
        #9'.reg .u16 %rh<4>;'
        #9'.reg .u32 %r<37>;'
        #9'.reg .f32 %f<31>;'
        #9'.reg .pred %p<8>;'
        #9'.loc'#9'16'#9'91'#9'0'
        '$LDWbegin_diffuseProject_k:'
        #9'mov.u16 '#9'%rh1, %ctaid.x;'
        #9'mov.u16 '#9'%rh2, %ntid.x;'
        #9'mul.wide.u16 '#9'%r1, %rh1, %rh2;'
        #9'cvt.u32.u16 '#9'%r2, %tid.x;'
        #9'add.u32 '#9'%r3, %r2, %r1;'
        #9'ld.param.s32 '#9'%r4, [__cudaparm_diffuseProject_k_dx];'
        #9'setp.le.s32 '#9'%p1, %r4, %r3;'
        #9'@%p1 bra '#9'$Lt_2_4354;'
        #9'ld.param.u32 '#9'%r5, [__cudaparm_diffuseProject_k_lb];'
        #9'mov.u32 '#9'%r6, 0;'
        #9'setp.le.s32 '#9'%p2, %r5, %r6;'
        #9'@%p2 bra '#9'$Lt_2_4866;'
        #9'ld.param.u32 '#9'%r5, [__cudaparm_diffuseProject_k_lb];'
        #9'mov.s32 '#9'%r7, %r5;'
        #9'cvt.u32.u16 '#9'%r8, %tid.y;'
        #9'mul.lo.u32 '#9'%r9, %r8, %r5;'
        #9'cvt.u32.u16 '#9'%r10, %ntid.y;'
        #9'mul.lo.u32 '#9'%r11, %r10, %r5;'
        #9'cvt.u32.u16 '#9'%r12, %ctaid.y;'
        #9'mul.lo.u32 '#9'%r13, %r12, %r11;'
        #9'add.s32 '#9'%r14, %r9, %r13;'
        #9'ld.param.s32 '#9'%r15, [__cudaparm_diffuseProject_k_dy];'
        #9'mov.s32 '#9'%r16, 0;'
        #9'mov.s32 '#9'%r17, %r7;'
        '$Lt_2_5378:'
        
          ' //<loop> Loop body line 91, nesting depth: 1, estimated iterati' +
          'ons: unknown'
        #9'add.s32 '#9'%r18, %r14, %r16;'
        #9'ld.param.s32 '#9'%r15, [__cudaparm_diffuseProject_k_dy];'
        #9'setp.ge.s32 '#9'%p3, %r18, %r15;'
        #9'@%p3 bra '#9'$Lt_2_5634;'
        ' //<loop> Part of loop body line 91, head labeled $Lt_2_5378'
        #9'ld.param.s32 '#9'%r15, [__cudaparm_diffuseProject_k_dy];'
        #9'.loc'#9'16'#9'116'#9'0'
        #9'shr.s32 '#9'%r19, %r15, 31;'
        #9'mov.s32 '#9'%r20, 1;'
        #9'and.b32 '#9'%r21, %r19, %r20;'
        #9'add.s32 '#9'%r22, %r21, %r15;'
        #9'shr.s32 '#9'%r23, %r22, 1;'
        #9'ld.param.f32 '#9'%f1, [__cudaparm_diffuseProject_k_dt];'
        #9'ld.param.f32 '#9'%f2, [__cudaparm_diffuseProject_k_visc];'
        #9'mul.f32 '#9'%f3, %f1, %f2;'
        #9'mul.lo.s32 '#9'%r24, %r3, %r3;'
        #9'.loc'#9'16'#9'91'#9'0'
        #9'ld.param.s32 '#9'%r4, [__cudaparm_diffuseProject_k_dx];'
        #9'.loc'#9'16'#9'116'#9'0'
        #9'mul.lo.s32 '#9'%r25, %r18, %r4;'
        #9'setp.lt.s32 '#9'%p4, %r23, %r18;'
        #9'sub.s32 '#9'%r26, %r18, %r15;'
        #9'add.s32 '#9'%r27, %r25, %r3;'
        #9'selp.s32 '#9'%r28, %r26, %r18, %p4;'
        #9'mul.lo.u32 '#9'%r29, %r27, 8;'
        #9'mul.lo.s32 '#9'%r30, %r28, %r28;'
        #9'ld.param.u32 '#9'%r31, [__cudaparm_diffuseProject_k_vx];'
        #9'add.u32 '#9'%r32, %r31, %r29;'
        #9'add.s32 '#9'%r33, %r24, %r30;'
        #9'cvt.rn.f32.s32 '#9'%f4, %r33;'
        #9'mov.f32 '#9'%f5, 0f3f800000;     '#9'// 1'
        #9'mad.f32 '#9'%f6, %f4, %f3, %f5;'
        #9'rcp.approx.f32 '#9'%f7, %f6;'
        #9'ld.global.v2.f32 '#9'{%f8,%f9}, [%r32+0];'
        #9'mul.f32 '#9'%f10, %f8, %f7;'
        #9'mul.f32 '#9'%f11, %f9, %f7;'
        #9'.loc'#9'16'#9'117'#9'0'
        #9'ld.param.u32 '#9'%r34, [__cudaparm_diffuseProject_k_vy];'
        #9'add.u32 '#9'%r35, %r34, %r29;'
        #9'ld.global.v2.f32 '#9'{%f12,%f13}, [%r35+0];'
        #9'mul.f32 '#9'%f14, %f12, %f7;'
        #9'mul.f32 '#9'%f15, %f13, %f7;'
        #9'mov.f32 '#9'%f16, 0f00000000;    '#9'// 0'
        #9'setp.gt.f32 '#9'%p5, %f4, %f16;'
        #9'@!%p5 bra '#9'$Lt_2_6146;'
        ' //<loop> Part of loop body line 91, head labeled $Lt_2_5378'
        #9'.loc'#9'16'#9'123'#9'0'
        #9'cvt.rn.f32.s32 '#9'%f17, %r3;'
        #9'cvt.rn.f32.s32 '#9'%f18, %r28;'
        #9'mul.f32 '#9'%f19, %f18, %f14;'
        #9'mad.f32 '#9'%f20, %f17, %f10, %f19;'
        #9'.loc'#9'16'#9'125'#9'0'
        #9'mul.f32 '#9'%f21, %f18, %f15;'
        #9'mad.f32 '#9'%f22, %f17, %f11, %f21;'
        #9'.loc'#9'16'#9'126'#9'0'
        #9'rcp.approx.f32 '#9'%f23, %f4;'
        #9'mul.f32 '#9'%f24, %f20, %f23;'
        #9'mul.f32 '#9'%f25, %f17, %f24;'
        #9'sub.f32 '#9'%f10, %f10, %f25;'
        #9'.loc'#9'16'#9'127'#9'0'
        #9'mul.f32 '#9'%f26, %f22, %f23;'
        #9'mul.f32 '#9'%f27, %f17, %f26;'
        #9'sub.f32 '#9'%f11, %f11, %f27;'
        #9'.loc'#9'16'#9'128'#9'0'
        #9'mul.f32 '#9'%f28, %f18, %f24;'
        #9'sub.f32 '#9'%f14, %f14, %f28;'
        #9'.loc'#9'16'#9'129'#9'0'
        #9'mul.f32 '#9'%f29, %f18, %f26;'
        #9'sub.f32 '#9'%f15, %f15, %f29;'
        '$Lt_2_6146:'
        ' //<loop> Part of loop body line 91, head labeled $Lt_2_5378'
        #9'st.global.v2.f32 '#9'[%r32+0], {%f10,%f11};'
        #9'st.global.v2.f32 '#9'[%r35+0], {%f14,%f15};'
        '$Lt_2_5634:'
        ' //<loop> Part of loop body line 91, head labeled $Lt_2_5378'
        #9'.loc'#9'16'#9'133'#9'0'
        #9'add.s32 '#9'%r16, %r16, 1;'
        #9'.loc'#9'16'#9'91'#9'0'
        #9'ld.param.u32 '#9'%r5, [__cudaparm_diffuseProject_k_lb];'
        #9'.loc'#9'16'#9'133'#9'0'
        #9'setp.ne.s32 '#9'%p6, %r16, %r5;'
        #9'@%p6 bra '#9'$Lt_2_5378;'
        '$Lt_2_4866:'
        '$Lt_2_4354:'
        #9'.loc'#9'16'#9'137'#9'0'
        #9'exit;'
        '$LDWend_diffuseProject_k:'
        #9'} // diffuseProject_k'
        ''
        #9'.entry updateVelocity_k ('
        #9#9'.param .u32 __cudaparm_updateVelocity_k_v,'
        #9#9'.param .u32 __cudaparm_updateVelocity_k_vx,'
        #9#9'.param .u32 __cudaparm_updateVelocity_k_vy,'
        #9#9'.param .s32 __cudaparm_updateVelocity_k_dx,'
        #9#9'.param .s32 __cudaparm_updateVelocity_k_pdx,'
        #9#9'.param .s32 __cudaparm_updateVelocity_k_dy,'
        #9#9'.param .s32 __cudaparm_updateVelocity_k_lb,'
        #9#9'.param .u32 __cudaparm_updateVelocity_k_pitch,'
        #9#9'.param .f32 __cudaparm_updateVelocity_k_scale)'
        #9'{'
        #9'.reg .u16 %rh<4>;'
        #9'.reg .u32 %r<34>;'
        #9'.reg .f32 %f<7>;'
        #9'.reg .pred %p<6>;'
        #9'.loc'#9'16'#9'152'#9'0'
        '$LDWbegin_updateVelocity_k:'
        #9'mov.u16 '#9'%rh1, %ctaid.x;'
        #9'mov.u16 '#9'%rh2, %ntid.x;'
        #9'mul.wide.u16 '#9'%r1, %rh1, %rh2;'
        #9'cvt.u32.u16 '#9'%r2, %tid.x;'
        #9'add.u32 '#9'%r3, %r2, %r1;'
        #9'ld.param.s32 '#9'%r4, [__cudaparm_updateVelocity_k_dx];'
        #9'setp.le.s32 '#9'%p1, %r4, %r3;'
        #9'@%p1 bra '#9'$Lt_3_2818;'
        #9'ld.param.u32 '#9'%r5, [__cudaparm_updateVelocity_k_lb];'
        #9'mov.u32 '#9'%r6, 0;'
        #9'setp.le.s32 '#9'%p2, %r5, %r6;'
        #9'@%p2 bra '#9'$Lt_3_3330;'
        #9'ld.param.u32 '#9'%r5, [__cudaparm_updateVelocity_k_lb];'
        #9'mov.s32 '#9'%r7, %r5;'
        #9'cvt.u32.u16 '#9'%r8, %tid.y;'
        #9'mul.lo.u32 '#9'%r9, %r8, %r5;'
        #9'cvt.u32.u16 '#9'%r10, %ntid.y;'
        #9'mul.lo.u32 '#9'%r11, %r10, %r5;'
        #9'cvt.u32.u16 '#9'%r12, %ctaid.y;'
        #9'mul.lo.u32 '#9'%r13, %r12, %r11;'
        #9'add.s32 '#9'%r14, %r9, %r13;'
        #9'ld.param.s32 '#9'%r15, [__cudaparm_updateVelocity_k_dy];'
        #9'mov.s32 '#9'%r16, 0;'
        #9'mov.s32 '#9'%r17, %r7;'
        '$Lt_3_3842:'
        
          ' //<loop> Loop body line 152, nesting depth: 1, estimated iterat' +
          'ions: unknown'
        #9'add.s32 '#9'%r18, %r14, %r16;'
        #9'ld.param.s32 '#9'%r15, [__cudaparm_updateVelocity_k_dy];'
        #9'setp.ge.s32 '#9'%p3, %r18, %r15;'
        #9'@%p3 bra '#9'$Lt_3_4098;'
        ' //<loop> Part of loop body line 152, head labeled $Lt_3_3842'
        #9'.loc'#9'16'#9'168'#9'0'
        #9'ld.param.s32 '#9'%r19, [__cudaparm_updateVelocity_k_pdx];'
        #9'mul.lo.s32 '#9'%r20, %r19, %r18;'
        #9'add.s32 '#9'%r21, %r20, %r3;'
        #9'mul.lo.u32 '#9'%r22, %r21, 4;'
        #9'ld.param.u32 '#9'%r23, [__cudaparm_updateVelocity_k_vy];'
        #9'add.u32 '#9'%r24, %r23, %r22;'
        #9'ld.global.f32 '#9'%f1, [%r24+0];'
        #9'.loc'#9'16'#9'175'#9'0'
        #9'mul.lo.u32 '#9'%r25, %r3, 8;'
        #9'ld.param.u32 '#9'%r26, [__cudaparm_updateVelocity_k_pitch];'
        #9'mul.lo.u32 '#9'%r27, %r18, %r26;'
        #9'add.u32 '#9'%r28, %r25, %r27;'
        #9'ld.param.u32 '#9'%r29, [__cudaparm_updateVelocity_k_v];'
        #9'add.u32 '#9'%r30, %r29, %r28;'
        #9'ld.param.f32 '#9'%f2, [__cudaparm_updateVelocity_k_scale];'
        #9'ld.param.u32 '#9'%r31, [__cudaparm_updateVelocity_k_vx];'
        #9'add.u32 '#9'%r32, %r31, %r22;'
        #9'ld.global.f32 '#9'%f3, [%r32+0];'
        #9'mul.f32 '#9'%f4, %f3, %f2;'
        #9'mul.f32 '#9'%f5, %f1, %f2;'
        #9'st.global.v2.f32 '#9'[%r30+0], {%f4,%f5};'
        '$Lt_3_4098:'
        ' //<loop> Part of loop body line 152, head labeled $Lt_3_3842'
        #9'add.s32 '#9'%r16, %r16, 1;'
        #9'.loc'#9'16'#9'152'#9'0'
        #9'ld.param.u32 '#9'%r5, [__cudaparm_updateVelocity_k_lb];'
        #9'.loc'#9'16'#9'175'#9'0'
        #9'setp.ne.s32 '#9'%p4, %r16, %r5;'
        #9'@%p4 bra '#9'$Lt_3_3842;'
        '$Lt_3_3330:'
        '$Lt_3_2818:'
        #9'.loc'#9'16'#9'179'#9'0'
        #9'exit;'
        '$LDWend_updateVelocity_k:'
        #9'} // updateVelocity_k'
        ''
        #9'.entry advectParticles_k ('
        #9#9'.param .u32 __cudaparm_advectParticles_k_part,'
        #9#9'.param .u32 __cudaparm_advectParticles_k_v,'
        #9#9'.param .s32 __cudaparm_advectParticles_k_dx,'
        #9#9'.param .s32 __cudaparm_advectParticles_k_dy,'
        #9#9'.param .f32 __cudaparm_advectParticles_k_dt,'
        #9#9'.param .s32 __cudaparm_advectParticles_k_lb,'
        #9#9'.param .u32 __cudaparm_advectParticles_k_pitch)'
        #9'{'
        #9'.reg .u16 %rh<4>;'
        #9'.reg .u32 %r<40>;'
        #9'.reg .f32 %f<25>;'
        #9'.reg .pred %p<6>;'
        
          #9'.local .align 8 .b8 __cuda___cuda_local_var_86465_19_non_const_' +
          'vterm_168[8];'
        #9'.loc'#9'16'#9'192'#9'0'
        '$LDWbegin_advectParticles_k:'
        #9'mov.u16 '#9'%rh1, %ctaid.x;'
        #9'mov.u16 '#9'%rh2, %ntid.x;'
        #9'mul.wide.u16 '#9'%r1, %rh1, %rh2;'
        #9'cvt.u32.u16 '#9'%r2, %tid.x;'
        #9'add.u32 '#9'%r3, %r2, %r1;'
        #9'ld.param.s32 '#9'%r4, [__cudaparm_advectParticles_k_dx];'
        #9'setp.le.s32 '#9'%p1, %r4, %r3;'
        #9'@%p1 bra '#9'$Lt_4_2818;'
        #9'ld.param.u32 '#9'%r5, [__cudaparm_advectParticles_k_lb];'
        #9'mov.u32 '#9'%r6, 0;'
        #9'setp.le.s32 '#9'%p2, %r5, %r6;'
        #9'@%p2 bra '#9'$Lt_4_3330;'
        #9'ld.param.u32 '#9'%r5, [__cudaparm_advectParticles_k_lb];'
        #9'mov.s32 '#9'%r7, %r5;'
        #9'cvt.u32.u16 '#9'%r8, %tid.y;'
        #9'mul.lo.u32 '#9'%r9, %r8, %r5;'
        #9'cvt.u32.u16 '#9'%r10, %ntid.y;'
        #9'mul.lo.u32 '#9'%r11, %r10, %r5;'
        #9'cvt.u32.u16 '#9'%r12, %ctaid.y;'
        #9'mul.lo.u32 '#9'%r13, %r12, %r11;'
        #9'add.s32 '#9'%r14, %r9, %r13;'
        #9'ld.param.s32 '#9'%r15, [__cudaparm_advectParticles_k_dy];'
        #9'mov.s32 '#9'%r16, 0;'
        #9'mov.s32 '#9'%r17, %r7;'
        '$Lt_4_3842:'
        
          ' //<loop> Loop body line 192, nesting depth: 1, estimated iterat' +
          'ions: unknown'
        #9'add.s32 '#9'%r18, %r14, %r16;'
        #9'ld.param.s32 '#9'%r15, [__cudaparm_advectParticles_k_dy];'
        #9'setp.ge.s32 '#9'%p3, %r18, %r15;'
        #9'@%p3 bra '#9'$Lt_4_4098;'
        ' //<loop> Part of loop body line 192, head labeled $Lt_4_3842'
        #9'ld.param.s32 '#9'%r4, [__cudaparm_advectParticles_k_dx];'
        #9'.loc'#9'16'#9'206'#9'0'
        #9'mul.lo.s32 '#9'%r19, %r18, %r4;'
        #9'add.s32 '#9'%r20, %r19, %r3;'
        #9'mul.lo.u32 '#9'%r21, %r20, 8;'
        #9'ld.param.u32 '#9'%r22, [__cudaparm_advectParticles_k_part];'
        #9'add.u32 '#9'%r23, %r22, %r21;'
        #9'ld.global.v2.f32 '#9'{%f1,%f2}, [%r23+0];'
        #9'.loc'#9'16'#9'210'#9'0'
        #9'ld.param.u32 '#9'%r24, [__cudaparm_advectParticles_k_v];'
        #9'ld.param.u32 '#9'%r25, [__cudaparm_advectParticles_k_pitch];'
        #9'.loc'#9'16'#9'192'#9'0'
        #9'ld.param.s32 '#9'%r15, [__cudaparm_advectParticles_k_dy];'
        #9'.loc'#9'16'#9'210'#9'0'
        #9'cvt.rn.f32.s32 '#9'%f3, %r15;'
        #9'mul.f32 '#9'%f4, %f3, %f2;'
        #9'cvt.rzi.s32.f32 '#9'%r26, %f4;'
        #9'mul.lo.u32 '#9'%r27, %r25, %r26;'
        #9'cvt.rn.f32.s32 '#9'%f5, %r4;'
        #9'mul.f32 '#9'%f6, %f5, %f1;'
        #9'cvt.rzi.s32.f32 '#9'%r28, %f6;'
        #9'mul.lo.u32 '#9'%r29, %r28, 8;'
        #9'add.u32 '#9'%r30, %r27, %r29;'
        #9'add.u32 '#9'%r31, %r24, %r30;'
        
          #9'mov.u32 '#9'%r32, __cuda___cuda_local_var_86465_19_non_const_vterm' +
          '_168;'
        #9'ld.global.v2.u32 '#9'{%r33,%r34}, [%r31+0];'
        #9'st.local.u32 '#9'[%r32+0], %r33;'
        #9'st.local.u32 '#9'[%r32+4], %r34;'
        #9'.loc'#9'16'#9'212'#9'0'
        #9'ld.param.f32 '#9'%f7, [__cudaparm_advectParticles_k_dt];'
        
          #9'ld.local.f32 '#9'%f8, [__cuda___cuda_local_var_86465_19_non_const_' +
          'vterm_168+0];'
        #9'mad.f32 '#9'%f9, %f7, %f8, %f1;'
        #9'.loc'#9'16'#9'213'#9'0'
        #9'cvt.rzi.s32.f32 '#9'%r35, %f9;'
        #9'cvt.rn.f32.s32 '#9'%f10, %r35;'
        #9'sub.f32 '#9'%f11, %f9, %f10;'
        #9'.loc'#9'16'#9'215'#9'0'
        #9'mov.f32 '#9'%f12, 0f3f800000;    '#9'// 1'
        #9'add.f32 '#9'%f13, %f11, %f12;'
        #9'cvt.rzi.s32.f32 '#9'%r36, %f13;'
        #9'cvt.rn.f32.s32 '#9'%f14, %r36;'
        #9'sub.f32 '#9'%f15, %f13, %f14;'
        #9'.loc'#9'16'#9'216'#9'0'
        
          #9'ld.local.f32 '#9'%f16, [__cuda___cuda_local_var_86465_19_non_const' +
          '_vterm_168+4];'
        #9'mad.f32 '#9'%f17, %f7, %f16, %f2;'
        #9'.loc'#9'16'#9'217'#9'0'
        #9'cvt.rzi.s32.f32 '#9'%r37, %f17;'
        #9'cvt.rn.f32.s32 '#9'%f18, %r37;'
        #9'sub.f32 '#9'%f19, %f17, %f18;'
        #9'.loc'#9'16'#9'219'#9'0'
        #9'mov.f32 '#9'%f20, 0f3f800000;    '#9'// 1'
        #9'add.f32 '#9'%f21, %f19, %f20;'
        #9'cvt.rzi.s32.f32 '#9'%r38, %f21;'
        #9'cvt.rn.f32.s32 '#9'%f22, %r38;'
        #9'sub.f32 '#9'%f23, %f21, %f22;'
        #9'st.global.v2.f32 '#9'[%r23+0], {%f15,%f23};'
        '$Lt_4_4098:'
        ' //<loop> Part of loop body line 192, head labeled $Lt_4_3842'
        #9'.loc'#9'16'#9'221'#9'0'
        #9'add.s32 '#9'%r16, %r16, 1;'
        #9'.loc'#9'16'#9'192'#9'0'
        #9'ld.param.u32 '#9'%r5, [__cudaparm_advectParticles_k_lb];'
        #9'.loc'#9'16'#9'221'#9'0'
        #9'setp.ne.s32 '#9'%p4, %r16, %r5;'
        #9'@%p4 bra '#9'$Lt_4_3842;'
        '$Lt_4_3330:'
        '$Lt_4_2818:'
        #9'.loc'#9'16'#9'225'#9'0'
        #9'exit;'
        '$LDWend_advectParticles_k:'
        #9'} // advectParticles_k'
        '')
      Compiler = GLSCUDACompiler1
      object TextureOfVelocityField: TCUDATexture
        KernelName = 'texref'
        NormalizedCoord = False
        FilterMode = fmLinear
        Format = ctFloat
        ChannelNum = cnTwo
        MemDataArray = ArrayOfTexture
      end
      object addForces: TCUDAFunction
        KernelName = 'addForces_k'
        OnParameterSetup = addForcesParameterSetup
        object addForces_k_v: TCUDAFuncParam
          KernelName = 'v'
          DataType = float2
          Size = 0
          Reference = True
        end
        object addForces_k_dx: TCUDAFuncParam
          KernelName = 'dx'
          DataType = int1
          Size = 0
          Reference = False
        end
        object addForces_k_dy: TCUDAFuncParam
          KernelName = 'dy'
          DataType = int1
          Size = 0
          Reference = False
        end
        object addForces_k_spx: TCUDAFuncParam
          KernelName = 'spx'
          DataType = int1
          Size = 0
          Reference = False
        end
        object addForces_k_spy: TCUDAFuncParam
          KernelName = 'spy'
          DataType = int1
          Size = 0
          Reference = False
        end
        object addForces_k_fx: TCUDAFuncParam
          KernelName = 'fx'
          DataType = float1
          Size = 0
          Reference = False
        end
        object addForces_k_fy: TCUDAFuncParam
          KernelName = 'fy'
          DataType = float1
          Size = 0
          Reference = False
        end
        object addForces_k_r: TCUDAFuncParam
          KernelName = 'r'
          DataType = int1
          Size = 0
          Reference = False
        end
        object addForces_k_pitch: TCUDAFuncParam
          KernelName = 'pitch'
          DataType = customType
          CustomType = 'size_t'
          Size = 0
          Reference = False
        end
      end
      object advectVelocity: TCUDAFunction
        KernelName = 'advectVelocity_k'
        OnParameterSetup = advectVelocityParameterSetup
        object advectVelocity_k_vx: TCUDAFuncParam
          KernelName = 'vx'
          DataType = float1
          Size = 0
          Reference = True
        end
        object advectVelocity_k_vy: TCUDAFuncParam
          KernelName = 'vy'
          DataType = float1
          Size = 0
          Reference = True
        end
        object advectVelocity_k_dx: TCUDAFuncParam
          KernelName = 'dx'
          DataType = int1
          Size = 0
          Reference = False
        end
        object advectVelocity_k_pdx: TCUDAFuncParam
          KernelName = 'pdx'
          DataType = int1
          Size = 0
          Reference = False
        end
        object advectVelocity_k_dy: TCUDAFuncParam
          KernelName = 'dy'
          DataType = int1
          Size = 0
          Reference = False
        end
        object advectVelocity_k_dt: TCUDAFuncParam
          KernelName = 'dt'
          DataType = float1
          Size = 0
          Reference = False
        end
        object advectVelocity_k_lb: TCUDAFuncParam
          KernelName = 'lb'
          DataType = int1
          Size = 0
          Reference = False
        end
      end
      object diffuseProject: TCUDAFunction
        KernelName = 'diffuseProject_k'
        OnParameterSetup = diffuseProjectParameterSetup
        object diffuseProject_k_vx: TCUDAFuncParam
          KernelName = 'vx'
          DataType = float2
          Size = 0
          Reference = True
        end
        object diffuseProject_k_vy: TCUDAFuncParam
          KernelName = 'vy'
          DataType = float2
          Size = 0
          Reference = True
        end
        object diffuseProject_k_dx: TCUDAFuncParam
          KernelName = 'dx'
          DataType = int1
          Size = 0
          Reference = False
        end
        object diffuseProject_k_dy: TCUDAFuncParam
          KernelName = 'dy'
          DataType = int1
          Size = 0
          Reference = False
        end
        object diffuseProject_k_dt: TCUDAFuncParam
          KernelName = 'dt'
          DataType = float1
          Size = 0
          Reference = False
        end
        object diffuseProject_k_visc: TCUDAFuncParam
          KernelName = 'visc'
          DataType = float1
          Size = 0
          Reference = False
        end
        object diffuseProject_k_lb: TCUDAFuncParam
          KernelName = 'lb'
          DataType = int1
          Size = 0
          Reference = False
        end
      end
      object updateVelocity: TCUDAFunction
        KernelName = 'updateVelocity_k'
        OnParameterSetup = updateVelocityParameterSetup
        object updateVelocity_k_v: TCUDAFuncParam
          KernelName = 'v'
          DataType = float2
          Size = 0
          Reference = True
        end
        object updateVelocity_k_vx: TCUDAFuncParam
          KernelName = 'vx'
          DataType = float1
          Size = 0
          Reference = True
        end
        object updateVelocity_k_vy: TCUDAFuncParam
          KernelName = 'vy'
          DataType = float1
          Size = 0
          Reference = True
        end
        object updateVelocity_k_dx: TCUDAFuncParam
          KernelName = 'dx'
          DataType = int1
          Size = 0
          Reference = False
        end
        object updateVelocity_k_pdx: TCUDAFuncParam
          KernelName = 'pdx'
          DataType = int1
          Size = 0
          Reference = False
        end
        object updateVelocity_k_dy: TCUDAFuncParam
          KernelName = 'dy'
          DataType = int1
          Size = 0
          Reference = False
        end
        object updateVelocity_k_lb: TCUDAFuncParam
          KernelName = 'lb'
          DataType = int1
          Size = 0
          Reference = False
        end
        object updateVelocity_k_pitch: TCUDAFuncParam
          KernelName = 'pitch'
          DataType = customType
          CustomType = 'size_t'
          Size = 0
          Reference = False
        end
        object updateVelocity_k_scale: TCUDAFuncParam
          KernelName = 'scale'
          DataType = float1
          Size = 0
          Reference = False
        end
      end
      object advectParticles: TCUDAFunction
        KernelName = 'advectParticles_k'
        OnParameterSetup = advectParticlesParameterSetup
        object advectParticles_k_part: TCUDAFuncParam
          KernelName = 'part'
          DataType = float2
          Size = 0
          Reference = True
        end
        object advectParticles_k_v: TCUDAFuncParam
          KernelName = 'v'
          DataType = float2
          Size = 0
          Reference = True
        end
        object advectParticles_k_dx: TCUDAFuncParam
          KernelName = 'dx'
          DataType = int1
          Size = 0
          Reference = False
        end
        object advectParticles_k_dy: TCUDAFuncParam
          KernelName = 'dy'
          DataType = int1
          Size = 0
          Reference = False
        end
        object advectParticles_k_dt: TCUDAFuncParam
          KernelName = 'dt'
          DataType = float1
          Size = 0
          Reference = False
        end
        object advectParticles_k_lb: TCUDAFuncParam
          KernelName = 'lb'
          DataType = int1
          Size = 0
          Reference = False
        end
        object advectParticles_k_pitch: TCUDAFuncParam
          KernelName = 'pitch'
          DataType = customType
          CustomType = 'size_t'
          Size = 0
          Reference = False
        end
      end
    end
    object ArrayOfTexture: TCUDAMemData
      Width = 512
      Height = 512
      MemoryType = mtArray
      ChannelsType = ctFloat
      ChannelsNum = cnTwo
    end
    object VelocityField: TCUDAMemData
      Width = 512
      Height = 512
      MemoryType = mtDevice
      ChannelsType = ctFloat
      ChannelsNum = cnTwo
    end
    object ComplexVXField: TCUDAMemData
      Width = 131584
      MemoryType = mtDevice
      ChannelsType = ctFloat
      ChannelsNum = cnTwo
    end
    object ComplexVYField: TCUDAMemData
      Width = 262144
      MemoryType = mtDevice
      ChannelsType = ctFloat
      ChannelsNum = cnTwo
    end
    object InitialPosition: TCUDAMemData
      Width = 512
      Height = 512
      ChannelsType = ctFloat
      ChannelsNum = cnTwo
    end
    object ForwardFFT: TCUDAFFTPlan
      Width = 512
      Height = 512
    end
    object InverseFFT: TCUDAFFTPlan
      Width = 512
      Height = 512
      Transform = fftComplexToReal
    end
    object ParticleMapper: TCUDAGLGeometryResource
      FeedBackMesh = ParticleRenderer
      Mapping = grmWriteDiscard
    end
  end
  object GLSCUDACompiler1: TGLSCUDACompiler
    NVCCPath = 'C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v3.2\\bin\'
    CppCompilerPath = 'C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\bin\'
    ProjectModule = 'Fluids kernels.cu'
    Left = 448
    Top = 160
  end
  object FluidShader: TGLSLShader
    FragmentProgram.Code.Strings = (
      '#version 330'
      'precision highp float;'
      'out vec4 FragColor;'
      'void main (void)'
      '{'
      '    FragColor = vec4(0.0, 1.0, 0.0, 0.5);'
      '}')
    FragmentProgram.Enabled = True
    VertexProgram.Code.Strings = (
      '#version 330'
      'in vec2 Position;'
      'void main(void)'
      '{'
      '    vec2 FlipPos = vec2(Position.x, 1.0 - Position.y);'
      '    gl_Position = vec4(2.0*FlipPos - 1.0, 0.0, 1.0);'
      '}')
    VertexProgram.Enabled = True
    OnApply = FluidShaderApply
    FailedInitAction = fiaSilentDisable
    Left = 40
    Top = 200
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'FluidMaterial'
        Tag = 0
        Material.BlendingMode = bmTransparency
        Shader = FluidShader
      end>
    Left = 40
    Top = 152
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Left = 40
    Top = 256
  end
  object GLGuiLayout1: TGLGuiLayout
    BitmapFont = GLWindowsBitmapFont1
    GuiComponents = <>
    Left = 40
    Top = 304
  end
end
