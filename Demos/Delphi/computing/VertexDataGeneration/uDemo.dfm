object Form1: TForm1
  Left = 423
  Top = 62
  Caption = 'CUDA fit GLScene'
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
    Buffer.BackgroundColor = clBlack
    FieldOfView = 157.897079467773400000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 16
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000C03F0000C03F000000400000803F}
    end
    object GLDummyCube1: TGLDummyCube
      CubeSize = 2.000000000000000000
      VisibleAtRunTime = True
      object GLFeedBackMesh1: TGLFeedBackMesh
        Attributes = <
          item
            Name = 'Position'
            GLSLType = GLSLType4F
          end>
        Shader = GLSLShader1
        CommonKernelFunction = MakeDotField
        Visible = False
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 96
    Top = 16
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'Form1 - %FPS'
    KeyCombinations = <
      item
        ShiftState = [ssLeft, ssRight]
        Action = snaZoom
      end
      item
        ShiftState = [ssLeft]
        Action = snaMoveAroundTarget
      end
      item
        ShiftState = [ssRight]
        Action = snaMoveAroundTarget
      end>
    Left = 24
    Top = 72
  end
  object GLSCUDADevice1: TGLSCUDADevice
    SelectDevice = 'GeForce GTX 260 (1)'
    Left = 448
    Top = 16
  end
  object GLSCUDA1: TGLSCUDA
    ComputingDevice = GLSCUDADevice1
    OnOpenGLInteropInit = GLSCUDA1OpenGLInteropInit
    Left = 448
    Top = 72
    object MainModule: TCUDAModule
      Code.Strings = (
        #9'.version 1.4'
        #9'.target sm_13'
        
          #9'// compiled with C:\Program Files\NVIDIA GPU Computing Toolkit\' +
          'CUDA\v3.2\bin/../open64/lib//be.exe'
        #9'// nvopencc 3.2 built on 2010-11-06'
        ''
        #9'//-----------------------------------------------------------'
        
          #9'// Compiling C:/Users/YARUNA~1/AppData/Local/Temp/tmpxft_00000a' +
          'b0_00000000-11_temp.cpp3.i (C:/Users/YARUNA~1/AppData/Local/Temp' +
          '/ccBI#.a02728)'
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
        
          #9'.file'#9'1'#9'"C:/Users/YARUNA~1/AppData/Local/Temp/tmpxft_00000ab0_0' +
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
        
          #9'.file'#9'16'#9'"c:\program files\nvidia gpu computing toolkit\cuda\v3' +
          '.2\include\texture_fetch_functions.h"'
        
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
          '.2\include\math_functions_dbl_ptx3.h"'
        #9'.file'#9'28'#9'"C:/Users/YARUNA~1/AppData/Local/Temp/temp.cu"'
        ''
        
          #9'.const .align 4 .b8 __cudart_i2opi_f[24] = {65,144,67,60,153,14' +
          '9,98,219,192,221,52,245,209,87,39,252,41,21,68,78,110,131,249,16' +
          '2};'
        ''
        #9'.entry _Z6kernelP6float4jjf ('
        #9#9'.param .u32 __cudaparm__Z6kernelP6float4jjf_pos,'
        #9#9'.param .u32 __cudaparm__Z6kernelP6float4jjf_width,'
        #9#9'.param .u32 __cudaparm__Z6kernelP6float4jjf_height,'
        #9#9'.param .f32 __cudaparm__Z6kernelP6float4jjf_time)'
        #9'{'
        #9'.reg .u16 %rh<6>;'
        #9'.reg .u32 %r<180>;'
        #9'.reg .f32 %f<92>;'
        #9'.reg .pred %p<26>;'
        #9'.local .align 4 .b8 __cuda_result_16[28];'
        #9'.local .align 4 .b8 __cuda_result_44[28];'
        #9'.loc'#9'28'#9'9'#9'0'
        '$LDWbegin__Z6kernelP6float4jjf:'
        #9'.loc'#9'18'#9'1638'#9'0'
        #9'mov.u16 '#9'%rh1, %ctaid.x;'
        #9'mov.u16 '#9'%rh2, %ntid.x;'
        #9'mul.wide.u16 '#9'%r1, %rh1, %rh2;'
        #9'ld.param.u32 '#9'%r2, [__cudaparm__Z6kernelP6float4jjf_width];'
        #9'cvt.rn.f32.u32 '#9'%f1, %r2;'
        #9'cvt.u32.u16 '#9'%r3, %tid.x;'
        #9'add.u32 '#9'%r4, %r3, %r1;'
        #9'cvt.rn.f32.u32 '#9'%f2, %r4;'
        #9'div.full.f32 '#9'%f3, %f2, %f1;'
        #9'add.f32 '#9'%f4, %f3, %f3;'
        #9'mov.f32 '#9'%f5, 0fbf800000;     '#9'// -1'
        #9'add.f32 '#9'%f6, %f4, %f5;'
        #9'ld.param.f32 '#9'%f7, [__cudaparm__Z6kernelP6float4jjf_time];'
        #9'mov.f32 '#9'%f8, 0f40800000;     '#9'// 4'
        #9'mad.f32 '#9'%f9, %f8, %f6, %f7;'
        #9'abs.f32 '#9'%f10, %f9;'
        #9'mov.f32 '#9'%f11, 0f00000000;    '#9'// 0'
        #9'set.eq.u32.f32 '#9'%r5, %f9, %f11;'
        #9'neg.s32 '#9'%r6, %r5;'
        #9'mov.f32 '#9'%f12, 0f7f800000;    '#9'// 1.#INF'
        #9'set.eq.u32.f32 '#9'%r7, %f10, %f12;'
        #9'neg.s32 '#9'%r8, %r7;'
        #9'or.b32 '#9'%r9, %r6, %r8;'
        #9'mov.u32 '#9'%r10, 0;'
        #9'setp.eq.s32 '#9'%p1, %r9, %r10;'
        #9'@%p1 bra '#9'$Lt_0_23554;'
        #9'.loc'#9'18'#9'1639'#9'0'
        #9'mov.f32 '#9'%f13, 0f00000000;    '#9'// 0'
        #9'mul.rn.f32 '#9'%f14, %f9, %f13;'
        #9'mov.u32 '#9'%r11, __cudart_i2opi_f;'
        #9'bra.uni '#9'$LDWendi___isinff_204_5;'
        '$Lt_0_23554:'
        #9'mov.f32 '#9'%f15, 0f473ba700;    '#9'// 48039'
        #9'setp.gt.f32 '#9'%p2, %f10, %f15;'
        #9'@!%p2 bra '#9'$Lt_0_24066;'
        #9'.loc'#9'18'#9'1396'#9'0'
        #9'mov.u32 '#9'%r11, __cudart_i2opi_f;'
        #9'mov.b32 '#9'%r12, %f9;'
        #9'and.b32 '#9'%r13, %r12, -2147483648;'
        #9'mov.s32 '#9'%r14, %r13;'
        #9'.loc'#9'18'#9'1405'#9'0'
        #9'shl.b32 '#9'%r15, %r12, 1;'
        #9'shr.u32 '#9'%r16, %r15, 24;'
        #9'sub.u32 '#9'%r17, %r16, 128;'
        #9'shr.u32 '#9'%r18, %r17, 5;'
        #9'mov.s32 '#9'%r19, 4;'
        #9'sub.s32 '#9'%r20, %r19, %r18;'
        #9'.loc'#9'18'#9'24'#9'0'
        #9'mov.s32 '#9'%r21, %r11;'
        #9'add.u32 '#9'%r22, %r11, 24;'
        #9'mov.u32 '#9'%r23, __cuda_result_16;'
        #9'shl.b32 '#9'%r24, %r12, 8;'
        #9'or.b32 '#9'%r25, %r24, -2147483648;'
        #9'mov.u32 '#9'%r26, 0;'
        '$Lt_0_25090:'
        ' //<loop> Loop body line 24, nesting depth: 1, iterations: 6'
        #9'.loc'#9'18'#9'1411'#9'0'
        #9'ld.const.u32 '#9'%r27, [%r21+0];'
        #9'mul.lo.u32 '#9'%r28, %r27, %r25;'
        #9'add.u32 '#9'%r29, %r28, %r26;'
        #9'.loc'#9'18'#9'1412'#9'0'
        #9'set.gt.u32.u32 '#9'%r30, %r28, %r29;'
        #9'neg.s32 '#9'%r31, %r30;'
        #9'mul.hi.u32 '#9'%r32, %r27, %r25;'
        #9'add.u32 '#9'%r26, %r31, %r32;'
        #9'.loc'#9'18'#9'1413'#9'0'
        #9'st.local.u32 '#9'[%r23+0], %r29;'
        #9'add.u32 '#9'%r23, %r23, 4;'
        #9'add.u32 '#9'%r21, %r21, 4;'
        #9'setp.ne.u32 '#9'%p3, %r21, %r22;'
        #9'@%p3 bra '#9'$Lt_0_25090;'
        #9'.loc'#9'18'#9'1415'#9'0'
        #9'st.local.u32 '#9'[__cuda_result_16+24], %r26;'
        #9'.loc'#9'18'#9'1420'#9'0'
        #9'mul.lo.u32 '#9'%r33, %r20, 4;'
        #9'mov.u32 '#9'%r34, __cuda_result_16;'
        #9'add.u32 '#9'%r35, %r33, %r34;'
        #9'ld.local.u32 '#9'%r26, [%r35+8];'
        #9'.loc'#9'18'#9'1421'#9'0'
        #9'ld.local.u32 '#9'%r36, [%r35+4];'
        #9'and.b32 '#9'%r37, %r17, 31;'
        #9'mov.u32 '#9'%r38, 0;'
        #9'setp.eq.u32 '#9'%p4, %r37, %r38;'
        #9'@%p4 bra '#9'$Lt_0_25602;'
        #9'.loc'#9'18'#9'1423'#9'0'
        #9'mov.s32 '#9'%r39, 32;'
        #9'sub.s32 '#9'%r40, %r39, %r37;'
        #9'.loc'#9'18'#9'1424'#9'0'
        #9'shr.u32 '#9'%r41, %r36, %r40;'
        #9'shl.b32 '#9'%r42, %r26, %r37;'
        #9'add.u32 '#9'%r26, %r41, %r42;'
        #9'.loc'#9'18'#9'1425'#9'0'
        #9'ld.local.u32 '#9'%r43, [%r35+0];'
        #9'shr.u32 '#9'%r44, %r43, %r40;'
        #9'shl.b32 '#9'%r45, %r36, %r37;'
        #9'add.u32 '#9'%r36, %r44, %r45;'
        '$Lt_0_25602:'
        #9'.loc'#9'18'#9'1427'#9'0'
        #9'shr.u32 '#9'%r40, %r26, 30;'
        #9'.loc'#9'18'#9'1429'#9'0'
        #9'shr.u32 '#9'%r46, %r36, 30;'
        #9'shl.b32 '#9'%r47, %r26, 2;'
        #9'add.u32 '#9'%r26, %r46, %r47;'
        #9'.loc'#9'18'#9'1430'#9'0'
        #9'shl.b32 '#9'%r36, %r36, 2;'
        #9'mov.u32 '#9'%r48, 0;'
        #9'setp.eq.u32 '#9'%p5, %r36, %r48;'
        #9'@%p5 bra '#9'$Lt_0_26370;'
        #9'.loc'#9'18'#9'1431'#9'0'
        #9'add.u32 '#9'%r49, %r26, 1;'
        #9'mov.u32 '#9'%r50, -2147483648;'
        #9'set.gt.u32.u32 '#9'%r51, %r49, %r50;'
        #9'neg.s32 '#9'%r52, %r51;'
        #9'bra.uni '#9'$Lt_0_26114;'
        '$Lt_0_26370:'
        #9'mov.u32 '#9'%r53, -2147483648;'
        #9'set.gt.u32.u32 '#9'%r54, %r26, %r53;'
        #9'neg.s32 '#9'%r52, %r54;'
        '$Lt_0_26114:'
        #9'.loc'#9'18'#9'1432'#9'0'
        #9'add.u32 '#9'%r55, %r40, %r52;'
        #9'.loc'#9'18'#9'1431'#9'0'
        #9'neg.s32 '#9'%r56, %r55;'
        #9'mov.u32 '#9'%r57, 0;'
        #9'setp.ne.u32 '#9'%p6, %r13, %r57;'
        #9'selp.s32 '#9'%r40, %r56, %r55, %p6;'
        #9'mov.u32 '#9'%r58, 0;'
        #9'setp.eq.u32 '#9'%p7, %r52, %r58;'
        #9'@%p7 bra '#9'$Lt_0_26626;'
        #9'.loc'#9'18'#9'1437'#9'0'
        #9'neg.s32 '#9'%r36, %r36;'
        #9'.loc'#9'18'#9'1439'#9'0'
        #9'mov.u32 '#9'%r59, 0;'
        #9'set.eq.u32.u32 '#9'%r60, %r36, %r59;'
        #9'neg.s32 '#9'%r61, %r60;'
        #9'not.b32 '#9'%r62, %r26;'
        #9'add.u32 '#9'%r26, %r61, %r62;'
        #9'.loc'#9'18'#9'1440'#9'0'
        #9'xor.b32 '#9'%r14, %r13, -2147483648;'
        '$Lt_0_26626:'
        #9'.loc'#9'18'#9'1442'#9'0'
        #9'mov.s32 '#9'%r63, %r40;'
        #9'mov.u32 '#9'%r64, 0;'
        #9'setp.le.s32 '#9'%p8, %r26, %r64;'
        #9'@%p8 bra '#9'$Lt_0_34818;'
        #9'mov.u32 '#9'%r65, 0;'
        '$Lt_0_27650:'
        
          ' //<loop> Loop body line 1442, nesting depth: 1, estimated itera' +
          'tions: unknown'
        #9'.loc'#9'18'#9'1446'#9'0'
        #9'shr.u32 '#9'%r66, %r36, 31;'
        #9'shl.b32 '#9'%r67, %r26, 1;'
        #9'add.u32 '#9'%r26, %r66, %r67;'
        #9'.loc'#9'18'#9'1447'#9'0'
        #9'shl.b32 '#9'%r36, %r36, 1;'
        #9'.loc'#9'18'#9'1448'#9'0'
        #9'sub.u32 '#9'%r65, %r65, 1;'
        #9'mov.u32 '#9'%r68, 0;'
        #9'setp.gt.s32 '#9'%p9, %r26, %r68;'
        #9'@%p9 bra '#9'$Lt_0_27650;'
        #9'bra.uni '#9'$Lt_0_27138;'
        '$Lt_0_34818:'
        #9'mov.u32 '#9'%r65, 0;'
        '$Lt_0_27138:'
        #9'.loc'#9'18'#9'1450'#9'0'
        #9'mul.lo.u32 '#9'%r36, %r26, -921707870;'
        #9'.loc'#9'18'#9'1451'#9'0'
        #9'mov.u32 '#9'%r69, -921707870;'
        #9'mul.hi.u32 '#9'%r26, %r26, %r69;'
        #9'mov.u32 '#9'%r70, 0;'
        #9'setp.le.s32 '#9'%p10, %r26, %r70;'
        #9'@%p10 bra '#9'$Lt_0_28162;'
        #9'.loc'#9'18'#9'1453'#9'0'
        #9'shr.u32 '#9'%r71, %r36, 31;'
        #9'shl.b32 '#9'%r72, %r26, 1;'
        #9'add.u32 '#9'%r26, %r71, %r72;'
        #9'.loc'#9'18'#9'1454'#9'0'
        #9'shl.b32 '#9'%r36, %r36, 1;'
        #9'.loc'#9'18'#9'1455'#9'0'
        #9'sub.u32 '#9'%r65, %r65, 1;'
        '$Lt_0_28162:'
        #9'.loc'#9'18'#9'1457'#9'0'
        #9'mov.u32 '#9'%r73, 0;'
        #9'set.ne.u32.u32 '#9'%r74, %r36, %r73;'
        #9'neg.s32 '#9'%r75, %r74;'
        #9'add.u32 '#9'%r26, %r75, %r26;'
        #9'.loc'#9'18'#9'1459'#9'0'
        #9'shl.b32 '#9'%r76, %r26, 24;'
        #9'mov.s32 '#9'%r77, 0;'
        #9'set.lt.u32.s32 '#9'%r78, %r76, %r77;'
        #9'neg.s32 '#9'%r79, %r78;'
        #9'shr.u32 '#9'%r80, %r26, 8;'
        #9'add.u32 '#9'%r81, %r65, 126;'
        #9'shl.b32 '#9'%r82, %r81, 23;'
        #9'add.u32 '#9'%r83, %r80, %r82;'
        #9'add.u32 '#9'%r84, %r79, %r83;'
        #9'or.b32 '#9'%r85, %r14, %r84;'
        #9'mov.b32 '#9'%f16, %r85;'
        #9'bra.uni '#9'$LDWendi___internal_fmad_204_6;'
        '$Lt_0_24066:'
        #9'.loc'#9'18'#9'1463'#9'0'
        #9'mov.f32 '#9'%f17, 0f3f22f983;    '#9'// 0.63662'
        #9'mul.f32 '#9'%f18, %f9, %f17;'
        #9'cvt.rni.s32.f32 '#9'%r86, %f18;'
        #9'cvt.rn.f32.s32 '#9'%f19, %r86;'
        #9'neg.f32 '#9'%f20, %f19;'
        #9'.loc'#9'18'#9'1472'#9'0'
        #9'mov.s32 '#9'%r63, %r86;'
        #9'.loc'#9'18'#9'1473'#9'0'
        #9'mov.f32 '#9'%f21, 0f3fc90000;    '#9'// 1.57031'
        #9'mad.f32 '#9'%f22, %f21, %f20, %f9;'
        #9'mov.f32 '#9'%f23, 0f39fd8000;    '#9'// 0.000483513'
        #9'mad.f32 '#9'%f24, %f23, %f20, %f22;'
        #9'mov.f32 '#9'%f25, 0f34a88000;    '#9'// 3.13856e-007'
        #9'mad.f32 '#9'%f26, %f25, %f20, %f24;'
        #9'mov.f32 '#9'%f27, 0f2e85a309;    '#9'// 6.0771e-011'
        #9'mad.f32 '#9'%f16, %f27, %f20, %f26;'
        #9'mov.u32 '#9'%r11, __cudart_i2opi_f;'
        '$LDWendi___internal_fmad_204_6:'
        #9'.loc'#9'18'#9'1641'#9'0'
        #9'mul.f32 '#9'%f28, %f16, %f16;'
        #9'and.b32 '#9'%r87, %r63, 1;'
        #9'mov.u32 '#9'%r88, 0;'
        #9'setp.eq.s32 '#9'%p11, %r87, %r88;'
        #9'@%p11 bra '#9'$Lt_0_28930;'
        #9'.loc'#9'18'#9'1644'#9'0'
        #9'mov.f32 '#9'%f29, 0f3f800000;    '#9'// 1'
        #9'mov.f32 '#9'%f30, 0fbf000000;    '#9'// -0.5'
        #9'mov.f32 '#9'%f31, 0f3d2aaaa5;    '#9'// 0.0416666'
        #9'mov.f32 '#9'%f32, 0fbab6061a;    '#9'// -0.00138873'
        #9'mov.f32 '#9'%f33, 0f37ccf5ce;    '#9'// 2.44332e-005'
        #9'mad.f32 '#9'%f34, %f33, %f28, %f32;'
        #9'mad.f32 '#9'%f35, %f28, %f34, %f31;'
        #9'mad.f32 '#9'%f36, %f28, %f35, %f30;'
        #9'mad.f32 '#9'%f37, %f28, %f36, %f29;'
        #9'bra.uni '#9'$Lt_0_28674;'
        '$Lt_0_28930:'
        #9'.loc'#9'18'#9'1646'#9'0'
        #9'mov.f32 '#9'%f38, 0fbe2aaaa3;    '#9'// -0.166667'
        #9'mov.f32 '#9'%f39, 0f3c08839e;    '#9'// 0.00833216'
        #9'mov.f32 '#9'%f40, 0fb94ca1f9;    '#9'// -0.000195153'
        #9'mad.f32 '#9'%f41, %f40, %f28, %f39;'
        #9'mad.f32 '#9'%f42, %f28, %f41, %f38;'
        #9'mul.f32 '#9'%f43, %f28, %f42;'
        #9'mad.f32 '#9'%f37, %f43, %f16, %f16;'
        '$Lt_0_28674:'
        #9'.loc'#9'18'#9'1648'#9'0'
        #9'neg.f32 '#9'%f44, %f37;'
        #9'and.b32 '#9'%r89, %r63, 2;'
        #9'mov.s32 '#9'%r90, 0;'
        #9'setp.ne.s32 '#9'%p12, %r89, %r90;'
        #9'selp.f32 '#9'%f37, %f44, %f37, %p12;'
        #9'.loc'#9'18'#9'1651'#9'0'
        #9'mov.f32 '#9'%f14, %f37;'
        '$LDWendi___isinff_204_5:'
        #9'.loc'#9'18'#9'1702'#9'0'
        #9'mov.u16 '#9'%rh3, %ctaid.y;'
        #9'mov.u16 '#9'%rh4, %ntid.y;'
        #9'mul.wide.u16 '#9'%r91, %rh3, %rh4;'
        #9'ld.param.u32 '#9'%r92, [__cudaparm__Z6kernelP6float4jjf_height];'
        #9'cvt.rn.f32.u32 '#9'%f45, %r92;'
        #9'cvt.u32.u16 '#9'%r93, %tid.y;'
        #9'add.u32 '#9'%r94, %r93, %r91;'
        #9'cvt.rn.f32.u32 '#9'%f46, %r94;'
        #9'div.full.f32 '#9'%f47, %f46, %f45;'
        #9'add.f32 '#9'%f48, %f47, %f47;'
        #9'mov.f32 '#9'%f49, 0fbf800000;    '#9'// -1'
        #9'add.f32 '#9'%f50, %f48, %f49;'
        #9'mov.f32 '#9'%f51, 0f40800000;    '#9'// 4'
        #9'.loc'#9'18'#9'1638'#9'0'
        #9'ld.param.f32 '#9'%f7, [__cudaparm__Z6kernelP6float4jjf_time];'
        #9'.loc'#9'18'#9'1702'#9'0'
        #9'mad.f32 '#9'%f52, %f51, %f50, %f7;'
        #9'abs.f32 '#9'%f53, %f52;'
        #9'mov.f32 '#9'%f54, 0f7f800000;    '#9'// 1.#INF'
        #9'setp.eq.f32 '#9'%p13, %f53, %f54;'
        #9'@!%p13 bra '#9'$Lt_0_29186;'
        #9'.loc'#9'18'#9'1703'#9'0'
        #9'neg.f32 '#9'%f55, %f52;'
        #9'add.rn.f32 '#9'%f56, %f52, %f55;'
        #9'bra.uni '#9'$LDWendi___isinff_204_1;'
        '$Lt_0_29186:'
        #9'mov.f32 '#9'%f57, 0f473ba700;    '#9'// 48039'
        #9'setp.gt.f32 '#9'%p14, %f53, %f57;'
        #9'@!%p14 bra '#9'$Lt_0_29698;'
        #9'.loc'#9'18'#9'1396'#9'0'
        #9'mov.b32 '#9'%r95, %f52;'
        #9'and.b32 '#9'%r96, %r95, -2147483648;'
        #9'mov.s32 '#9'%r97, %r96;'
        #9'.loc'#9'18'#9'1405'#9'0'
        #9'shl.b32 '#9'%r98, %r95, 1;'
        #9'shr.u32 '#9'%r99, %r98, 24;'
        #9'sub.u32 '#9'%r100, %r99, 128;'
        #9'shr.u32 '#9'%r101, %r100, 5;'
        #9'mov.s32 '#9'%r102, 4;'
        #9'sub.s32 '#9'%r103, %r102, %r101;'
        #9'.loc'#9'18'#9'24'#9'0'
        #9'mov.s32 '#9'%r104, %r11;'
        #9'add.u32 '#9'%r22, %r11, 24;'
        #9'mov.u32 '#9'%r105, __cuda_result_44;'
        #9'shl.b32 '#9'%r106, %r95, 8;'
        #9'or.b32 '#9'%r107, %r106, -2147483648;'
        #9'mov.u32 '#9'%r108, 0;'
        '$Lt_0_30722:'
        ' //<loop> Loop body line 24, nesting depth: 1, iterations: 6'
        #9'.loc'#9'18'#9'1411'#9'0'
        #9'ld.const.u32 '#9'%r109, [%r104+0];'
        #9'mul.lo.u32 '#9'%r110, %r109, %r107;'
        #9'add.u32 '#9'%r111, %r110, %r108;'
        #9'.loc'#9'18'#9'1412'#9'0'
        #9'set.gt.u32.u32 '#9'%r112, %r110, %r111;'
        #9'neg.s32 '#9'%r113, %r112;'
        #9'mul.hi.u32 '#9'%r114, %r109, %r107;'
        #9'add.u32 '#9'%r108, %r113, %r114;'
        #9'.loc'#9'18'#9'1413'#9'0'
        #9'st.local.u32 '#9'[%r105+0], %r111;'
        #9'add.u32 '#9'%r105, %r105, 4;'
        #9'add.u32 '#9'%r104, %r104, 4;'
        #9'setp.ne.u32 '#9'%p15, %r104, %r22;'
        #9'@%p15 bra '#9'$Lt_0_30722;'
        #9'.loc'#9'18'#9'1415'#9'0'
        #9'st.local.u32 '#9'[__cuda_result_44+24], %r108;'
        #9'.loc'#9'18'#9'1420'#9'0'
        #9'mul.lo.u32 '#9'%r115, %r103, 4;'
        #9'mov.u32 '#9'%r116, __cuda_result_44;'
        #9'add.u32 '#9'%r117, %r115, %r116;'
        #9'ld.local.u32 '#9'%r108, [%r117+8];'
        #9'.loc'#9'18'#9'1421'#9'0'
        #9'ld.local.u32 '#9'%r118, [%r117+4];'
        #9'and.b32 '#9'%r119, %r100, 31;'
        #9'mov.u32 '#9'%r120, 0;'
        #9'setp.eq.u32 '#9'%p16, %r119, %r120;'
        #9'@%p16 bra '#9'$Lt_0_31234;'
        #9'.loc'#9'18'#9'1423'#9'0'
        #9'mov.s32 '#9'%r121, 32;'
        #9'sub.s32 '#9'%r122, %r121, %r119;'
        #9'.loc'#9'18'#9'1424'#9'0'
        #9'shr.u32 '#9'%r123, %r118, %r122;'
        #9'shl.b32 '#9'%r124, %r108, %r119;'
        #9'add.u32 '#9'%r108, %r123, %r124;'
        #9'.loc'#9'18'#9'1425'#9'0'
        #9'ld.local.u32 '#9'%r125, [%r117+0];'
        #9'shr.u32 '#9'%r126, %r125, %r122;'
        #9'shl.b32 '#9'%r127, %r118, %r119;'
        #9'add.u32 '#9'%r118, %r126, %r127;'
        '$Lt_0_31234:'
        #9'.loc'#9'18'#9'1427'#9'0'
        #9'shr.u32 '#9'%r122, %r108, 30;'
        #9'.loc'#9'18'#9'1429'#9'0'
        #9'shr.u32 '#9'%r128, %r118, 30;'
        #9'shl.b32 '#9'%r129, %r108, 2;'
        #9'add.u32 '#9'%r108, %r128, %r129;'
        #9'.loc'#9'18'#9'1430'#9'0'
        #9'shl.b32 '#9'%r118, %r118, 2;'
        #9'mov.u32 '#9'%r130, 0;'
        #9'setp.eq.u32 '#9'%p17, %r118, %r130;'
        #9'@%p17 bra '#9'$Lt_0_32002;'
        #9'.loc'#9'18'#9'1431'#9'0'
        #9'add.u32 '#9'%r131, %r108, 1;'
        #9'mov.u32 '#9'%r132, -2147483648;'
        #9'set.gt.u32.u32 '#9'%r133, %r131, %r132;'
        #9'neg.s32 '#9'%r134, %r133;'
        #9'bra.uni '#9'$Lt_0_31746;'
        '$Lt_0_32002:'
        #9'mov.u32 '#9'%r135, -2147483648;'
        #9'set.gt.u32.u32 '#9'%r136, %r108, %r135;'
        #9'neg.s32 '#9'%r134, %r136;'
        '$Lt_0_31746:'
        #9'.loc'#9'18'#9'1432'#9'0'
        #9'add.u32 '#9'%r137, %r122, %r134;'
        #9'.loc'#9'18'#9'1431'#9'0'
        #9'neg.s32 '#9'%r138, %r137;'
        #9'mov.u32 '#9'%r139, 0;'
        #9'setp.ne.u32 '#9'%p18, %r96, %r139;'
        #9'selp.s32 '#9'%r122, %r138, %r137, %p18;'
        #9'mov.u32 '#9'%r140, 0;'
        #9'setp.eq.u32 '#9'%p19, %r134, %r140;'
        #9'@%p19 bra '#9'$Lt_0_32258;'
        #9'.loc'#9'18'#9'1437'#9'0'
        #9'neg.s32 '#9'%r118, %r118;'
        #9'.loc'#9'18'#9'1439'#9'0'
        #9'mov.u32 '#9'%r141, 0;'
        #9'set.eq.u32.u32 '#9'%r142, %r118, %r141;'
        #9'neg.s32 '#9'%r143, %r142;'
        #9'not.b32 '#9'%r144, %r108;'
        #9'add.u32 '#9'%r108, %r143, %r144;'
        #9'.loc'#9'18'#9'1440'#9'0'
        #9'xor.b32 '#9'%r97, %r96, -2147483648;'
        '$Lt_0_32258:'
        #9'.loc'#9'18'#9'1442'#9'0'
        #9'mov.s32 '#9'%r145, %r122;'
        #9'mov.u32 '#9'%r146, 0;'
        #9'setp.le.s32 '#9'%p20, %r108, %r146;'
        #9'@%p20 bra '#9'$Lt_0_35074;'
        #9'mov.u32 '#9'%r147, 0;'
        '$Lt_0_33282:'
        
          ' //<loop> Loop body line 1442, nesting depth: 1, estimated itera' +
          'tions: unknown'
        #9'.loc'#9'18'#9'1446'#9'0'
        #9'shr.u32 '#9'%r148, %r118, 31;'
        #9'shl.b32 '#9'%r149, %r108, 1;'
        #9'add.u32 '#9'%r108, %r148, %r149;'
        #9'.loc'#9'18'#9'1447'#9'0'
        #9'shl.b32 '#9'%r118, %r118, 1;'
        #9'.loc'#9'18'#9'1448'#9'0'
        #9'sub.u32 '#9'%r147, %r147, 1;'
        #9'mov.u32 '#9'%r150, 0;'
        #9'setp.gt.s32 '#9'%p21, %r108, %r150;'
        #9'@%p21 bra '#9'$Lt_0_33282;'
        #9'bra.uni '#9'$Lt_0_32770;'
        '$Lt_0_35074:'
        #9'mov.u32 '#9'%r147, 0;'
        '$Lt_0_32770:'
        #9'.loc'#9'18'#9'1450'#9'0'
        #9'mul.lo.u32 '#9'%r118, %r108, -921707870;'
        #9'.loc'#9'18'#9'1451'#9'0'
        #9'mov.u32 '#9'%r151, -921707870;'
        #9'mul.hi.u32 '#9'%r108, %r108, %r151;'
        #9'mov.u32 '#9'%r152, 0;'
        #9'setp.le.s32 '#9'%p22, %r108, %r152;'
        #9'@%p22 bra '#9'$Lt_0_33794;'
        #9'.loc'#9'18'#9'1453'#9'0'
        #9'shr.u32 '#9'%r153, %r118, 31;'
        #9'shl.b32 '#9'%r154, %r108, 1;'
        #9'add.u32 '#9'%r108, %r153, %r154;'
        #9'.loc'#9'18'#9'1454'#9'0'
        #9'shl.b32 '#9'%r118, %r118, 1;'
        #9'.loc'#9'18'#9'1455'#9'0'
        #9'sub.u32 '#9'%r147, %r147, 1;'
        '$Lt_0_33794:'
        #9'.loc'#9'18'#9'1457'#9'0'
        #9'mov.u32 '#9'%r155, 0;'
        #9'set.ne.u32.u32 '#9'%r156, %r118, %r155;'
        #9'neg.s32 '#9'%r157, %r156;'
        #9'add.u32 '#9'%r108, %r157, %r108;'
        #9'.loc'#9'18'#9'1459'#9'0'
        #9'shl.b32 '#9'%r158, %r108, 24;'
        #9'mov.s32 '#9'%r159, 0;'
        #9'set.lt.u32.s32 '#9'%r160, %r158, %r159;'
        #9'neg.s32 '#9'%r161, %r160;'
        #9'shr.u32 '#9'%r162, %r108, 8;'
        #9'add.u32 '#9'%r163, %r147, 126;'
        #9'shl.b32 '#9'%r164, %r163, 23;'
        #9'add.u32 '#9'%r165, %r162, %r164;'
        #9'add.u32 '#9'%r166, %r161, %r165;'
        #9'or.b32 '#9'%r167, %r97, %r166;'
        #9'mov.b32 '#9'%f58, %r167;'
        #9'bra.uni '#9'$LDWendi___internal_fmad_204_2;'
        '$Lt_0_29698:'
        #9'.loc'#9'18'#9'1463'#9'0'
        #9'mov.f32 '#9'%f59, 0f3f22f983;    '#9'// 0.63662'
        #9'mul.f32 '#9'%f60, %f52, %f59;'
        #9'cvt.rni.s32.f32 '#9'%r168, %f60;'
        #9'cvt.rn.f32.s32 '#9'%f61, %r168;'
        #9'neg.f32 '#9'%f62, %f61;'
        #9'.loc'#9'18'#9'1472'#9'0'
        #9'mov.s32 '#9'%r145, %r168;'
        #9'.loc'#9'18'#9'1473'#9'0'
        #9'mov.f32 '#9'%f63, 0f3fc90000;    '#9'// 1.57031'
        #9'mad.f32 '#9'%f64, %f63, %f62, %f52;'
        #9'mov.f32 '#9'%f65, 0f39fd8000;    '#9'// 0.000483513'
        #9'mad.f32 '#9'%f66, %f65, %f62, %f64;'
        #9'mov.f32 '#9'%f67, 0f34a88000;    '#9'// 3.13856e-007'
        #9'mad.f32 '#9'%f68, %f67, %f62, %f66;'
        #9'mov.f32 '#9'%f69, 0f2e85a309;    '#9'// 6.0771e-011'
        #9'mad.f32 '#9'%f58, %f69, %f62, %f68;'
        '$LDWendi___internal_fmad_204_2:'
        #9'.loc'#9'18'#9'1705'#9'0'
        #9'add.s32 '#9'%r169, %r145, 1;'
        #9'mul.f32 '#9'%f70, %f58, %f58;'
        #9'and.b32 '#9'%r170, %r169, 1;'
        #9'mov.u32 '#9'%r171, 0;'
        #9'setp.eq.s32 '#9'%p23, %r170, %r171;'
        #9'@%p23 bra '#9'$Lt_0_34562;'
        #9'.loc'#9'18'#9'1709'#9'0'
        #9'mov.f32 '#9'%f71, 0f3f800000;    '#9'// 1'
        #9'mov.f32 '#9'%f72, 0fbf000000;    '#9'// -0.5'
        #9'mov.f32 '#9'%f73, 0f3d2aaaa5;    '#9'// 0.0416666'
        #9'mov.f32 '#9'%f74, 0fbab6061a;    '#9'// -0.00138873'
        #9'mov.f32 '#9'%f75, 0f37ccf5ce;    '#9'// 2.44332e-005'
        #9'mad.f32 '#9'%f76, %f75, %f70, %f74;'
        #9'mad.f32 '#9'%f77, %f70, %f76, %f73;'
        #9'mad.f32 '#9'%f78, %f70, %f77, %f72;'
        #9'mad.f32 '#9'%f79, %f70, %f78, %f71;'
        #9'bra.uni '#9'$Lt_0_34306;'
        '$Lt_0_34562:'
        #9'.loc'#9'18'#9'1711'#9'0'
        #9'mov.f32 '#9'%f80, 0fbe2aaaa3;    '#9'// -0.166667'
        #9'mov.f32 '#9'%f81, 0f3c08839e;    '#9'// 0.00833216'
        #9'mov.f32 '#9'%f82, 0fb94ca1f9;    '#9'// -0.000195153'
        #9'mad.f32 '#9'%f83, %f82, %f70, %f81;'
        #9'mad.f32 '#9'%f84, %f70, %f83, %f80;'
        #9'mul.f32 '#9'%f85, %f70, %f84;'
        #9'mad.f32 '#9'%f79, %f85, %f58, %f58;'
        '$Lt_0_34306:'
        #9'.loc'#9'18'#9'1713'#9'0'
        #9'neg.f32 '#9'%f86, %f79;'
        #9'and.b32 '#9'%r172, %r169, 2;'
        #9'mov.s32 '#9'%r173, 0;'
        #9'setp.ne.s32 '#9'%p24, %r172, %r173;'
        #9'selp.f32 '#9'%f79, %f86, %f79, %p24;'
        #9'.loc'#9'18'#9'1716'#9'0'
        #9'mov.f32 '#9'%f56, %f79;'
        '$LDWendi___isinff_204_1:'
        #9'.loc'#9'18'#9'1638'#9'0'
        #9'ld.param.u32 '#9'%r2, [__cudaparm__Z6kernelP6float4jjf_width];'
        #9'.loc'#9'28'#9'25'#9'0'
        #9'mul.lo.u32 '#9'%r174, %r94, %r2;'
        #9'add.u32 '#9'%r175, %r4, %r174;'
        #9'mul.lo.u32 '#9'%r176, %r175, 16;'
        #9'ld.param.u32 '#9'%r177, [__cudaparm__Z6kernelP6float4jjf_pos];'
        #9'add.u32 '#9'%r178, %r177, %r176;'
        #9'mul.f32 '#9'%f87, %f56, %f14;'
        #9'mov.f32 '#9'%f88, 0f3f000000;    '#9'// 0.5'
        #9'mul.f32 '#9'%f89, %f87, %f88;'
        #9'mov.f32 '#9'%f90, 0f3f800000;    '#9'// 1'
        #9'st.global.v4.f32 '#9'[%r178+0], {%f6,%f89,%f50,%f90};'
        #9'.loc'#9'28'#9'26'#9'0'
        #9'exit;'
        '$LDWend__Z6kernelP6float4jjf:'
        #9'} // _Z6kernelP6float4jjf'
        '')
      Compiler = GLSCUDACompiler1
      object MakeDotField: TCUDAFunction
        KernelName = '_Z6kernelP6float4jjf'
        BlockShape.SizeX = 8
        BlockShape.SizeY = 8
        OnParameterSetup = MakeVertexBufferParameterSetup
      end
    end
    object DotFieldMapper: TCUDAGLGeometryResource
      FeedBackMesh = GLFeedBackMesh1
      Mapping = grmWriteDiscard
      Left = 248
      Top = 264
    end
  end
  object GLSCUDACompiler1: TGLSCUDACompiler
    NVCCPath = 'C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v3.2\bin\'
    CppCompilerPath = 'C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\bin\'
    ProjectModule = 'Simple kernel.cu'
    Left = 448
    Top = 128
  end
  object GLSLShader1: TGLSLShader
    FragmentProgram.Code.Strings = (
      '#version 330'
      'out vec4 FragColor;'
      'void main(void)'
      '{'
      '    FragColor = vec4(1.0, 0.0, 0.0, 1.0);'
      '};')
    FragmentProgram.Enabled = True
    VertexProgram.Code.Strings = (
      '#version 330'
      'in vec4 Position;'
      'uniform mat4 ModelViewProjectionMatrix;'
      'void main()'
      '{'
      '    gl_Position = ModelViewProjectionMatrix * Position;'
      '};')
    VertexProgram.Enabled = True
    OnApply = GLSLShader1Apply
    FailedInitAction = fiaSilentDisable
    Left = 24
    Top = 128
  end
end
