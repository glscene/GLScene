object Form1: TForm1
  Left = 192
  Top = 107
  Width = 710
  Height = 371
  Caption = 'DWS2 Scripted GLDirectOpenGL'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 342
    Height = 344
    Camera = GLCamera1
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object Panel2: TPanel
    Left = 342
    Top = 0
    Width = 360
    Height = 344
    Align = alRight
    Caption = 'Panel2'
    TabOrder = 1
    DesignSize = (
      360
      344)
    object CompileButton: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Compile'
      TabOrder = 0
      OnClick = CompileButtonClick
    end
    object Script: TMemo
      Left = 8
      Top = 40
      Width = 345
      Height = 297
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        'glBegin(GL_QUADS);'
        ''
        '  glNormal3f(  0,  0, 1);'
        '  glVertex3f( 0.5,  0.5, 0.5);'
        '  glVertex3f(-0.5,  0.5, 0.5);'
        '  glVertex3f(-0.5, -0.5, 0.5);'
        '  glVertex3f( 0.5, -0.5, 0.5);'
        ''
        '  glNormal3f(  0,  0, -1);'
        '  glVertex3f( 0.5,  0.5, -0.5);'
        '  glVertex3f( 0.5, -0.5, -0.5);'
        '  glVertex3f(-0.5, -0.5, -0.5);'
        '  glVertex3f(-0.5,  0.5, -0.5);'
        ''
        '  glNormal3f(-1,  0,  0);'
        '  glVertex3f(-0.5,  0.5,  0.5);'
        '  glVertex3f(-0.5,  0.5, -0.5);'
        '  glVertex3f(-0.5, -0.5, -0.5);'
        '  glVertex3f(-0.5, -0.5,  0.5);'
        ''
        '  glNormal3f(1,  0,  0);'
        '  glVertex3f(0.5,  0.5,  0.5);'
        '  glVertex3f(0.5, -0.5,  0.5);'
        '  glVertex3f(0.5, -0.5, -0.5);'
        '  glVertex3f(0.5,  0.5, -0.5);'
        ''
        '  glNormal3f(  0, 1,  0);'
        '  glVertex3f(-0.5, 0.5, -0.5);'
        '  glVertex3f(-0.5, 0.5,  0.5);'
        '  glVertex3f( 0.5, 0.5,  0.5);'
        '  glVertex3f( 0.5, 0.5, -0.5);'
        ''
        '  glNormal3f(  0, -1,  0);'
        '  glVertex3f(-0.5, -0.5, -0.5);'
        '  glVertex3f( 0.5, -0.5, -0.5);'
        '  glVertex3f( 0.5, -0.5,  0.5);'
        '  glVertex3f(-0.5, -0.5,  0.5);'
        ''
        'glEnd;')
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 1
      WordWrap = False
    end
  end
  object GLDelphiWebScriptII1: TGLDelphiWebScriptII
    Config.CompilerOptions = []
    Config.MaxDataSize = 0
    Config.Timeout = 0
    Left = 432
    Top = 8
  end
  object dws2OpenGL1xUnit1: Tdws2OpenGL1xUnit
    Script = GLDelphiWebScriptII1
    Left = 496
    Top = 8
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1
      object GLCamera1: TGLCamera
        DepthOfView = 100
        FocalLength = 50
        TargetObject = GLDummyCube1
        Position.Coordinates = {0000803F00000040000040400000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1
          SpotCutOff = 180
        end
      end
    end
    object GLDirectOpenGL1: TGLDirectOpenGL
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
      Blend = False
    end
    object GLHUDText1: TGLHUDText
      Position.Coordinates = {0000204100002041000000000000803F}
      BitmapFont = GLWindowsBitmapFont1
      Alignment = taLeftJustify
      Layout = tlTop
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 40
  end
  object AsyncTimer1: TGLAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpHigher
    Left = 40
    Top = 40
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Left = 40
    Top = 8
  end
  object dws2VectorGeometryUnit1: Tdws2VectorGeometryUnit
    Script = GLDelphiWebScriptII1
    Left = 464
    Top = 8
  end
end
