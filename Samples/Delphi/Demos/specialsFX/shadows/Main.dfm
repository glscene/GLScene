object MainFm: TMainFm
  Left = 135
  Top = 57
  AutoSize = True
  Caption = 'Shadows'
  ClientHeight = 441
  ClientWidth = 553
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 273
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Main View'
    Color = 16099723
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label2: TLabel
    Left = 280
    Top = 0
    Width = 273
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Lightsource Point of view'
    Color = 16099723
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Panel2: TPanel
    Left = 280
    Top = 16
    Width = 273
    Height = 273
    TabOrder = 0
    object Caster: TGLSceneViewer
      Left = 8
      Top = 8
      Width = 256
      Height = 256
      Camera = GLCamera2
      Buffer.FaceCulling = False
      FieldOfView = 137.326278686523400000
      OnMouseDown = CasterMouseDown
      OnMouseMove = CasterMouseMove
      OnMouseUp = CasterMouseUp
      TabOrder = 0
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 16
    Width = 273
    Height = 273
    TabOrder = 1
    object Viewer: TGLSceneViewer
      Left = 8
      Top = 8
      Width = 256
      Height = 256
      Camera = GLCamera1
      FieldOfView = 137.326278686523400000
      OnMouseDown = ViewerMouseDown
      OnMouseMove = ViewerMouseMove
      OnMouseUp = ViewerMouseUp
      TabOrder = 0
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 296
    Width = 273
    Height = 41
    TabOrder = 2
    object Label4: TLabel
      Left = 25
      Top = 6
      Width = 72
      Height = 11
      Caption = 'Camera Distance'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
    end
    object TimeLbl: TLabel
      Left = 228
      Top = 27
      Width = 21
      Height = 11
      Caption = 'Time'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
    end
    object DistanceBar: TTrackBar
      Left = 6
      Top = 16
      Width = 115
      Height = 16
      Hint = 'Moves the main camera closer/further from the teapot'
      Max = 30
      Min = 1
      ParentShowHint = False
      Position = 8
      SelEnd = 20
      SelStart = 1
      ShowHint = True
      TabOrder = 0
      TabStop = False
      ThumbLength = 10
      OnChange = DistanceBarChange
    end
    object CastBtn: TButton
      Left = 208
      Top = 8
      Width = 57
      Height = 17
      Hint = 
        'Measure the time it takes in s/100 to render the lightsource z-b' +
        'uffer, generate the shadow texture and render the main view. '
      Caption = 'Cast'
      TabOrder = 1
      OnClick = CastBtnClick
    end
  end
  object Panel4: TPanel
    Left = 280
    Top = 296
    Width = 273
    Height = 41
    TabOrder = 3
    object Label3: TLabel
      Left = 25
      Top = 6
      Width = 72
      Height = 11
      Caption = 'Camera Distance'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 193
      Top = 6
      Width = 27
      Height = 11
      Caption = 'Focus'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
    end
    object DistanceBar2: TTrackBar
      Left = 6
      Top = 16
      Width = 115
      Height = 16
      Hint = 'Moves the light closer/further from the teapot.'
      Max = 30
      Min = 1
      ParentShowHint = False
      Position = 9
      SelEnd = 20
      SelStart = 1
      ShowHint = True
      TabOrder = 0
      TabStop = False
      ThumbLength = 10
      OnChange = DistanceBar2Change
    end
    object Focal: TTrackBar
      Left = 150
      Top = 17
      Width = 115
      Height = 16
      Hint = 
        'Adjust the Focal length of the lightsource camera, to adjust the' +
        ' lightbeam width'
      Max = 300
      Min = 1
      ParentShowHint = False
      Frequency = 10
      Position = 50
      ShowHint = True
      TabOrder = 1
      TabStop = False
      ThumbLength = 10
      OnChange = FocalChange
    end
  end
  object Panel5: TPanel
    Left = 0
    Top = 344
    Width = 121
    Height = 97
    TabOrder = 4
    object FrustBox: TCheckBox
      Left = 8
      Top = 24
      Width = 105
      Height = 17
      Caption = 'Frustrum Shadow'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = FrustBoxClick
    end
    object RotateBox: TCheckBox
      Left = 8
      Top = 72
      Width = 105
      Height = 17
      Caption = 'Rotate the Torus'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = RotateBoxClick
    end
    object ShadowOnBox: TCheckBox
      Left = 8
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Shadows On'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = ShadowOnBoxClick
    end
    object SoftBox: TCheckBox
      Left = 8
      Top = 56
      Width = 73
      Height = 17
      Hint = 
        'Tests 4 pixels on the lightsource z-buffer, in stead of 1, to ca' +
        'lculate shadow brightness, and give soft edges to shadows.'
      Caption = 'Soft Edges'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = SoftBoxClick
    end
    object SkyShadBox: TCheckBox
      Left = 8
      Top = 40
      Width = 97
      Height = 17
      Caption = 'Sky Shadow'
      TabOrder = 4
      OnClick = SkyShadBoxClick
    end
  end
  object Panel6: TPanel
    Left = 432
    Top = 344
    Width = 121
    Height = 97
    TabOrder = 5
    object Label9: TLabel
      Left = 28
      Top = 64
      Width = 59
      Height = 11
      Caption = 'Shadow Alpha'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
    end
    object FadeBox: TCheckBox
      Left = 8
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Depth of view fade'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = FadeBoxClick
    end
    object dovBar: TTrackBar
      Left = 2
      Top = 21
      Width = 115
      Height = 16
      Hint = 'Adjust depth of view'
      Max = 100
      Min = 1
      ParentShowHint = False
      Frequency = 5
      Position = 50
      SelEnd = 20
      SelStart = 1
      ShowHint = True
      TabOrder = 1
      TabStop = False
      ThumbLength = 10
      OnChange = dovBarChange
    end
    object AlphaBar: TTrackBar
      Left = 2
      Top = 74
      Width = 115
      Height = 16
      Hint = 'Adjust the darkness of shadows'
      Max = 256
      Min = 1
      ParentShowHint = False
      Frequency = 10
      Position = 128
      SelEnd = 20
      SelStart = 1
      ShowHint = True
      TabOrder = 2
      TabStop = False
      ThumbLength = 10
      OnChange = AlphaBarChange
    end
  end
  object Memo1: TMemo
    Left = 128
    Top = 344
    Width = 297
    Height = 97
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Small Fonts'
    Font.Style = []
    Lines.Strings = (
      
        'This application casts and draws shadows, using the Z-Buffer met' +
        'hod.'
      'Left-click and drag on the viewers, to rotate the camera angles.'
      
        'Anything that is not visible from the lightsource viewer(Right) ' +
        'is in '
      'shadow in the main viewer.(Left)'
      ''
      
        'When Depth Fade is on, the light fades as it approaches the far ' +
        'plane.'
      'Shadows can be set to have either hard or soft edges.'
      
        'The area outside the lightsource view, can be set to be light or' +
        ' dark.')
    ParentFont = False
    TabOrder = 6
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 32
    object Objects: TGLDummyCube
      CubeSize = 1.000000000000000000
      object HeightField1: TGLHeightField
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'Tiles'
        Direction.Coordinates = {000000000000803F0000000000000000}
        Position.Coordinates = {00000000000020C0000000000000803F}
        Scale.Coordinates = {00002041000020410000004000000000}
        Up.Coordinates = {00000000000000000000803F00000000}
        XSamplingScale.Min = -1.000000000000000000
        XSamplingScale.Max = 1.000000000000000000
        XSamplingScale.Step = 0.070000000298023220
        YSamplingScale.Min = -1.000000000000000000
        YSamplingScale.Max = 1.000000000000000000
        YSamplingScale.Step = 0.070000000298023220
        Options = [hfoTextureCoordinates]
      end
      object Cube1: TGLCube
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'PlaneMat'
        Position.Coordinates = {CDCCCC3F00000000000000000000803F}
        Scale.Coordinates = {CDCCCC3D000020400000404000000000}
      end
      object Torus1: TGLTorus
        Material.Texture.TextureMode = tmModulate
        MajorRadius = 1.100000023841858000
        MinorRadius = 0.150000005960464500
        Rings = 28
        Sides = 16
        StopAngle = 360.000000000000000000
        Parts = [toSides, toStartDisk, toStopDisk]
        BehavioursData = {
          0458434F4C02010201060B54474C42496E657274696102001200000000020002
          00050000000000000080FF3F0200080500000000000000000000050000000000
          00000000000500000000000000A0034009020008020008}
      end
      object Teapot1: TGLTeapot
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'BeigeMarble'
        Scale.Coordinates = {9A99D93F9A99D93F9A99D93F00000000}
        BehavioursData = {
          0458434F4C02010201060B54474C42496E657274696102001200000000020002
          00050000000000000080FF3F0200080500000000000000A00340050000000000
          0000000000050000000000000000000008020008020008}
      end
    end
    object Shadows1: TGLZShadows
      Viewer = Viewer
      Caster = MemView
      FrustShadow = True
      SkyShadow = False
      Optimise = op9in1
      Width = 256
      Height = 256
      Color.Color = {0000000000000000000000000000003F}
      Soft = False
      Tolerance = 0.014999999664723870
      DepthFade = False
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Objects
      Position.Coordinates = {00000000000000000000A0400000803F}
      Left = 272
      Top = 200
    end
    object GLCamera2: TGLCamera
      DepthOfView = 50.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Objects
      Position.Coordinates = {000040400000E040000080400000803F}
      Left = 280
      Top = 208
      object GLLightSource1: TGLLightSource
        Ambient.Color = {9A99993E9A99993E9A99993E0000803F}
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'ShadowMat'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000003F}
        Material.FrontProperties.Emission.Color = {0000803F0000803F0000803F0000803F}
        Material.BlendingMode = bmTransparency
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.ColorFormat = 6408
      end
      item
        Name = 'PlaneMat'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLBlankImage'
        Material.Texture.Image.ColorFormat = 6408
      end
      item
        Name = 'Tiles'
        Tag = 0
        Material.Texture.TextureMode = tmModulate
      end
      item
        Name = 'BeigeMarble'
        Tag = 0
        Material.Texture.TextureMode = tmModulate
      end
      item
        Name = 'Marble'
        Tag = 0
        Material.Texture.TextureMode = tmModulate
      end>
    Left = 48
    Top = 32
  end
  object MemView: TGLMemoryViewer
    Camera = GLCamera2
    Buffer.Lighting = False
    Left = 152
    Top = 32
  end
  object AsyncTimer1: TGLAsyncTimer
    Enabled = True
    Interval = 700
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpNormal
    Left = 16
    Top = 80
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Enabled = False
    OnProgress = GLCadencer1Progress
    Left = 16
    Top = 128
  end
end
