object Form1: TForm1
  Left = 268
  Top = 248
  Caption = 'Pak Editor'
  ClientHeight = 493
  ClientWidth = 685
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    685
    493)
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 691
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
    ExplicitWidth = 545
  end
  object ListView: TListView
    Left = 161
    Top = 0
    Width = 524
    Height = 493
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'Name'
      end
      item
        Caption = 'Size'
        Width = 70
      end>
    Enabled = False
    Items.ItemData = {
      054E0000000100000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
      001A4F00700065006E0020006F00720020006300720065006100740065002000
      700061006B002000660069006C0065002E002E002E00}
    MultiSelect = True
    SmallImages = ImageList1
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = ListViewChange
    OnClick = ListViewClick
    OnDblClick = ListViewDblClick
    OnKeyDown = ListViewKeyDown
  end
  object PanelTree: TPanel
    Left = 0
    Top = 0
    Width = 161
    Height = 493
    Align = alLeft
    Caption = ' '
    TabOrder = 1
    object TreeView: TTreeView
      Left = 1
      Top = 1
      Width = 159
      Height = 299
      Align = alClient
      Images = ImageList1
      Indent = 23
      ShowRoot = False
      TabOrder = 0
      ToolTips = False
      OnChange = TreeViewChange
      OnCollapsing = TreeViewCollapsing
      OnKeyDown = TreeViewKeyDown
    end
    object GLSceneViewer1: TGLSceneViewer
      Left = 1
      Top = 300
      Width = 159
      Height = 192
      Camera = GLCamera1
      VSync = vsmSync
      Buffer.BackgroundColor = clWhite
      Buffer.ContextOptions = [roDoubleBuffer, roStencilBuffer, roRenderToWindow]
      Buffer.AntiAliasing = aa2x
      FieldOfView = 76.969398498535160000
      PenAsTouch = False
      Align = alBottom
      TabOrder = 1
    end
  end
  object MainMenu1: TMainMenu
    Left = 230
    Top = 176
    object File1: TMenuItem
      Caption = 'File'
      object New1: TMenuItem
        Caption = 'New...'
        OnClick = New1Click
      end
      object Open1: TMenuItem
        Caption = 'Open...'
        OnClick = Open1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Createfolder1: TMenuItem
        Caption = 'Create folder...'
        OnClick = Createfolder1Click
      end
      object Addfiles1: TMenuItem
        Caption = 'Add file(s)...'
        OnClick = Addfiles1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Compression1: TMenuItem
        Caption = 'Compression'
        object None1: TMenuItem
          Caption = 'None'
          Checked = True
          GroupIndex = 2
          OnClick = None1Click
        end
        object Fast1: TMenuItem
          Tag = 1
          Caption = 'Fast'
          GroupIndex = 2
          OnClick = None1Click
        end
        object Default1: TMenuItem
          Tag = 2
          Caption = 'Default'
          GroupIndex = 2
          OnClick = None1Click
        end
        object Max1: TMenuItem
          Tag = 3
          Caption = 'Max'
          GroupIndex = 2
          OnClick = None1Click
        end
      end
      object N3: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object Deleteselectedfile1: TMenuItem
        Caption = 'Delete selected file(s)'
        GroupIndex = 2
        OnClick = Deleteselectedfile1Click
      end
      object Deleteselectedfolder1: TMenuItem
        Caption = 'Delete selected folder'
        GroupIndex = 2
        OnClick = Deleteselectedfolder1Click
      end
      object N2: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object Extractselectedfiles1: TMenuItem
        Caption = 'Extract selected file(s)'
        GroupIndex = 2
        OnClick = Extractselectedfiles1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 184
    Top = 64
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'glp'
    Filter = 'All (Pak, Zlib)|*.pak;*.zlib|PAK files|*.pak|ZLib Files|*.zlib'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 216
    Top = 64
  end
  object ImageList1: TImageList
    Height = 20
    Width = 20
    Left = 216
    Top = 32
  end
  object GLScene1: TGLScene
    Left = 248
    Top = 32
    object GLCamera1: TGLCamera
      DepthOfView = 100000.000000000000000000
      FocalLength = 100.000000000000000000
      NearPlaneBias = 0.100000001490116100
      TargetObject = GLCube1
      Position.Coordinates = {0000F04100002042000048420000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
    object GLFreeForm1: TGLFreeForm
    end
    object GLSprite1: TGLSprite
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'image'
      Position.Coordinates = {0000A0420000A042000000000000803F}
      Visible = False
      Width = 160.000000000000000000
      Height = 160.000000000000000000
      Rotation = 0.000000000000000000
    end
    object GLCube1: TGLCube
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'image'
      Visible = False
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'image'
        Tag = 0
        Material.Texture.ImageClassName = 'TGLCompositeImage'
        Material.Texture.Image.Width = 256
        Material.Texture.Image.Height = 256
        Material.Texture.Image.Depth = 0
        Material.Texture.Disabled = False
      end>
    Left = 248
    Top = 64
  end
  object GLSimpleNavigation1: TGLSimpleNavigation
    Form = Owner
    GLSceneViewer = GLSceneViewer1
    FormCaption = 'GLScene Pak Editor - %FPS'
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
    Left = 280
    Top = 32
  end
  object GLSArchiveManager1: TGLSArchiveManager
    Archives = <
      item
        Name = 'LibArchive'
      end>
    Left = 280
    Top = 64
  end
end
