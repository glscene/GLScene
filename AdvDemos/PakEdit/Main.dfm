object Form1: TForm1
  Left = 253
  Top = 218
  Caption = 'GLScene Pak Editor'
  ClientHeight = 240
  ClientWidth = 539
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    539
    240)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 545
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object TreeView: TTreeView
    Left = 8
    Top = 8
    Width = 161
    Height = 221
    Anchors = [akLeft, akTop, akBottom]
    Images = ImageList1
    Indent = 19
    ShowRoot = False
    TabOrder = 0
    ToolTips = False
    OnChange = TreeViewChange
    OnCollapsing = TreeViewCollapsing
    OnKeyDown = TreeViewKeyDown
  end
  object ListView: TListView
    Left = 175
    Top = 8
    Width = 365
    Height = 221
    Anchors = [akLeft, akTop, akRight, akBottom]
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
    TabOrder = 1
    ViewStyle = vsReport
    OnChange = ListViewChange
    OnClick = ListViewClick
    OnDblClick = ListViewDblClick
    OnKeyDown = ListViewKeyDown
  end
  object MainMenu1: TMainMenu
    Left = 184
    Top = 32
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
      object N4: TMenuItem
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
      object N3: TMenuItem
        Caption = '-'
      end
      object Deleteselectedfile1: TMenuItem
        Caption = 'Delete selected file(s)'
        OnClick = Deleteselectedfile1Click
      end
      object Deleteselectedfolder1: TMenuItem
        Caption = 'Delete selected folder'
        OnClick = Deleteselectedfolder1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Extractselectedfiles1: TMenuItem
        Caption = 'Extract selected file(s)'
        OnClick = Extractselectedfiles1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Converttocompressed: TMenuItem
        Caption = 'Convert to compressed'
        OnClick = ConverttocompressedClick
      end
      object Converttouncompressed: TMenuItem
        Caption = 'Convert to uncompressed'
        OnClick = ConverttouncompressedClick
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
    Filter = 'PAK files|*.pak'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 216
    Top = 64
  end
  object ImageList1: TImageList
    Left = 216
    Top = 32
  end
end
