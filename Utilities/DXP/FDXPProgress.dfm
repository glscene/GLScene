object DXPProgress: TDXPProgress
  Left = 223
  Top = 140
  BorderStyle = bsDialog
  Caption = 'Compilation Progress'
  ClientHeight = 205
  ClientWidth = 399
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 385
    Height = 153
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 369
      Height = 18
      Alignment = taCenter
      AutoSize = False
      Caption = 'DXP / FreePascal'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = cl3DDkShadow
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object STProject: TStaticText
      Left = 8
      Top = 32
      Width = 369
      Height = 18
      AutoSize = False
      BorderStyle = sbsSunken
      Caption = 'STProject'
      ShowAccelChar = False
      TabOrder = 0
    end
    object STStatus: TStaticText
      Left = 8
      Top = 56
      Width = 369
      Height = 18
      Alignment = taCenter
      AutoSize = False
      BorderStyle = sbsSunken
      Caption = 'STStatus'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ShowAccelChar = False
      TabOrder = 1
    end
    object Panel2: TPanel
      Left = 8
      Top = 104
      Width = 177
      Height = 17
      Alignment = taLeftJustify
      BevelOuter = bvLowered
      BorderWidth = 1
      Caption = ' Errors :'
      TabOrder = 2
      object LAErrors: TLabel
        Left = 131
        Top = 2
        Width = 44
        Height = 13
        Align = alRight
        Caption = 'LAErrors'
      end
    end
    object Panel3: TPanel
      Left = 8
      Top = 128
      Width = 177
      Height = 17
      Alignment = taLeftJustify
      BevelOuter = bvLowered
      BorderWidth = 1
      Caption = ' Warnings :'
      TabOrder = 3
      object LAWarnings: TLabel
        Left = 115
        Top = 2
        Width = 60
        Height = 13
        Align = alRight
        Caption = 'LAWarnings'
      end
    end
    object Panel4: TPanel
      Left = 200
      Top = 128
      Width = 177
      Height = 17
      Alignment = taLeftJustify
      BevelOuter = bvLowered
      BorderWidth = 1
      Caption = ' Notes :'
      TabOrder = 4
      object LANotes: TLabel
        Left = 133
        Top = 2
        Width = 42
        Height = 13
        Align = alRight
        Caption = 'LANotes'
      end
    end
    object Panel5: TPanel
      Left = 200
      Top = 104
      Width = 177
      Height = 17
      Alignment = taLeftJustify
      BevelOuter = bvLowered
      BorderWidth = 1
      Caption = ' Hints :'
      TabOrder = 5
      object LAHints: TLabel
        Left = 137
        Top = 2
        Width = 38
        Height = 13
        Align = alRight
        Caption = 'LAHints'
      end
    end
    object Panel6: TPanel
      Left = 8
      Top = 80
      Width = 177
      Height = 17
      Alignment = taLeftJustify
      BevelOuter = bvLowered
      BorderWidth = 1
      Caption = ' Time Elapsed :'
      TabOrder = 6
      object LATime: TLabel
        Left = 139
        Top = 2
        Width = 36
        Height = 13
        Align = alRight
        Caption = 'LATime'
      end
    end
  end
  object BUOk: TButton
    Left = 160
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Ok'
    Enabled = False
    TabOrder = 1
    OnClick = BUOkClick
  end
  object BUAbort: TButton
    Left = 312
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Abort'
    ModalResult = 2
    TabOrder = 2
    Visible = False
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 16
    Top = 16
  end
end
