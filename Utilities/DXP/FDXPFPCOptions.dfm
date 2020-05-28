object DXPFPCOptions: TDXPFPCOptions
  Left = 160
  Top = 111
  BorderStyle = bsDialog
  BorderWidth = 5
  Caption = 'FreePascal Compiler Options'
  ClientHeight = 312
  ClientWidth = 522
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 278
    Width = 522
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      522
      34)
    object BUOk: TButton
      Left = 358
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object BUCancel: TButton
      Left = 446
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 522
    Height = 278
    ActivePage = TSCompiler
    Align = alClient
    TabOrder = 1
    object TSCompiler: TTabSheet
      Caption = 'Compiler'
      object GroupBox1: TGroupBox
        Left = 8
        Top = 8
        Width = 169
        Height = 233
        Caption = 'Code Generation'
        TabOrder = 0
        object CheckBox1: TCheckBox
          Left = 8
          Top = 208
          Width = 89
          Height = 17
          Hint = '-Ce'
          Caption = 'Emulate FPU'
          TabOrder = 0
        end
        object Panel2: TPanel
          Left = 8
          Top = 120
          Width = 153
          Height = 49
          BevelOuter = bvNone
          TabOrder = 1
          object Label1: TLabel
            Left = 0
            Top = 1
            Width = 56
            Height = 13
            Caption = 'Target CPU'
          end
          object RadioButton1: TRadioButton
            Left = 64
            Top = 0
            Width = 65
            Height = 17
            Hint = '-Op1'
            Caption = '386/486'
            TabOrder = 0
          end
          object RadioButton2: TRadioButton
            Left = 64
            Top = 16
            Width = 57
            Height = 17
            Hint = '-Op2'
            Caption = 'Pentium'
            TabOrder = 1
          end
          object RadioButton3: TRadioButton
            Left = 64
            Top = 32
            Width = 81
            Height = 17
            Hint = '-Op3'
            Caption = 'PPro/PII/K6'
            TabOrder = 2
          end
        end
        object Panel3: TPanel
          Left = 8
          Top = 24
          Width = 137
          Height = 33
          BevelOuter = bvNone
          TabOrder = 2
          object Label2: TLabel
            Left = 0
            Top = 1
            Width = 55
            Height = 13
            Caption = 'Optimize for'
          end
          object RadioButton4: TRadioButton
            Left = 64
            Top = 0
            Width = 65
            Height = 17
            Hint = '-OG'
            Caption = 'Speed'
            TabOrder = 0
          end
          object RadioButton5: TRadioButton
            Left = 64
            Top = 16
            Width = 57
            Height = 17
            Hint = '-Og'
            Caption = 'Size'
            TabOrder = 1
          end
        end
        object CheckBox6: TCheckBox
          Left = 8
          Top = 192
          Width = 158
          Height = 17
          Hint = '-Ou'
          Caption = 'Allow uncertain optimizations'
          TabOrder = 3
        end
        object Panel4: TPanel
          Left = 8
          Top = 64
          Width = 137
          Height = 49
          BevelOuter = bvNone
          TabOrder = 4
          object Label3: TLabel
            Left = 0
            Top = 1
            Width = 57
            Height = 13
            Caption = 'Optimization'
          end
          object RadioButton6: TRadioButton
            Left = 64
            Top = 0
            Width = 65
            Height = 17
            Hint = '-O1'
            Caption = 'Level 1'
            TabOrder = 0
          end
          object RadioButton7: TRadioButton
            Left = 64
            Top = 16
            Width = 57
            Height = 17
            Hint = '-O2'
            Caption = 'Level 2'
            TabOrder = 1
          end
          object RadioButton8: TRadioButton
            Left = 64
            Top = 32
            Width = 81
            Height = 17
            Hint = '-O3'
            Caption = 'Level 3'
            TabOrder = 2
          end
        end
        object CheckBox7: TCheckBox
          Left = 8
          Top = 176
          Width = 129
          Height = 17
          Hint = '-Or'
          Caption = 'Register optimizations'
          TabOrder = 5
        end
      end
      object GroupBox3: TGroupBox
        Left = 376
        Top = 136
        Width = 129
        Height = 105
        Caption = 'Debug'
        TabOrder = 2
        object CheckBox9: TCheckBox
          Left = 8
          Top = 24
          Width = 81
          Height = 17
          Hint = '-Sa'
          Caption = 'Assertions'
          TabOrder = 0
        end
      end
      object GroupBox4: TGroupBox
        Left = 376
        Top = 8
        Width = 129
        Height = 121
        Caption = 'Runtime Errors'
        TabOrder = 3
        object CheckBox2: TCheckBox
          Left = 8
          Top = 48
          Width = 97
          Height = 17
          Hint = '-Ci'
          Caption = 'I/O Checking'
          TabOrder = 0
        end
        object CheckBox3: TCheckBox
          Left = 8
          Top = 24
          Width = 113
          Height = 17
          Hint = '-Cr'
          Caption = 'Range Checking'
          TabOrder = 1
        end
        object CheckBox4: TCheckBox
          Left = 8
          Top = 72
          Width = 113
          Height = 17
          Hint = '-Co'
          Caption = 'Overflow Checking'
          TabOrder = 2
        end
        object CheckBox5: TCheckBox
          Left = 8
          Top = 96
          Width = 113
          Height = 17
          Hint = '-Ct'
          Caption = 'Stack Checking'
          TabOrder = 3
        end
      end
      object GroupBox2: TGroupBox
        Left = 184
        Top = 8
        Width = 185
        Height = 121
        Caption = 'Syntax'
        TabOrder = 1
        object CheckBox8: TCheckBox
          Left = 8
          Top = 24
          Width = 169
          Height = 17
          Hint = '-Sc'
          Caption = 'Allow C operators (*=, +=, etc.)'
          TabOrder = 0
        end
        object CheckBox10: TCheckBox
          Left = 8
          Top = 48
          Width = 105
          Height = 17
          Hint = '-Sg'
          Caption = 'Allow goto/label'
          TabOrder = 1
        end
        object CheckBox11: TCheckBox
          Left = 8
          Top = 72
          Width = 105
          Height = 17
          Hint = '-Sh'
          Caption = 'Use AnsiStrings'
          TabOrder = 2
        end
        object CheckBox12: TCheckBox
          Left = 8
          Top = 96
          Width = 145
          Height = 17
          Hint = '-Si'
          Caption = 'Support C++ styled inline'
          TabOrder = 3
        end
      end
    end
    object TSCompilerMessages: TTabSheet
      Caption = 'Messages'
      ImageIndex = 1
      object GroupBox5: TGroupBox
        Left = 8
        Top = 8
        Width = 121
        Height = 97
        Caption = 'Compiler messages'
        TabOrder = 0
        object CBShowWarnings: TCheckBox
          Left = 8
          Top = 24
          Width = 97
          Height = 17
          Caption = 'Show Warnings'
          TabOrder = 0
        end
        object CBShowNotes: TCheckBox
          Left = 8
          Top = 48
          Width = 97
          Height = 17
          Caption = 'Show Notes'
          TabOrder = 1
        end
        object CBShowHints: TCheckBox
          Left = 8
          Top = 72
          Width = 97
          Height = 17
          Caption = 'Show Hints'
          TabOrder = 2
        end
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Linker'
      ImageIndex = 2
      object GroupBox6: TGroupBox
        Left = 8
        Top = 8
        Width = 121
        Height = 105
        Caption = 'Executable Options'
        TabOrder = 0
        object RadioButton9: TRadioButton
          Left = 8
          Top = 48
          Width = 89
          Height = 17
          Hint = '-XD'
          Caption = 'Link Dynamic'
          TabOrder = 0
        end
        object CheckBox16: TCheckBox
          Left = 8
          Top = 24
          Width = 97
          Height = 17
          Hint = '-Xs'
          Caption = 'Strip all symbols'
          TabOrder = 1
        end
        object RadioButton10: TRadioButton
          Left = 8
          Top = 64
          Width = 89
          Height = 17
          Hint = '-XS'
          Caption = 'Link Static'
          TabOrder = 2
        end
        object RadioButton11: TRadioButton
          Left = 8
          Top = 80
          Width = 89
          Height = 17
          Hint = '-XX'
          Caption = 'Link Smart'
          TabOrder = 3
        end
      end
    end
  end
end
