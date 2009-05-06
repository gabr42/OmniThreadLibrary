object frmRecursiveTreeDemo: TfrmRecursiveTreeDemo
  Left = 0
  Top = 0
  Anchors = [akTop]
  Caption = 'Recursive tree walk demo'
  ClientHeight = 247
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblNumChildren: TLabel
    Left = 8
    Top = 11
    Width = 94
    Height = 13
    Caption = 'Number of children:'
  end
  object lblTreeDepth: TLabel
    Left = 8
    Top = 39
    Width = 88
    Height = 13
    Caption = 'Depth of the tree:'
  end
  object lblNumTasks: TLabel
    Left = 8
    Top = 123
    Width = 114
    Height = 13
    Caption = 'Number of active tasks:'
  end
  object lblNumNodes: TLabel
    Left = 8
    Top = 67
    Width = 112
    Height = 13
    Caption = 'Total number of nodes:'
  end
  object lblTreeSize: TLabel
    Left = 8
    Top = 95
    Width = 105
    Height = 13
    Caption = 'Memory consumption:'
  end
  object inpNumChildren: TSpinEdit
    Left = 128
    Top = 8
    Width = 65
    Height = 22
    MaxValue = 9999
    MinValue = 1
    TabOrder = 0
    Value = 6
    OnChange = inpNumChildrenChange
  end
  object inpTreeDepth: TSpinEdit
    Left = 128
    Top = 36
    Width = 65
    Height = 22
    MaxValue = 9999
    MinValue = 4
    TabOrder = 1
    Value = 10
    OnChange = inpNumChildrenChange
  end
  object inpNumTasks: TSpinEdit
    Left = 128
    Top = 120
    Width = 65
    Height = 22
    MaxValue = 9999
    MinValue = 1
    TabOrder = 4
    Value = 1
  end
  object outNumNodes: TEdit
    Left = 128
    Top = 64
    Width = 65
    Height = 21
    ParentColor = True
    ReadOnly = True
    TabOrder = 2
  end
  object outTreeSize: TEdit
    Left = 128
    Top = 92
    Width = 65
    Height = 21
    ParentColor = True
    ReadOnly = True
    TabOrder = 3
  end
  object btnBuildTree: TButton
    Left = 8
    Top = 152
    Width = 185
    Height = 25
    Caption = 'Build tree'
    TabOrder = 5
    OnClick = btnBuildTreeClick
  end
  object lbLog: TListBox
    Left = 199
    Top = 0
    Width = 273
    Height = 247
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 6
  end
  object btnSingleCoreTest: TButton
    Left = 8
    Top = 183
    Width = 185
    Height = 25
    Caption = 'Single task test'
    TabOrder = 7
    OnClick = btnSingleCoreTestClick
  end
  object btnMultiCoreTest: TButton
    Left = 8
    Top = 214
    Width = 185
    Height = 25
    Caption = 'Multiple task test'
    TabOrder = 8
    OnClick = btnMultiCoreTestClick
  end
  object OtlMonitor: TOmniEventMonitor
    OnTaskMessage = OtlMonitorTaskMessage
    OnTaskTerminated = OtlMonitorTaskTerminated
    Left = 8
    Top = 208
  end
end
