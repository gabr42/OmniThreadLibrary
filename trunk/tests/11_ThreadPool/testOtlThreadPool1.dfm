object frmTestOtlThreadPool: TfrmTestOtlThreadPool
  Left = 0
  Top = 0
  Caption = 'OtlThreadPool tester'
  ClientHeight = 306
  ClientWidth = 611
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbLog: TListBox
    Left = 143
    Top = 0
    Width = 468
    Height = 306
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object btnSchedule: TButton
    Left = 8
    Top = 39
    Width = 129
    Height = 25
    Caption = 'Schedule'
    TabOrder = 1
    OnClick = btnScheduleClick
  end
  object btnRun: TButton
    Left = 8
    Top = 8
    Width = 129
    Height = 25
    Caption = 'Run'
    TabOrder = 2
    OnClick = btnScheduleClick
  end
  object OmniTED: TOmniTaskEventDispatch
    OnTaskMessage = OmniTEDTaskMessage
    Left = 8
    Top = 264
  end
end
