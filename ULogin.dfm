object Form_Login: TForm_Login
  Left = 0
  Top = 0
  Caption = 'Form_Login'
  ClientHeight = 194
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Label1: TLabel
    Left = 160
    Top = 19
    Width = 29
    Height = 15
    Caption = 'email'
  end
  object Label2: TLabel
    Left = 160
    Top = 72
    Width = 50
    Height = 15
    Caption = 'password'
  end
  object Edit_Emai: TEdit
    Left = 160
    Top = 40
    Width = 121
    Height = 23
    TabOrder = 0
  end
  object MaskEdit_Password: TMaskEdit
    Left = 160
    Top = 93
    Width = 121
    Height = 23
    TabOrder = 1
    Text = ''
  end
  object Button1: TButton
    Left = 160
    Top = 136
    Width = 121
    Height = 25
    Caption = 'Enviar'
    TabOrder = 2
    OnClick = Button1Click
  end
end
