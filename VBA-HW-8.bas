Attribute VB_Name = "Module3"
Sub LeapYearMacro()

Dim year As Integer
Dim LeapYear As Boolean


'Ensures that year inputted must be 4 digits
year = CInt(InputBox("Enter year: "))


'Elif statements for year parameters to determine if leap year or not
If year Mod 4 = 0 Then
If year Mod 100 = 0 Then
If year Mod 400 = 0 Then
LeapYear = True
Else
LeapYear = False
End If
Else
LeapYear = True
End If
Else
LeapYear = False

End If

If LeapYear Then


'Message Box prompts for wether year entered is a leap year or not
MsgBox year & " is a leap year!", vbInformation
Else

MsgBox year & " is a NOT leap year!", vbInformation

End If

End Sub
