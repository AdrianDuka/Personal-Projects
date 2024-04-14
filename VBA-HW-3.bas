Attribute VB_Name = "Module1"
Sub Macro1()
Attribute Macro1.VB_ProcData.VB_Invoke_Func = " \n14"
'
' Macro1 Macro
'

'
    Sheets("Template").Select
    Sheets("Template").Copy After:=Sheets(1)
    Sheets("Template (2)").Select
    Sheets("Template (2)").Name = "Human Resources"
    Range("A1:I1").Select
    ActiveCell.FormulaR1C1 = "Human Resources"
    Range("A2:I2").Select
    ActiveCell.FormulaR1C1 = "2021"
    Range("A4:H4").Select
    Selection.ClearContents
End Sub

Sub Macro2()
'
' Macro1 Macro
'
    Sheets("Template").Copy After:=Sheets(1)
    'Selects sheet named "Template" and duplicates it into a new sheet
    
    Sheets("Template (2)").Name = "Human Resources"
    'Selects duplicate sheet named "Template (2)" and renames the sheet to Human Resources
    
    Range("A1:I1").FormulaR1C1 = "Human Resources"
    'Selects header range A1:I1 and renames it Human Resources
    
    Range("A2:I2").FormulaR1C1 = "2021"
    'Selects header range A2:I2 and renames it to 2021
    
    Range("A4:H4").ClearContents
    'Selects range A4:H4 and clears contents of that range
    
    
End Sub

Sub Macro3()
'
' Macro1 Macro
'

'
    Sheets("Template").Copy Before:=Sheets(1)
    Department = InputBox("Enter a Department Name")
    YearName = InputBox("Enter a Year")
    Sheets("Template (2)").Name = Department
    Range("A1:I1").FormulaR1C1 = Department
    Range("A2:I2").FormulaR1C1 = YearName
    Range("A4:H4").ClearContents
End Sub



