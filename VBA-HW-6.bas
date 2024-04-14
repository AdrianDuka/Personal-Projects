Attribute VB_Name = "Module1"
Option Explicit

Sub Macro1()
'
' Recorded Macro1

    Sheets("Template").Select
    Sheets("Template").Copy After:=Sheets(1)
    Sheets("Template (2)").Select
    Sheets("Template (2)").Name = "January"
    Range("E1").Select
    ActiveCell.FormulaR1C1 = "2022"
    Range("E2").Select
    ActiveCell.FormulaR1C1 = "January"
    Range("E3").Select
End Sub

Sub Macro2()
'
' Macro2: Commented and compacted

    'Create a copy of the "Template" sheet
    Sheets("Template").Copy After:=Sheets(1)
    
    'Rename the new "Template (2)" sheet to "January"
    Sheets("Template (2)").Name = "January"
   
    'Assign an appropriate year to cell E1
    Range("E1").FormulaR1C1 = "2022"
    
    'Assign an appropriate month to cell E2
    Range("E2").FormulaR1C1 = "January"

End Sub

Sub Macro3()
'
' Macro3: Commented, compacted, modified, and ask user for the month and year

    'Declare variables to store month and year
    Dim ansMonth As String
    Dim ansYear As String
    
    'Ask user for the month and year. Store them in ansMonth and ansYear, respectively
    ansMonth = InputBox(prompt:="What month (enter as January, February, etc.)? ")
    ansYear = InputBox(prompt:="What year (enter as 2021, 2022, etc.)? ")
    
    'Create a copy of the "Template" sheet
    Sheets("Template").Copy After:=Sheets(1)
    
    'Rename the new "Template (2)" sheet to the month entered by the user
    Sheets("Template (2)").Name = ansMonth
    
    'Assign the year entered by the user to cell E1
    Range("E1").FormulaR1C1 = ansYear
    
    'Assign the month entered by the user to cell E2
    Range("E2").FormulaR1C1 = ansMonth

End Sub


