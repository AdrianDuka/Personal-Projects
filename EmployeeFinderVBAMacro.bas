Attribute VB_Name = "Module1"
Sub Find_Employee()
'
'Employee Finder Test Macro
'

'Creates new sheet called Person of Interest
    On Error Resume Next
    Sheets.Add(After:=Sheets("All Employees")).Name = "Person of Interest"
    
'Makes "All Employees" Sheet ActiveSheet
    Sheets("All Employees").Select
    
'Copies column titles into "Person of Interest" Sheet
    Range("A1").Select
    Range(Selection, Selection.End(xlToRight)).Select
    Selection.Copy
    Sheets("Person of Interest").Select
    Range("A1").Select
    ActiveSheet.Paste
    
'Makes "All Employees" Sheet ActiveSheet again
    Sheets("All Employees").Select
    
'Overarching Do/Loop command to re-run nested code until user selects no in MsgBox

Do

'Inputbox statement to find employee
    Dim userInput As String
    userInput = InputBox("Enter the full name of the employee you are looking for, including middle name if applicable:")
    
Cells.Find(What:=userInput, After:=ActiveCell, LookIn:=xlFormulas2, _
        LookAt:=xlPart, SearchOrder:=xlByRows, SearchDirection:=xlNext, _
        MatchCase:=False, SearchFormat:=False).Activate
    Range(Selection, Selection.End(xlToRight)).Select
    Selection.Copy
    Sheets("Person of Interest").Select
    Rows("2:2").Select
    Selection.Insert Shift:=xlDown, CopyOrigin:=xlFormatFromLeftOrAbove
    Range("A2").Select
    ActiveSheet.Paste
    Cells.Select
    Selection.Columns.AutoFit
    Range("A2").Select
    
'MsgBox to determine whether user would like to find another employee from the IHS Directory
    Dim answer As Integer
    answer = MsgBox("Would you like to search for another employee?", vbQuestion + vbYesNo, "Message Box")
    
    If answer = vbYes Then
    Sheets("All Employees").Select
    Else
    End If
    
    Loop While answer = vbYes
    

End Sub
