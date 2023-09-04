Attribute VB_Name = "Module1"
Sub Macro1Test()
Attribute Macro1Test.VB_ProcData.VB_Invoke_Func = " \n14"
'
' Macro1Test Macro
'

'Creates new sheet called Player Comparison
    Sheets.Add(After:=Sheets("Player Statistics")).Name = "Player Comparison"
    
'Makes Player Statistics ActiveSheet
    Sheets("Player Statistics").Select
    
'Copies column titles into Player Comparison Sheet
    Range("A1").Select
    Range(Selection, Selection.End(xlToRight)).Select
    Selection.Copy
    Sheets("Player Comparison").Select
    Range("A1").Select
    ActiveSheet.Paste
    
'Makes Player Statistics ActiveSheet again
    Sheets("Player Statistics").Select

'Overarching Do/Loop command to rerun nested code until user selects no in the MsgBox
Do

'Nested Inputbox statement to find player and put their statistics in "Player Comparison" Sheet
    Dim userInput As String
    userInput = InputBox("Enter a player's name:")

    Cells.Find(What:=userInput, After:=ActiveCell, LookIn:=xlFormulas2, _
        LookAt:=xlPart, SearchOrder:=xlByRows, SearchDirection:=xlNext, _
        MatchCase:=False, SearchFormat:=False).Activate
    Range(Selection, Selection.End(xlToRight)).Select
    Selection.Copy
    Sheets("Player Comparison").Select
    Rows("2:2").Select
    Selection.Insert Shift:=xlDown, CopyOrigin:=xlFormatFromLeftOrAbove
    Range("A2").Select
    ActiveSheet.Paste
    Cells.Select
    Selection.Columns.AutoFit
    Range("A2").Select

'MsgBox to determine whether user would like to add another player to compare
    Dim answer As Integer
    answer = MsgBox("Would you like to compare another player?", vbQuestion + vbYesNo, "Message Box")

    If answer = vbYes Then
    Sheets("Player Statistics").Select
    Else
    End If
    
Loop While answer = vbYes

End Sub
