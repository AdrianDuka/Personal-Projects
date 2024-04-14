Attribute VB_Name = "Module1"
Sub VbaHwk1()
Attribute VbaHwk1.VB_ProcData.VB_Invoke_Func = " \n14"
'
' VbaHwk1 Macro
'

'
    Range("C5:D5").Select
    Range(Selection, Selection.End(xlDown)).Select
    ActiveWorkbook.Names.Add Name:="Sales", RefersToR1C1:="=Sheet1!R5C3:R46C4"
    ActiveSheet.Shapes.AddChart2(227, xlLine).Select
    ActiveChart.SetSourceData Source:=Range("Sheet1!$C$5:$D$46")
    ActiveChart.SetElement (msoElementChartTitleAboveChart)
    Selection.Caption = "Sales Revenue"
    ActiveChart.ChartArea.Select
    ActiveChart.Location Where:=xlLocationAsNewSheet, Name:="Sales Chart"
End Sub
