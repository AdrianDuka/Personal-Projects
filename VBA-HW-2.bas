Attribute VB_Name = "Module1"
Option Explicit

Sub vba_hwk2()
Attribute vba_hwk2.VB_ProcData.VB_Invoke_Func = " \n14"
'
' vba_hwk1 Macro
'

'
'Selects Range C5:D5
    Range("C5:D5").Select
'Names the range selects "Sales"
    ActiveWorkbook.Names.Add Name:="sales", RefersToR1C1:="=Sheet1!R5C3:R46C4"
'Adds line Chart
    ActiveSheet.Shapes.AddChart2(227, xlLine).Select
'Specifies that you want to make a chart from the range selects
    ActiveChart.SetSourceData Source:=Range("Sheet1!$C$5:$D$46")
'Title above chart setting activated
    ActiveChart.SetElement (msoElementChartTitleAboveChart)
'Names the chart title "Sales Revenue"
    ActiveChart.ChartTitle.Text = Cells(2, 3).Value
'Selects Chart Area excel function
    ActiveChart.ChartArea.Select
'Option to create new chart in new excel sheet enabled
    ActiveChart.Location Where:=xlLocationAsNewSheet, Name:="Sales Chart"
End Sub
