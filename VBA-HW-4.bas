Attribute VB_Name = "Module1"
Option Explicit

Sub Macro1()

    'Add a chart onto the active sheet and select the chart
    ActiveSheet.Shapes.AddChart.Select
    
    With ActiveChart
     
            'Chart type is Clustered Bar chart
            .ChartType = xlBarClustered
      
            'The data set is located in cells A3:E6 of "Sheet1" worksheet
            .SetSourceData Source:=Worksheets("Sheet1").Range("A3:E6")
      
            'Set a chart title, located at the top of the chart
            .SetElement (msoElementChartTitleAboveChart)
      
            'Assign the content of cell B1 to the title of the chart
            .chartTitle.Text = Worksheets("Sheet1").Range("B1").Value
      
            'Move the chart to a new sheet. Name this sheet "Sales Chart"
            .Location Where:=xlLocationAsNewSheet, Name:="Sales Chart"
            
     
      End With
      

End Sub


Sub Macro2()
    
    'Creates a variable that gets the value of a selected range of cells
    Dim selectedrange As Range
    Set selectedrange = Selection

    'Add a chart onto the active sheet and select the chart
    ActiveSheet.Shapes.AddChart.Select
    
    With ActiveChart
     
            'Chart type is Clustered Bar chart
            .ChartType = xlBarClustered
      
            'The data set is located in the selected range of cells on any worksheet
            .SetSourceData Source:=selectedrange
            
            'Creates a variable that gets the InputBox's input for the Chart Title
            Dim DesiredChartTitle As String
            DesiredChartTitle = InputBox("Give your Chart a Title")
            
            'Creates a variable that gets the InputBox's input for the Sheet Name
            Dim DesiredSheetName As String
            DesiredSheetName = InputBox("Name New Sheet")
      
            'Set a chart title, located at the top of the chart
            .SetElement (msoElementChartTitleAboveChart)
      
            'Assign the content of DesiredChartTitle Inputbox to the title of the chart
            .chartTitle.Text = DesiredChartTitle
      
            'Move the chart to a new sheet. Name this sheet the content of DesiredSheetName InputBox
            .Location Where:=xlLocationAsNewSheet, Name:=DesiredSheetName
            
     
      End With
      
End Sub

