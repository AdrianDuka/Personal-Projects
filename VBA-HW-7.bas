Attribute VB_Name = "Module5"
Sub vba_hw7_D()
'
' Macro1 Macro
'
      Dim dateStart As Date
      Dim dateStop As Date
      Dim indexMonth As Integer
      Dim numDaysInMonth As Integer
      Dim nameMonth As String

'1. Ask user for starting date (Assumption is that this will be the 1st of the month)
 'Determine needed information from starting date
             dateStart = InputBox(prompt:= _
                    "Please give the starting date of the month in the form mm/dd/yyyy")
            dateStop = WorksheetFunction.EoMonth(dateStart, 0)
                    
            indexMonth = Month(dateStart)
            nameMonth = MonthName(indexMonth)
                    
            numDaysInMonth = dateStop - dateStart + 1

'2. Copy Template sheet from the macro workbook to the end of the data workbook
    ThisWorkbook.Sheets("Template").Copy After:=Workbooks("vba_hwk7_Data.xlsx").Sheets(1)
    
    Dim sheetCount As Integer
    Dim x, y, z As Integer
    
    sheetCount = ActiveWorkbook.Sheets.Count
    
    For x = 1 To sheetCount - 1
        For y = x + 1 To sheetCount
    
      If Sheets(y).Name < Sheets(x).Name Then
        Sheets(y).Move Before:=Sheets(x)
      
      End If
    
'3. Fill in dates for the desired month
    Range("A2").FormulaR1C1 = dateStart
    Range("A2").DataSeries Rowcol:=xlColumns, Type:=xlChronological, Date:= _
        xlDay, Step:=1, Stop:=dateStop, Trend:=False
    
'4. Clear the rows following the last date of the month
        ' select the range of data
                someRange = Range("A2", Range("A2").End(xlDown))
                
            ' if numDaysInMonth is = 28, then select range A30:F32/resize that to 3 rows and 6 columns, and call the Clear method.
                If numDaysInMonth = 28 Then
                    someRange = Range("A30:F32").Clear
        
             ' if numDaysInMonth is = 29, then select range A31:F33/resize that to 2 rows and 6 columns, and call the Clear method.
                ElseIf numDaysInMonth = 29 Then
                    someRange = Range("A31:F32").Clear
             
             ' if numDaysInMonth is = 30, then select range A32:F34/resize that to 1 rows and 6 columns, and call the Clear method
                ElseIf numDaysInMonth = 30 Then
                    someRange = Range("A32:F32").Clear
                
            End If
'5. Rename new sheet using the desired month
    Sheets("Template").Name = nameMonth
    
    
'6. Select cell B2 on the new sheet
    Range("B2").Select
    Windows("vba_hwk7_Macro.xlsm").Activate
    
    Next y
        Next x
        
End Sub

