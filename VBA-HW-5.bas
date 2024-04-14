Attribute VB_Name = "Module1"
Sub Macro1()


    'InputBox code and prompts
    interestrate = InputBox("Please enter your cost of capital (interest rate) in decimal form")
    periods = InputBox("Please enter the number of periods you will be making payments")
    payment = InputBox("Please enter your payment amount as a negative value")
    BegorEnd = InputBox("Please enter (0) if you are making your payment at the end of each period, or (1) if you are making your payment at the beginning of each period")
    extra = InputBox("Are you making any additional payments following the final period? If so, how much is your extra payment?")

    'Assigns the output of the following function as currency
    Dim pvfrominput As Currency

    'Present Value Formula Code
    pvfrominput = PV(interestrate, periods, payment, , BegorEnd)
    
    'New Worksheet Code and rename
    Sheets.Add After:=ActiveSheet
    ActiveSheet.Name = "PresentValue"
    
    'Inserts formula answer into new worksheet
    Sheets("PresentValue").Range("A1").Formula = pvfrominput

    'Delete "Present Value" worksheet prior to the next use of the macro for it to work again
    'P.S. There is no way to get $11,098 to come up as the answer for Scenario 3 on the assignment PDF without the use of the NPV Formula, but the assignment wants us to use the PV formula.
    


End Sub

