# Employee Payroll Program with TIN Validation
This program is used as a tool to calculate and produce information about the employees' monthly salary (gross and net) as well as the taxes that are applied to each person.

The input is the PAYROLL-FILE which's records consist of: The employee's name (EMPLOYEE 1, EMPLOYEE 2 ETC.), their Total Work Hours (first three digits), their Hour Pay Rate
(next 4 digits, two of which are for decimal) and their TIN (remaining 9 digits).

The program checks and outputs each valid or invalid employee (according to their TIN) in the respective file (VALID-PAYROL-FILE for employees with a valid TIN and 
INVALID-PAYROLL-FILE for employees with an invalid TIN) along with their respective information. Salary is NOT calculated for employees with an invalid TIN.

TIN Validation is being done through a subroutine which checks if the employee's TIN is valid or invalid.

Lastly, the STATS-FILE contains the totals for all gross and net salaries calculated as well as the total taxes applied (again, regarding only employees with a valid TIN,
invalid employees are not considerated for this calculation).

The program, as well as the subroutine, are both written in COBOL and ran on OpenCobolIDE v4.7.1.
