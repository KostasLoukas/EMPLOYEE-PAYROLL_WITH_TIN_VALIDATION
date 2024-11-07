# Employee Payroll Program with TIN Validation
This program is used a tool to calculate and extract information about the employees' salary (gross and net) as well as the taxes that are applied to his person.

The input is the PAYROLL-FILE which's records consist of: The employee's name (EMPLOYEE 1, EMPLOYEE 2 ETC.), their Final Work Hours (first three digits), their Hour Pay Rate
(next 4 digits, two digits last for decimal) and their TIN (remaining 9 digits).

The program checks and outputs each valid or invalid employee (according to their TIN) in the respecting file (VALID-PAYROL-FILE for employees with valid TIN and 
INVALID-PAYROLL-FILE for employees with invalid TIN) along with their respecting information. Salary is NOT calculated for employee with invalid TIN.

Lastly, the STATS-FILE contains the totals for gross and net salaries as well as the total taxes applied (again, regarding only employees with a valid TIN).
