-- CTE

--Example 00
/* Select products with higher price than the average. */

-- Average product price: 28.8663
select avg(UnitPrice)
from Products

-- no CTE
select	P.ProductName,
	C.CategoryName,
	P.UnitPrice
from Products P
join Categories C on C.CategoryID = P.CategoryID
where UnitPrice > (select avg(UnitPrice)
		from Products)
order by P.UnitPrice;

-- With CTE
with ProdAvgUnitPrice (AvgUnitPrice) as
	(
	select AVG(UnitPrice)
	from Products
	),

	GreaterThanAvg (ProductName, CategoryID, UnitPrice) as
	(
	select	ProductName,
		CategoryID,
		UnitPrice
	from Products P
	where UnitPrice >(select AvgUnitPrice
			from ProdAvgUnitPrice)
	)

select	G.ProductName,
	C.CategoryName,
	G.UnitPrice
from GreaterThanAvg G
join Categories C on C.CategoryID = G.CategoryID
order by P.UnitPrice;


-- EX01
/* Using the Products table, display all product IDs and names (ProductName) whose unit price (UnitPrice) is higher than the average in a given category.
Sort the result by unit price (UnitPrice). Execute the query in two variants: without and with CTE */

-- w/o CTE
select P1.ProductID, P1.ProductName
from Products as P1
where P1.UnitPrice > (select avg(P2.UnitPrice)
			from Products P2
			where P1.CategoryID = P2.CategoryID)
order by P1.UnitPrice;

-- with CTE
with AvgPricePerCat as
	(
	select	C.CategoryID,
		avg(P2.UnitPrice) as AVG
	from Products as P2
	join Categories as C on C.CategoryID = P2.CategoryID
	group by C.CategoryID
	)

select	P1.ProductID,
	P1.ProductName
from Products as P1
join AvgPricePerCat APPC on APPC.CategoryID = P1.CategoryID
where P1.UnitPrice > APPC.AVG
order by P1.UnitPrice;


-- EX02
/* Using the Products and Order Details tables, display all IDs (Products.ProductID) and product names (Products.ProductName)
whose maximum value of orders (UnitPrice*Quantity) is less than the average in the category. 
Sort the result in ascending order by product ID. */

--Order Value Per Category 
select	P.CategoryID,
	sum(OD.UnitPrice * OD.Quantity) Value
from [Order Details] OD
join Products P on P.ProductID = OD.ProductID
group by OD.OrderID, P.CategoryID;

--Order Value Per Product 
select	P.ProductID,
	sum(OD.UnitPrice * OD.Quantity) Value
from [Order Details] OD
join Products P on P.ProductID = OD.ProductID
group by OD.OrderID, P.ProductID
order by P.ProductID;

--Max Order Value Per Product
with OrderValuePerProd (ProductID, ProdOrderValue) as
	(
	select	P.ProductID,
		sum(OD.UnitPrice * OD.Quantity) as ProdOrderValue
	from [Order Details] OD
	join Products P on P.ProductID = OD.ProductID
	group by OD.OrderID, P.ProductID
	)
	
	select	P.CategoryID,
		P.ProductID,
		P.ProductName,
		round(max(OVPP.ProdOrderValue),2) as MaxOrderValue
	from Products P
	join OrderValuePerProd OVPP on OVPP.ProductID = P.ProductID
	group by P.CategoryID, P.ProductID, P.ProductName
	order by P.CategoryID, P.ProductID

--Average Order Value Per Category
with OrderValuePerCat (CategoryID, CatOrderValue) as
	(
	select	P.CategoryID,
		sum(OD.UnitPrice * OD.Quantity) as CatOrderValue
	from [Order Details] OD
	join Products P on P.ProductID = OD.ProductID
	group by OD.OrderID, P.CategoryID
	)
	
	select	P.CategoryID,
		round(avg(OVPC.CatOrderValue),2) as AvgOrderValue
	from Products P
	join OrderValuePerCat OVPC on OVPC.CategoryID = P.CategoryID
	group by P.CategoryID
	order by P.CategoryID

--Combine all those queries: look for max order value (grouped by products) lower than avg order value (grouped by categories)
with OrderValuePerProd (ProductID, ProdOrderValue) as
	(
	select	P.ProductID,
		sum(OD.UnitPrice * OD.Quantity) as ProdOrderValue
	from [Order Details] OD
	join Products P on P.ProductID = OD.ProductID
	group by OD.OrderID, P.ProductID
	),

	OrderValuePerCat (CategoryID, CatOrderValue) as
	(
	select	P.CategoryID,
		sum(OD.UnitPrice * OD.Quantity) as CatOrderValue
	from [Order Details] OD
	join Products P on P.ProductID = OD.ProductID
	group by OD.OrderID, P.CategoryID
	),
	
	AverageOrderValuePerCat (CategoryID, AvgOrderValue) as
	(
	select	P.CategoryID,
		round(avg(OVPC.CatOrderValue),2) as AvgOrderValue
	from Products P
	join OrderValuePerCat OVPC on OVPC.CategoryID = P.CategoryID
	group by P.CategoryID
	),

	MaxOrderValuePerProduct (CategoryID, ProductID, ProductName, MaxOrderValue) as
	(
	select	P.CategoryID,
		P.ProductID,
		P.ProductName,
		max(OVPP.ProdOrderValue) as MaxOrderValue
	from Products P
	join OrderValuePerProd OVPP on OVPP.ProductID = P.ProductID
	group by P.CategoryID, P.ProductID, P.ProductName
	)
	
select	MOVPP.CategoryID,
	MOVPP.ProductID,
	MOVPP.ProductName,
	MOVPP.MaxOrderValue,
	AOVPC.AvgOrderValue
from MaxOrderValuePerProduct MOVPP
join AverageOrderValuePerCat AOVPC on AOVPC.CategoryID = MOVPP.CategoryID
where MOVPP.MaxOrderValue < AOVPC.AvgOrderValue
order by MOVPP.ProductID


-- EX03
/*Using the Employees table, display the ID, name and surname of the employee together with an identifier, name and surname of his supervisor.
To find a given supervisor use the ReportsTo field. Display results for hierarchy level no greater than 1 (starting from 0).
Add the WhoIsThis column to the result, which will take the appropriate values for of a given level:
	- Level = 0 – Krzysiu Jarzyna ze Szczecina
	- Level = 1 - Mr. Frog*/

with EmployeesRecCTE (EmployeeID, FirstName, LastName, ReportsTo, ManagerFirstName, ManagerLastName, Level) as
	(
	select	EmployeeID,
		FirstName,
		LastName,
		ReportsTo,
		cast(null as nvarchar(10)) as ManagerFirstName, 
		cast(null as nvarchar(20)) as ManagerLastName,
		0 as Level
	from Employees
	where ReportsTo is null
	
	union all 
	
	select 	E.EmployeeID,
		E.FirstName,
		E.LastName,
		R.EmployeeID,
		R.FirstName,
		R.LastName,
		Level + 1
	from Employees E
	join EmployeesRecCTE R on E.ReportsTo = R.EmployeeID
	where Level < 1
	)
select	EmployeeID,
	FirstName,
	LastName,
	ReportsTo,
	ManagerFirstName, 
	ManagerLastName,
	case Level
		when 0 then 'Krzysiu Jarzyna ze Szczecina'
		when 1 then 'Mr. Frog'
	end as Level
from EmployeesRecCTE

--with maxrecursion = 1: "The maximum recursion 1 has been exhausted before statement completion."
--option (maxrecursion 1);

-- EX04
/* Extend the previous query so that in the ReportsTo column, instead of the identifier, it appears the value
from the supervisor's WhoIsThis column. This time present all levels of the hierarchy. Let the WhoIsThis column for the Level=2
take the value "Rezyser Kina Akcji". */

with EmployeesRecCTE (EmployeeID, FirstName, LastName, ReportsTo, ManagerFirstName, ManagerLastName, Level) as
	(
	select	EmployeeID,
		FirstName,
		LastName,
		ReportsTo,
		cast(null as nvarchar(10)) as ManagerFirstName, 
		cast(null as nvarchar(20)) as ManagerLastName,
		0 as Level
	from Employees
	where ReportsTo is null
	
	union all 
	
	select 	E.EmployeeID,
		E.FirstName,
		E.LastName,
		R.EmployeeID,
		R.FirstName,
		R.LastName,
		Level + 1
	from Employees E
	join EmployeesRecCTE R on E.ReportsTo = R.EmployeeID
	)
select	EmployeeID,
	FirstName,
	LastName,
	ReportsTo,
	ManagerFirstName, 
	ManagerLastName,
	case Level
		when 0 then 'Krzysiu Jarzyna ze Szczecina'
		when 1 then 'Mr. Frog'
		when 2 then 'Rezyser Kina Akcji'
	end as Level
from EmployeesRecCTE;

--EX05
/*Zadanie 5.
Using CTEs and recursions, build a query to represent the Fibonacci sequence.*/

with fib (N, FibValue, NextValue) as
	(
	select	0 as N,
			0 as FibValue,
			1 as NextValue
	
	union all 
	
	select	N+1,
			NextValue,
			FibValue + NextValue
	from fib
	where N <10
	)

select N, FibValue
from fib
--option(maxrecursion 10);
