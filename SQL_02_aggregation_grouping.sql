--AGGREGATION AND GROUPING

-- EX01
/* Using the Products table, display the maximum unit price of the available products (UnitPrice). */
select max(UnitPrice) as MaxUnitPrice
from Products


-- EX02
/* Using the Products and Categories tables, display the sum of the product values ​​in the warehouse (UnitPrice * UnitsInStock) divided into categories.
Sort the result by category (ascending). */
select top 5*
from Products

select	C.CategoryName,
		sum(P.UnitPrice * P.UnitsInStock) as StockValue
from Products as P
join Categories as C on C.CategoryID = P.CategoryID
group by C.CategoryName
order by C.CategoryName asc;


-- EX03
/* Extend the query from EX02 so that only the categories for which are presented the value of products exceeds 10000.
Sort the result descending by product value. */

select	C.CategoryName,
		sum(P.UnitPrice * P.UnitsInStock) as StockValue
from Products as P
join Categories as C on C.CategoryID = P.CategoryID
group by C.CategoryName
having sum(P.UnitPrice * P.UnitsInStock) > 10000
order by C.CategoryName asc;

-- EX04
/* Using the Suppliers, Products and Order Details tables, display information on how many unique orders appeared products
of a given supplier. Sort the results alphabetically by name suppliers. */
select top 5*
from Products

select top 5*
from Suppliers

select top 5*
from [Order Details]

select	S.CompanyName,
		count(distinct OD.OrderID) as NumberOfOrders
from Suppliers as S
join Products as P on P.SupplierID = S.SupplierID
join [Order Details] as OD on OD.ProductID = P.ProductID
group by CompanyName
order by CompanyName;

-- EX05
/* Using the Orders, Customers, and Order Details tables, show the average, minimum, and the maximum value of the order for each customer
(Customers.CustomerID). Sort the results according to the average value orders - descending. Remember to keep the average, minimum and maximum
order value calculated based on its value, i.e. the sum of the products of unit prices and the size of the order. */

--!!!WRONG SOLUTION!!!
select O.CustomerID, 
		avg(OD.UnitPrice * OD.Quantity) AverageOrder,
		min(OD.UnitPrice * OD.Quantity) MinOrder,
		max(OD.UnitPrice * OD.Quantity) MaxOrder
from [Order Details] as OD
join Orders as O on O.OrderID = OD.OrderID
group by O.CustomerID
order by avg(OD.UnitPrice * OD.Quantity);

--Correct:
with OrderSumPerID (OrderID, OrderSum) as
	(
	select	OD.OrderID,
			sum(OD.UnitPrice * OD.Quantity) as OrderSum
	from [Order Details] OD
	join Orders O on O.OrderID = OD.OrderID
	group by OD.OrderID
	)

select	O.CustomerID,
		avg(OrderSum) as AverageOrder,
		min(OrderSum) as MinOrder,
		max(OrderSum) as MaxOrder
from OrderSumPerID as OSPI
join Orders as O on O.OrderID = OSPI.OrderID
group by O.CustomerID
order by avg(OrderSum) desc;

-- EX06
/* Using the Orders table, display the dates (OrderDate) where there was more than one order taking into account the exact number of orders. 
Display the order date in the format YYYY-MM-DD. Result sort descending by number of orders. */
select *
from Orders

select	cast(OrderDate as date) as OrderDate,
		count(distinct OrderID) as CNT
from Orders
group by OrderDate
having count(distinct OrderID) > 1
order by CNT desc;

-- EX07
/* Using the Orders table, analyze the number of orders in 3 dimensions: Year and month, year and overall summary.
Sort the result by "Year-Month" (descending). */

select	datepart(year, OrderDate) 'Year',
		datepart(mm, OrderDate) 'Month'
from Orders

--07A:
select	format(OrderDate, 'yyyy') 'Year',
		format(OrderDate, 'yyyy-MM') 'Year-Month',
		count(distinct OrderID) CNT
from Orders
group by rollup	(format(OrderDate, 'yyyy'),
				format(OrderDate, 'yyyy-MM'))
order by 'Year-Month' desc, 'Year' desc;

--07B (less clear to read data imho)
select	datepart(year, OrderDate) as 'Year',
		datepart(mm, OrderDate) as 'Month',
		count(distinct OrderID) as CNT
from Orders
group by rollup (datepart(year, OrderDate),
				datepart(mm, OrderDate))
order by 'Year' desc;


-- EX08
/* Using the Orders table, analyze the number of orders in following dimensions:
	- Country, region and city of delivery
	- Country and region of delivery
	- Country of delivery
	- Summary
Add a GroupingLevel column to explain the grouping level that's for each dimension will assume the following values:
	- Country & Region & City
	- Country & Region
	- Country
	- Total
The region field may have empty values - mark such values as "Not Provided".
Sort the result alphabetically according to the country of delivery. */

--from lab:

select	ShipCountry,
		
		case 
			when grouping (ShipRegion) = 0 and ShipRegion is null then 'Not provided'
			else ShipRegion
		end ShipRegion,
		ShipCity,
		count(1) as CNT,
		
		case grouping_id (ShipCountry,ShipRegion,ShipCity)
			when 0 then 'Country & Region & City'
			when 1 then 'Country & Region'
			when 3 then 'Country'
			when 7 then 'Total'
		end as GroupingLevel
from orders
group by rollup (ShipCountry,ShipRegion,ShipCity)
order by ShipCountry

-- EX09
/* Using the tables Orders, Order Details and Customers, present a full analysis of the sum of the value of orders
in following dimensions:
	- Year (Order.OrderDate)
	- Customer (Customers.CompanyName)
	- Overall summary
Include only records that have all the required information (no external joins needed).
Sort the result by customer name (alphabetically). */

select	format(O.OrderDate, 'yyyy') as 'Year',
		C.CompanyName,
		sum(OD.Quantity * OD.UnitPrice) as OrdersValue
from Orders O 
join Customers C on C.CustomerID = O.CustomerID
join [Order Details] OD on OD.OrderID = O.OrderID

group by cube (format(O.OrderDate, 'yyyy'),
				C.CompanyName)
order by C.CompanyName;


-- EX10
/* Modify the previous query to include the country instead of the name (Customers.Country) and region (Customers.Region)
of the customer (the dimension should consist of two: country and region; summary should not be counted separately
for country and region). Sort the results by country name (alphabetically).*/

select	format(O.OrderDate, 'yyyy') as 'Year',
		C.Country as Country,
		C.Region as Region,
		sum(OD.Quantity * OD.UnitPrice) as OrdersValue
from Orders O 
join Customers C on C.CustomerID = O.CustomerID
join [Order Details] OD on OD.OrderID = O.OrderID

group by cube (format(O.OrderDate, 'yyyy'),
				(C.Country,
				C.Region))
order by C.Country;

-- EX11
/* Use the Orders, Orders Details, Customers, Products, Suppliers, and Categories tables to represent
analysis of the total value of orders (without discounts) for specific dimensions:
	- Categories (Cateogires.CategoryName)
	- Supplier country (Suppliers.Country)
	- Customer country and region (Customers.Country, Customers.Region)
Dimensions consisting of more than one attribute should be treated as a whole (no groupings for subsets).
Don't generate additional summaries - include them carefully in dimensions listed above.
Add a GroupingLevel field to the result explaining the level of grouping that will take the values
respectively for individual dimensions:
	- Category
	- Country-Supplier
	- Country & Region - Customer
Sort the result alphabetically first by GroupingLevel column (ascending) and then
after the column with the sum of the order values OrdersValue (descending). */

--from labs:
select Cat.CategoryName,
       S.Country,
       C.Country,
       C.Region,
       SUM(OD.Quantity * OD.UnitPrice) as OrdersValue,
       case grouping_id(Cat.CategoryName,
                        S.Country,
                        C.Country,
                        C.Region)
           when 7 then 'Category'
           when 11 then 'Country - Supplier'
           when 12 then 'Country & Region - Customer'
           end as GroupingLevel
from Orders O
         join [Order Details] OD on O.OrderID = OD.OrderID
         join Customers C on O.CustomerID = C.CustomerID
         join Products P on OD.ProductID = P.ProductID
         join Categories Cat on P.CategoryID = Cat.CategoryID
         join Suppliers S on P.SupplierID = S.SupplierID
group by grouping sets ((Cat.CategoryName), (S.Country), (C.Country, C.Region))
order by GroupingLevel, sum(OD.Quantity * OD.UnitPrice) desc;

-- EX12
/* Using the Orders and Shippers tables, present a table containing the number of completed orders
to a given country (ShipCountry) by a given transport company. Enter the country of delivery as the rows a
as supplier columns. Sort the result by the name of the country of delivery (alphabetically). */

select * from Shippers

--Query to produce the data
select	O.ShipCountry as Country,
		S.CompanyName as Company,
		count(distinct O.OrderID) CNT
from Orders O
join Shippers S on S.ShipperID = O.ShipVia
group by O.ShipCountry, S.CompanyName

--PIVOT query:
select	[Country], [Speedy Express], [United Package], [Federal Shipping]
from
	(select	O.ShipCountry as Country,
			S.CompanyName as Company,
			count(distinct O.OrderID) CNT
	from Orders O
	join Shippers S on S.ShipperID = O.ShipVia
	group by O.ShipCountry, S.CompanyName
	) Query
PIVOT
(
sum(Query.CNT)
for Company IN ([Speedy Express], [United Package], [Federal Shipping])
) as CNT

-- EX13
/*Including the Order Details table, update the previous query so that instead of a number
completed orders, the sum of the value of orders handled by a given company appeared
freight forwarded to your country.*/

--Query to produce the data
select	O.ShipCountry as Country,
		S.CompanyName as Company,
		sum(OD.Quantity * OD.UnitPrice) Total
from Orders O
join Shippers S on S.ShipperID = O.ShipVia
join [Order Details] OD on OD.OrderID = O.OrderID
group by O.ShipCountry, S.CompanyName

--PIVOT query:
select	[Country], [Speedy Express], [United Package], [Federal Shipping]
from
	(select	O.ShipCountry as Country,
		S.CompanyName as Company,
		sum(OD.Quantity * OD.UnitPrice) Total
	from Orders O
	join Shippers S on S.ShipperID = O.ShipVia
	join [Order Details] OD on OD.OrderID = O.OrderID
	group by O.ShipCountry, S.CompanyName) Query
PIVOT
(
sum(Query.Total)
for Company IN ([Speedy Express], [United Package], [Federal Shipping])
) as Total