-- SQL BASICS --

-- EX01
/* Using the Products and Categories tables, display the product name (Products.ProductName) and the name of the category
(Categories.CategoryName) to which the product belongs. Sort the result by product name (ascending). */

select	P.ProductName,
	C.CategoryName
from Products as P
join Categories as C on C.CategoryID = P.CategoryID
order by P.ProductName asc;

-- EX02
/* Using the Suppliers table, expand the previous one to also present the name of the supplier of a given product (CompanyName),
name the column SupplierName. Sort the result descending by the unit price of the product. */

select	P.ProductName,
	C.CategoryName,
	S.CompanyName as SupplierName
from Products P
join Categories C on C.CategoryID = P.CategoryID
join Suppliers S on S.SupplierID = P.SupplierID
order by UnitPrice desc;

-- EX03
/* Using the Products table, display the product names (ProductName) with the highest unit price in a given category (UnitPrice).
Sort the result by product name (ascending). */

with MaxPriceInCat (MaxPrice, CategoryID) as
	(
	select	max(P.UnitPrice) as MaxPrice,
		C.CategoryID 
	from Products as P
	join Categories as C on C.CategoryID = P.CategoryID
	group by C.CategoryID
	)

select	P.ProductName,
	P.UnitPrice
from Products as P
join MaxPriceInCat as MPIC on MPIC.MaxPrice = P.UnitPrice
where P.CategoryID = MPIC.CategoryID
order by ProductName

-- w/o CTE (from labs):
select	P1.ProductName,
	P1.UnitPrice as MaxPriceInCat
from Products P1
where UnitPrice = (select max(UnitPrice)
			from Products P2
			where P1.CategoryID = P2.CategoryID)
order by P1.ProductName;

-- EX04
/* Using the Products table, display the names of products whose unit price is greater than all average product prices
calculated for other categories (other than the one to which the product belongs). Sort the result by unit price (descending). */

-- AVG price per Category:
select	CategoryID,
	avg(UnitPrice) as AveragePrice
from Products
group by CategoryID;

-- 04A:
select	P1.ProductName,
	P1.UnitPrice,
	P1.CategoryID
from Products as P1 
where UnitPrice > all(select avg(P2.UnitPrice)
			from Products as P2
			join Categories as C2 on P2.CategoryID = C2.CategoryID
			where P1.CategoryID != P2.CategoryID
			group by CategoryName)
order by P1.UnitPrice desc;

-- 04BB (from labs):
select	P1.ProductName,
	P1.UnitPrice,
	P1.CategoryID
from Products P1
where UnitPrice > all(select avg(P2.UnitPrice)
			from Products P2
			where P1.CategoryID != P2.CategoryID
			group by P2.CategoryID) 
order by P1.UnitPrice desc;


-- EX05
/* Using the Order Details table, extend the previous query to display as well the maximum number of ordered items
(MaxQuantity) of a given product in one order (in a given OrderID). */

with MaxPriceInOrder (ProductID, MaxQuantity) as
	(
	select	ProductID,
		max(quantity) as MaxQuantity
	from [Order Details]
	group by ProductID
	)

select	P1.ProductName,
	P1.UnitPrice,
	P1.CategoryID,
	MPIO.MaxQuantity
from Products P1
join MaxPriceInOrder as MPIO on MPIO.ProductID = P1.ProductID
where UnitPrice > all(select avg(P2.UnitPrice)
			from Products P2
			where P1.CategoryID != P2.CategoryID
			group by P2.CategoryID) 
order by P1.ProductName desc;

-- EX06
/* Using the Products and Order Details tables, display the Category IDs and the sum of all values orders of products
in a given category ([Order Details].UnitPrice * [OrderDetails].Quantity) without discount. The result should contain
only those categories for which the above the sum is greater than 200,000. Sort result by sum of order values ​​(descending)*/

select	P.CategoryID,
	sum(OD.UnitPrice * OD.Quantity) as TotalQuantity
from [Order Details] as OD
join Products as P on P.ProductID = OD.ProductID
group by P.CategoryID
having sum(OD.UnitPrice * OD.Quantity) > 200000

-- EX07
/* Using the Categories table, update the previous query to return except category ID as well as its name.*/
-- 07A:
select	P.CategoryID,
	C.CategoryName,
	sum(OD.UnitPrice * OD.Quantity) as TotalQuantity
from [Order Details] as OD
join Products as P on P.ProductID = OD.ProductID
join Categories as C on C.CategoryID = P.CategoryID
group by P.CategoryID, C.CategoryName
having sum(OD.UnitPrice * OD.Quantity) > 200000

-- 07B:
with TotalQuantityPerCategory (CategoryID, TotalQuantity) as
	(
	select	P.CategoryID,
		sum(OD.UnitPrice * OD.Quantity) as TotalQuantity
	from [Order Details] as OD
	join Products as P on P.ProductID = OD.ProductID
	group by P.CategoryID
	having sum(OD.UnitPrice * OD.Quantity) > 200000
	)

select	C.CategoryID,
	CategoryName,
	TQPC.TotalQuantity
from Categories as C
join TotalQuantityPerCategory as TQPC
on C.CategoryID = TQPC.CategoryID

-- EX08
/* Using the Orders and Employees tables, display the number of orders that have been shipped (ShipRegion) to regions other
than those in orders handled by a Robert King employee (FirstName -> Robert; LastName -> King).*/

select *
from Employees
where LastName = 'King' and FirstName = 'Robert';

select *
from Orders

select distinct isnull(O.ShipRegion,'no region') as ShipReg
from Orders as O
join Employees as E on E.EmployeeID = O.EmployeeID
where (LastName = 'King' and FirstName = 'Robert')

-- 08A
select count(*) as CNT
from Orders
where ShipRegion not in (select distinct isnull(O.ShipRegion,'no region') as ShipReg
			from Orders as O
			join Employees as E on E.EmployeeID = O.EmployeeID
			where (LastName = 'King' and FirstName = 'Robert'))
		or ShipRegion is null;

-- 08B (from labs):
select count(*) as CNT
from Orders O
where not exists (select 1 
		from Orders P
		join Employees E on P.EmployeeID = E.EmployeeID
		where P.ShipRegion = O.ShipRegion
		and E.FirstName = 'Robert' AND E.LastName = 'King')


-- EX09
/* Using the Orders table, display all shipping countries (ShipCountry) for which they exist
records (orders) that have a filled value in the ShipRegion field as well as records with a NULL value.*/

select distinct ShipCountry
from Orders
where shipcountry in (select ShipCountry
			from Orders 
			where Shipregion is null)
			and shipregion is not null

-- 09B (from labs):
select ShipCountry
from Orders
where ShipRegion IS NULL

intersect

select ShipCountry
from Orders
where ShipRegion IS NOT NULL

-- EX10
/* Using the appropriate tables, display the product ID (Products.ProductID), product name (Products.ProductName),
supplier country and city (Suppliers.Country, Suppliers.City - name them accordingly: SupplierCountry and SupplierCity) 
and the country and city of delivery of the given product (Orders.ShipCountry, Orders.ShipCity).

Limit the score to products that have been shipped at least once to the same country as their supplier.
Additionally, extend the result with information whether, in addition to the country, the city where the product supplier
is located also matches, with the city to which the product was shipped - name the column FullMatch, which will assume Y/N values.
Sort the result so that the alphabetically sorted products for which there is a full match are displayed first */

select distinct P.ProductID,
		P.ProductName,
		S.Country SupplierCountry,
		S.City SupplierCity,
		O.ShipCountry,
		O.ShipCity,
				
		case
			when O.ShipCity = S.City then 'Y'
			else 'N'
			end as FullMatch

from Products as P
join Suppliers as S on S.SupplierID = P.SupplierID
join [Order Details] as OD on OD.ProductID = P.ProductID
join Orders as O on O.OrderID = OD.OrderID
where S.Country = O.ShipCountry
order by FullMatch desc, P.ProductName asc;

-- EX11
/* Expand the previous query to include the delivery region as well as the shipping region. The FullMatch column should have the following set of values:
	- Y: for full compliance of three values
	- N (the region doesn't match): for matching country and city, but not region
	- N: for non-compliance
Add also the fields containing the region to the result: Suppliers.Region (name them SupplierRegion) and Orders.ShipRegion) */

select distinct P.ProductID,
		P.ProductName,
		S.Country SupplierCountry,
		S.City SupplierCity,
		S.Region SupplierRegion,
		O.ShipCountry,
		O.ShipCity,
		O.ShipRegion,
		
		case
			when O.ShipCity = S.City and isnull(S.Region,0) = isnull(O.ShipRegion,0) then 'Y'
			when O.ShipCity = S.City and isnull(S.Region,0) != isnull(O.ShipRegion,0) then 'N (the region doesn''t match)'
			else 'N'
			end as FullMatch

from Products as P
join Suppliers as S on S.SupplierID = P.SupplierID
join [Order Details] as OD on OD.ProductID = P.ProductID
join Orders as O on O.OrderID = OD.OrderID
where S.Country = O.ShipCountry
order by FullMatch desc, P.ProductName asc;

-- EX12
/* Using the Products table, verify that there are two (or more) products with the same name.
The query should return either Yes or No in the DuplicatedProductsFlag column.*/

select distinct

	case
	when (select distinct count(*) from Products) = (select count(*) from Products) then 'No'
	else 'Yes'
	end as DuplicatedProductsFlag 

from Products

-- EX13
/* Using the Products and Order Details tables, display the names of the products along with information on how many orders
the given products appeared on. Sort the result so that the products that appear on orders most often appear first. */

select	P.ProductName,
	count(OD.OrderID) as NumberOfOrders
from Products as P
join [Order Details] as OD on OD.ProductID = P.ProductID
group by P.ProductName
order by NumberOfOrders desc;

-- EX14
/* Using the Orders table, expand the previous query so that the above analysis is presented in the context of individual years (Orders.OrderDate)
- name the column OrderYear. -- This time, sort the result so that the most common products are displayed first
-- appearing on orders in the context of a given year, i.e. we are first interested in the year: 1996, then 1997, etc. */

select	P.ProductName,
	year(O.OrderDate) as OrderYear,
	count(OD.OrderID) as NumberOfOrders
from Products as P
join [Order Details] as OD on OD.ProductID = P.ProductID
join Orders as O on O.OrderID = OD.OrderID
group by P.ProductName, year(O.OrderDate)
order by OrderYear asc, NumberOfOrders desc;

-- EX15
/* Using the Suppliers table, expand the query to display additionally for each product name of the supplier
of a given product (Suppliers.CompanyName) - name the column SupplierName. */ 

select	P.ProductName,
	S.CompanyName as SupplierName,
	year(O.OrderDate) as OrderYear,
	count(OD.OrderID) as NumberOfOrders
from Products as P
join [Order Details] as OD on OD.ProductID = P.ProductID
join Orders as O on O.OrderID = OD.OrderID
join Suppliers as S on S.SupplierID = P.SupplierID
group by P.ProductName, S.CompanyName, year(O.OrderDate)
order by OrderYear asc, NumberOfOrders desc;

