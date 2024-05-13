conn = localDBConn(paste0(path,"output"), SCE_NAME)


# get data ----
na_query = '<query title="National Account">
    <axis1 name="Account">account[@name]</axis1>
    <axis2 name="Year">nationalAccount[@year]</axis2>
    <xPath buildList="true" dataName="all accounts" group="false" sumAll="false">nationalAccount/account/node()</xPath>
    <comments/>
</query>'

na_data = runQuery(conn, na_query, c(), c())

service_query = '<marketQuery title="price of a specific market">
    <axis1 name="market">market</axis1>
    <axis2 name="Year">market</axis2>
    <xPath buildList="true" dataName="price" group="false" sumAll="false">Marketplace/market[true() and contains(@name,\'service\')]/price/node()</xPath>
    <comments/>
</marketQuery>'
service_data = runQuery(conn, service_query, c(), c())

labor_price_query = '<marketQuery title="price of a specific market">
    <axis1 name="market">market</axis1>
    <axis2 name="Year">market</axis2>
    <xPath buildList="true" dataName="price" group="false" sumAll="false">Marketplace/market[true() and contains(@name,\'Labor\')]/price/node()</xPath>
    <comments/>
</marketQuery>'
labor_price_data = runQuery(conn, labor_price_query, c(), c())

labor_demand_query = '<marketQuery title="demand of a specific market">
    <axis1 name="market">market</axis1>
    <axis2 name="Year">market</axis2>
    <xPath buildList="true" dataName="demand" group="false" sumAll="false">Marketplace/market[true() and contains(@name,\'Labor\')]/demand/node()</xPath>
    <comments/>
</marketQuery>'
labor_demand_data = runQuery(conn, labor_demand_query, c(), c())

price_k_query = '<marketQuery title="price of a specific market">
    <axis1 name="market">market</axis1>
    <axis2 name="Year">market</axis2>
    <xPath buildList="true" dataName="price" group="false" sumAll="false">Marketplace/market[true() and ends-with(@name,\'K\')]/price/node()</xPath>
    <comments/>
</marketQuery>'
price_k_data = runQuery(conn, price_k_query, c(), c())

demand_k_query = '<marketQuery title="demand of a specific market">
    <axis1 name="market">market</axis1>
    <axis2 name="Year">market</axis2>
    <xPath buildList="true" dataName="demand" group="false" sumAll="false">Marketplace/market[true() and ends-with(@name,\'K\')]/demand/node()</xPath>
    <comments/>
</marketQuery>'
demand_k_data = runQuery(conn, demand_k_query, c(), c())

s_invest_query = '<supplyDemandQuery title="Capital investment demands by tech">
    <axis1 name="sector">sector[@name]</axis1>
    <axis2 name="Year">technology[@year]</axis2>
    <xPath buildList="true" dataName="capital" group="false" sumAll="false">*[@type=\'sector\']//*[@type=\'input\' (: collapse :)]/capital/text()</xPath>
    <comments>Note: these are investment demands per timestep</comments>
</supplyDemandQuery> '
s_invest_data = runQuery(conn, s_invest_query, c(), c())

r_invest_query = '<supplyDemandQuery title="Capital investment demands by resource">
    <axis1 name="technology">technology[@name]</axis1>
    <axis2 name="Year">technology[@year]</axis2>
    <xPath buildList="true" dataName="capital" group="false" sumAll="false">*[@type=\'resource\']/*[@type = \'subresource\']/*[@type = \'technology\']/*[@type=\'input\' (: collapse :)]/capital/text()</xPath>
    <comments>Note: these are investment demands per timestep</comments>
</supplyDemandQuery>'
r_invest_data = runQuery(conn, r_invest_query, c(), c())

export_query = '<supplyDemandQuery title="Tracking accounts by tech">
    <axis1 name="technology">technology[@name]</axis1>
    <axis2 name="Year">currency-output[@vintage]</axis2>
    <xPath buildList="true" dataName="account" group="false" sumAll="false">*[@type=\'sector\']//*[@type=\'subsector\']/*[@type=\'technology\']/*[@type=\'output\' and @name=\'energy net export\']/currency-output/node()</xPath>
    <comments/>
</supplyDemandQuery>'
export_data = runQuery(conn, export_query, c(), c("USA"))

import_query = '<supplyDemandQuery title="inputs by sector">
    <axis1 name="sector">sector</axis1>
    <axis2 name="Year">demand-physical[@vintage]</axis2>
    <xPath buildList="true" dataName="input" group="false" sumAll="false">*[@type=\'sector\']//*[@type=\'input\' and contains(@name, \'traded\')]/demand-physical/node()</xPath>
    <comments/>
</supplyDemandQuery>'
import_data = runQuery(conn, import_query, c(), c())

trade_price_query = '<marketQuery title="price of a specific market">
    <axis1 name="market">market</axis1>
    <axis2 name="Year">market</axis2>
    <xPath buildList="true" dataName="price" group="false" sumAll="false">Marketplace/market[true() and contains(@name,\'traded\')]/price/node()</xPath>
    <comments/>
</marketQuery>'
trade_price_data = runQuery(conn, trade_price_query, c(), c("USA"))
