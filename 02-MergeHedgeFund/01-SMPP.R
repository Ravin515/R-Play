library(RODBC)

# 先导入所有需要用到的数据库 ----
# r 表示未作任何修改
# 没有前缀表示中间数据集
# 所有 info 类，每只基金只占 一行 ！！
pp <- odbcConnect("SMPP")

# fund_infomation
r.pp.info <- sqlQuery(pp, "select * from [dbo].[fund_info]") %>% setDT() # 基金状态表
pp.info <- r.pp.info[, .(pp.fund.id = fund_id, pp.fund.code = fund_code, pp.fund.name = fund_name, pp.fund.name.short = fund_short_name, pp.fund.type = fund_type, pp.currency = base_currency, pp.foundation.date = inception_date, pp.performance.start.date = performance_inception_date, pp.lockup.period = lockup_period, pp.duration = duration, pp.initial.size = initial_size)] %>% unique(by = "pp.fund.id")

# fund_status
r.pp.status <- sqlQuery(pp, "select * from [dbo].[fund_status]") %>% setDT() # 基金状态表
pp.status <- r.pp.status[, .(pp.fund.id = fund_id, pp.fund.status = fund_status, pp.liquidate.date = liquidate_date, pp.update.date = updatetime)] %>%  unique(by = "pp.fund.id")

# fund_strategy
r.pp.strategy <- sqlQuery(pp, "select * from [dbo].[fund_strategy]") %>% setDT()
pp.strategy <- r.pp.strategy[, .(pp.fund.id = fund_id, pp.strategy = strategy, pp.streategy.sub = substrategy)] %>% unique(by = "pp.fund.id")

# nav
r.pp.nv <- sqlQuery(pp, "select * from [dbo].[nav]") %>% setDT()
pp.nv <- r.pp.nv[, .(pp.fund.id = fund_id, pp.date = price_date, pp.nv = nav, pp.cnv = cumulative_nav)] %>% unique(by = c("pp.fund.id", "pp.date"))

# company_information
#r.pp.company.info <- sqlQuery(pp, "select * from [dbo].[company_information]") %>%  setDT() # 公司信息
#pp.company.info <- r.pp.company.info[, .(pp.company.id = company_id, pp.company.name = company_name, pp.company.name.short = company_short_name, pp.company.type = company_type)]

# 合并 info类，得到 f.pp.info ----
# pp.fund.id 作为主键进行合并
f.pp.info <- pp.info[pp.strategy, on = .(pp.fund.id), nomatch = 0][pp.status, on = .(pp.fund.id), nomatch = 0]
sv(f.pp.info)


# 合并 nv 类，得到 f.pp.nv ----
f.pp.nv <- pp.nv
sv(f.pp.nv)
