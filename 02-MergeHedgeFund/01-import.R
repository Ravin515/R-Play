library(RODBC)

# PP ----
pp <- odbcConnect("SMPP")

# fund_infomation
r.pp.info <- sqlQuery(pp, "select * from [dbo].[fund_information]") %>% setDT() # 基金状态表
pp.info <- r.pp.info[, .(pp.fund.id = fund_id, pp.fund.code = fund_code, pp.fund.name = fund_name, pp.fund.name.short = fund_short_name, pp.fund.type = fund_type, pp.currency = base_currency, pp.foundation.date = inception_date, pp.performance.start.date = performance_inception_date, pp.lockup.period = lockup_period, pp.duration = duration, pp.initial.size = initial_size)]

# fund_status
r.pp.status <- sqlQuery(pp, "select * from [dbo].[fund_status]") %>% setDT() # 基金状态表
pp.status <- r.pp.status[, .(pp.fund.id = fund_id, pp.fund.status = fund_status, pp.liquidate.date = liquidate_date, pp.update.date = updatetime)]

# fund_strategy
r.pp.strategy <- sqlQuery(pp, "select * from [dbo].[fund_strategy]") %>% setDT()
pp.stategy <- r.pp.strategy[, .(pp.fund.id = fund_id, pp.strategy = strategy, pp.streategy.sub = substrategy)]

# nav
r.pp.nv <- sqlQuery(pp, "select * from [dbo].[nav]") %>% setDT()
pp.nv <- r.pp.nv[, .(pp.fund.id = fund_id, pp.date = price_date, pp.nv = nav, pp.cnv = cumulative_nav)]

# company_information
#r.pp.company.info <- sqlQuery(pp, "select * from [dbo].[company_information]") %>%  setDT() # 公司信息
#pp.company.info <- r.pp.company.info[, .(pp.company.id = company_id, pp.company.name = company_name, pp.company.name.short = company_short_name, pp.company.type = company_type)]
