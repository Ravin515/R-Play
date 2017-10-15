library(RODBC)

# 先导入所有需要用到的数据库 ----
# r 表示未作任何修改
# 没有前缀表示中间数据集
# 所有 info 类，每只基金只占 一行 ！！
pp <- odbcConnect("SMPP")

# fund_infomation
r.pp.info <- sqlQuery(pp, "select * from [dbo].[fund_info]") %>% setDT() # 基金状态表
pp.info <- r.pp.info[, .(pp.fund.id = fund_id, pp.fund.code = fund_code, pp.fund.name = fund_name, pp.fund.name.short = fund_short_name, pp.fund.type = fund_type, pp.currency = base_currency, pp.foundation.date = inception_date, pp.performance.start.date = performance_inception_date, pp.lockup.period = lockup_period, pp.duration = duration, pp.initial.size = initial_size)] %>% unique(by = "pp.fund.id")