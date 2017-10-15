library(RODBC)

# 先导入所有需要用到的数据库 ----
# r 表示未作任何修改
# 没有前缀表示中间数据集
# 所有 info 类，每只基金只占 一行 ！！
zy <- odbcConnect("ZYYX")

# fund_infomation
r.zy.info <- sqlQuery(zy, "select * from [dbo].[t_fund_info]") %>% setDT() # 基金状态表
zy.info <- r.zy.info
setnames(zy.info, names(zy.info), str_c("zy.", names(zy.info)))
setnames(zy.info, names(zy.info), str_replace_all(names(zy.info), "_", "."))
setnames(zy.info, c("zy.fund.name", "zy.fund.full.name"), c("zy.fund.name.short", "zy.fund.name"))
f.zy.info <- zy.info
sv(f.zy.info)

# fund_nv
r.zy.nv <- sqlQuery(zy, "select * from [dbo].[t_fund_daily_nv_statistic]") %>% setDT() # 41830459
sv(r.zy.nv)
zy.nv <- r.zy.nv
setnames(zy.nv, names(zy.nv), str_c("zy.", names(zy.nv)))
setnames(zy.nv, names(zy.nv), str_replace_all(names(zy.nv), "_", "."))
f.zy.nv <- zy.nv[, .(zy.fund.id, zy.date = zy.statistic.date, zy.nv = zy.nav, zy.cnv = zy.swanav, zy.source = str_sub(zy.source, 1, 1))]
sv(f.zy.nv)
