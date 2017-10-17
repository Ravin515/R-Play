# �ϲ� info���������� hf.info ----
ld(f.pp.info)
ld(f.zy.info)
ld(f.dzh.info)
# ��factorת��Ϊchar�ĺ���
factor2char <- function(x) {
    if (is.factor(x)) {
        as.character(x)
    } else x
}
# Ϊÿ��������ͬ������ fund.name / fund.name.short ���ںϲ�
pp.info <- f.pp.info[, ":="(db.name = "SMPP", fund.name = pp.fund.name, fund.name.short = pp.fund.name.short)][, lapply(.SD, factor2char)]
zy.info <- f.zy.info[, ":="(db.name = "ZYYX", fund.name = zy.fund.name, fund.name.short = zy.fund.name.short)][, lapply(.SD, factor2char)]
dzh.info <- f.dzh.info[, ":="(db.name = "DZH", fund.name = dzh.fund.name, fund.name.short = dzh.fund.name.short)][, lapply(.SD, factor2char)]
rm(f.pp.info, f.zy.info, f.dzh.info)
# �����б��ϲ��� hf.info
clean_fund_name <- function(x) {
    str_replace_all(x, '[����"����\\(��\\)��\\-&\\?\\[\\]]+', " ")
}
hf.info <- rbindlist(list(pp.info, zy.info, dzh.info), use.names = T, fill = T)[order(fund.name, db.name)
    ][, ":="(fund.name = clean_fund_name(fund.name), fund.name.short = clean_fund_name(fund.name.short)) # �� fund.name���е���������滻�ɿո�
    ][, ":="(ndup = .N), keyby = .(fund.name)] # ndup��ʾ����ÿ��fund.name���ظ����м���
# reorder column order, put fund.name, ndup, db.name at first
new.colorder <- c(c("fund.name", "ndup", "db.name"), names(hf.info)[!(names(hf.info) %in% c("fund.name", "ndup", "db.name"))])
setcolorder(hf.info, new.colorder)
# ���һ�������ж��й۲⣬��֮�ϲ���ͬһ�У�ͬʱ�޳�NA
select_unique_non_na <- function(x) {
    x[!is.na(x)][1]
}
hf.info <- hf.info[, ":="(db.name = str_c(db.name, collapse = " ")), keyby = .(fund.name)][, lapply(.SD, select_unique_non_na), keyby = .(fund.name)]
sv(hf.info)

# �ϲ� nv���������� hf.nv ----
# ����ÿ����
ld(f.pp.nv)
ld(f.zy.nv)
ld(f.dzh.nv)
ld(hf.info)
# ÿ������ǳ�db.name
pp.nv <- f.pp.nv[, ":="(db.name = "SMPP")][hf.info[, .(fund.name, pp.fund.id)], on = .(pp.fund.id), nomatch = 0]
setnames(pp.nv, "pp.date", "date")
zy.nv <- f.zy.nv[, ":="(db.name = "ZYYX")][hf.info[, .(fund.name, zy.fund.id)], on = .(zy.fund.id), nomatch = 0]
setnames(zy.nv, "zy.date", "date")
dzh.nv <- f.dzh.nv[, ":="(db.name = "DZH")][hf.info[, .(fund.name, dzh.fund.id)], on = .(dzh.fund.id), nomatch = 0]
setnames(dzh.nv, "dzh.date", "date")
#rm(f.pp.nv, f.zy.nv, f.dzh.nv)

# Ϊhf.nv����fund.name
hf.nv <- rbindlist(list(pp.nv, zy.nv, dzh.nv), use.names = T, fill = T)[order(fund.name, date)]

# reorder columns, put fund.name first
new.colorder <- c(c("fund.name", "date", "db.name"), names(hf.nv)[!(names(hf.nv) %in% c("fund.name", "date", "db.name"))])
setcolorder(hf.nv, new.colorder)
hf.nv[, ":="(date = as.Date(date))]