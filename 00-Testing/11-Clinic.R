clinic <- fread("zhenduan.csv", encoding = "UTF-8", na.strings = "")
clinic <- clinic[, str_c(colnames(clinic)[2:23], "_xtrct") := lapply(.SD[, 2:23], str_match, "¼Ì·¢ÐÔÈ©¹ÌÍª|È©¹ÌÍª")]
clinic[is.na(clinic)] <- "ÎÞ"
clinic <- clinic[rowMeans(clinic[, 31:52] == "È©¹ÌÍª") > 0 & rowMeans(clinic[, 31:52] != "¼Ì·¢ÐÔÈ©¹ÌÍª") == 1]

ALD_supine_remark <- fread("ALD_supine_remark.csv", encoding = "UTF-8")
ALD_supine_value <- fread("ALD_supine_value.csv", encoding = "UTF-8")
ALD_supine_remark[, ald_supine_prescribe_time_match := str_sub(ald_supine_prescribe_time, end = 16)]
ALD_supine_value[, ald_supine_prescribe_time_match := str_sub(ald_supine_prescribe_time, end = 16)]
cj <- ALD_supine_remark[ALD_supine_value, on = .(id, ald_supine_prescribe_time_match), nomatch = 0]
cj[, ':='(V1 = NULL, i.V1 = NULL, ald_supine_prescribe_time_match = NULL)
    ][!is.na(ald_supine_prescribe_time), i.ald_supine_prescribe_time := ald_supine_prescribe_time]
setnames(cj, c(2, 4), c("ald_supine_prescribe_time_remark", "ald_supine_prescribe_time_value"))
cj[, uniqueN(id)]
fwrite(cj, "cj.csv")