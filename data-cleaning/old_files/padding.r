# pre_pad_id_series_nrow <- result[, uniqueN(id_series)]
# print(pre_pad_id_series_nrow)
# pre_pad_id_pin_nrow <- result[, uniqueN(id_pin)]
# # pre_paid_thai_drg_nrow <- result[, uniqueN(thai_drg)]
# # result[, thai_drg := str_pad(thai_drg, width = 5, side = "left", pad = "0")]
# result[, id_series := str_pad(id_series, width = 13, side = "left", pad = "0")]
# result[, id_pin := str_pad(id_pin, width = 20, side = "left", pad = "0")]
# post_pad_id_series_nrow <- result[, uniqueN(id_series)]
# post_pad_id_pin_nrow <- result[, uniqueN(id_pin)]
# # post_paid_thai_drg_nrow <- result[, uniqueN(thai_drg)]

# if (pre_pad_id_series_nrow != post_pad_id_series_nrow) stop("Error: id_series differs pre and post padding") else message("id_series nrow integrity valid")
# if (pre_pad_id_pin_nrow != post_pad_id_pin_nrow) stop("Error: id_series differs pre and post padding") else message("id_pin nrow integrity valid")
# # if (pre_paid_thai_drg_nrow != post_paid_thai_drg_nrow) stop("Error: thai_drg differs pre and post padding") else message("thai_drg nrow integrity valid")
# if (to_debug) fwrite(result, "test3.csv")
