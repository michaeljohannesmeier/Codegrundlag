zipCodes <- zipCodes[, -ncol(zipCodes)]
zipCodesSearch <- zipCodes[order(zipCodes["Postal.Code"]),1:5]
zipCodesSearch <- do.call(paste ,zipCodesSearch)
zipCodesSearch <- zipCodesSearch[-1]

colorListLinesMarkers <- c("#3488EE", "#F3B627", "#165F16", "#D42F2F", "#CE2FD4")
colorListBars <- c("#AAB0B4", "#292B29")
