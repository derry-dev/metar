identify_METAR_token <- function(i) {

  for (strType in names(METAR_regex)) {
    if (grepl(METAR_regex[[strType]], i)) return(strType)
  }

  if (i %in% METAR_miscwxTokens) {
    return("wx")
  }

  return("unknown")

}

split_METAR_token <- function(regex_str, i) {

  splits <- table(unlist(strsplit(regex_str, split = "")))[["("]]

  x <- unlist(strsplit(gsub(regex_str, paste(paste0("\\", seq(1, splits, 1)), collapse = "_"), i), split = "_"))

  if (length(x) == splits) {
    return(x)
  } else {
    return(NA)
  }
  return(NULL)

}

parse_METAR_token <- function(i) {

  tokenType <- identify_METAR_token(i)
  outList <- list(token_type = tokenType)
  x <- split_METAR_token(METAR_regex[[tokenType]], i)

  if (length(x) == 1 & is.na(x[1])) {

    outList[[paste0(tokenType, "_unknown")]] <- i

  } else if (tokenType == "trend") {

    outList[["trend"]] <- x[1]

  } else if (tokenType == "time") {

    outList[["day"]] <- as.numeric(x[1])
    outList[["hour"]] <- as.numeric(x[2])
    outList[["min"]] <- as.numeric(x[3])

  } else if (tokenType == "wind") {

    outList[["wind_hdg"]] <- ifelse(x[1] == "VRB", NA, as.numeric(x[1]))
    outList[["wind_vrb"]] <- ifelse(x[1] == "VRB", T, F)
    outList[["wind_speed"]] <- as.numeric(x[2])
    outList[["wind_gust"]] <- ifelse(nchar(x[3]) == 0, NA, as.numeric(substring(x[3], 2)))
    outList[["wind_unit"]] <- x[4]

  } else if (tokenType == "wind_vdir") {

    outList[["wind_hdg_min"]] <- as.numeric(x[1])
    outList[["wind_hdg_max"]] <- as.numeric(x[2])

  } else if (tokenType == "vsby") {

    outList[["vsby_dist"]] <- as.numeric(x[1])

  } else if (tokenType == "dvsby") {

    outList[["vsby_dir_dist"]] <- as.numeric(x[1])
    outList[["vsby_dir_hdg"]] <- ifelse(nchar(x[2] == 0), NA, x[2])

  } else if (tokenType == "rvr") {

    outList[["rvr_rwy"]] <- x[1]
    outList[["rvr_code"]] <- ifelse(nchar(x[2] == 0), NA, x[2])
    outList[["rvr_vis"]] <- as.numeric(x[3])
    outList[["rvr_trend"]] <- ifelse(nchar(x[4] == 0), NA, x[4])

  } else if (tokenType == "wx") {

    outList[["wx"]] <- paste(x, collapse = "")

  } else if (tokenType == "cloud") {

    outList[["skyc"]] <- x[1]
    outList[["skyl"]] <- ifelse(x[2] == "///", NA, as.numeric(paste0(x[2], "00")))

  } else if (tokenType == "cloud_conv") {

    outList[["skyc"]] <- x[1]
    outList[["skyl"]] <- ifelse(x[2] == "///", NA, as.numeric(paste0(x[2], "00")))
    outList[["skyx"]] <- x[3]

  } else if (tokenType == "cloud_alt") {

    outList[["skyc"]] <- x[1]

  } else if (tokenType == "vv") {

    outList[["vv"]] <- ifelse(x[1] == "///", NA, as.numeric(paste0(x[1], "00")))

  } else if (tokenType == "tempdew") {

    outList[["tmp"]] <- ifelse(x[1] == "M", as.numeric(paste0("-", x[2])), as.numeric(x[2]))
    outList[["dwp"]] <- ifelse(x[3] == "M", as.numeric(paste0("-", x[4])), as.numeric(x[4]))

  } else if (tokenType == "qnh") {

    outList[["qnh"]] <- as.numeric(x)

  } else if (tokenType == "apt" & is.null(outList[["apt"]])) {

    outList[["apt"]] <- x

  } else if (tokenType == "unknown") {

    outList[["unknown"]] <- x

  }
  return(outList)

}

parse_METAR <- function(metar, out_format = "data.frame") {

  metar <- strsplit(metar, split = " ")
  metarList <- list()

  for (k in 1:length(metar)) {

    # Remove string "=" from metar
    metar_k <- gsub("=", "", metar[[k]])

    parsed_k <- list()
    parsed_typeList <- c()

    for (i in 1:length(metar_k)) {

      parsed_token_i <- parse_METAR_token(metar_k[i])
      parsed_type_i <- as.character(parsed_token_i[1])
      parsed_info_i <- parsed_token_i[2:length(parsed_token_i)]

      # Deal with duplicate names --- WIP
      if (parsed_type_i %in% parsed_typeList & !grepl(METAR_duphandle[["increment"]], parsed_type_i)) {

        if (grepl(METAR_duphandle[["merge"]], parsed_type_i)) { # Merge with existing
          parsed_k[[parsed_type_i]] <- paste(parsed_k[[parsed_type_i]], parsed_info_i[[parsed_type_i]], collapse = " ")
        } else if (grepl(METAR_duphandle[["ignoreextra"]], parsed_type_i)) { # Ignore duplicates

        } else { # Catch unhandled duplicates
          parsed_k[["unhandled"]] <- paste(append(parsed_k[["unhandled"]], metar_k[i]), collapse = " ")
        }

      } else {

        if (grepl(METAR_duphandle[["increment"]], parsed_type_i)) { # Append increment numbering
          names(parsed_info_i) <- paste0(names(parsed_info_i), "_", length(grep(parsed_type_i, parsed_typeList)) + 1)
        } else if (parsed_type_i == "trend") {
          parsed_k[["trend"]] <- paste(metar_k[i:length(metar_k)], collapse = " ")
          break
        }
        parsed_typeList <- c(parsed_typeList, parsed_type_i)
        parsed_k <- append(parsed_k, parsed_info_i)

      }

    }

    metarList[[k]] <- parsed_k[!is.na(parsed_k)]

  }

  if (out_format == "list") {

    return(metarList)

  } else if (out_format == "data.frame") {

    metar_names <- unique(unlist(lapply(1:length(metarList), function(x) names(metarList[[x]]))))
    metarListVec <- lapply(1:length(metarList), function(x) {
      missing_names <- metar_names[-match(names(metarList[[x]]), metar_names)]
      filler <- rep(NA, length(missing_names))
      names(filler) <- missing_names
      return(c(unlist(metarList[[x]]), filler))
    })
    metarTable <- data.frame(do.call(rbind, c(lapply(metarListVec, function(x) x[match(names(metarListVec[[1]]), names(x))]))))
    return(metarTable[, intersect(METAR_headers, names(metarTable))])

  } else if (out_format == "data.table") {

    if (requireNamespace("data.table", quietly = T)) {
      metarTable <- data.table::rbindlist(metarList, fill = T)
      return(metarTable[, intersect(METAR_headers, names(metarTable))])
    } else {
      warning("\"data.table\" not available for argument out_format, default to \"data.frame\"", call. = T)
    }

  } else {

    stop("invalid out_format argument of parse_METAR()")

  }

}
