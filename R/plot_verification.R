#### TEMPORY VERSION OF FUNCTION - MAJOR REFACTORING TO COME


#' Title
#'
#' @param plotData
#' @param Score
#' @param parameter
#' @param xAxis
#' @param yAxis
#' @param colourBy
#' @param facetBy
#' @param linetypeBy
#' @param plotPoints
#' @param ncol
#' @param colourTable
#' @param plotCaption
#' @param includeZero
#' @param xLabel
#' @param yLabel
#' @param linesize
#' @param pointsize
#' @param num_legend_rows
#' @param legend.position
#' @param plot_title
#' @param plot_subtitle
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_verification <- function(plotData,
	Score,
	parameter,
	xAxis           = "leadtime",
	yAxis           = NULL,
	colourBy        = "mname",
	facetBy         = NULL,
	linetypeBy      = NULL,
	plotPoints      = TRUE,
	ncol            = 2,
	colourTable     = NULL,
	plotCaption     = NULL,
	includeZero     = TRUE,
	xLabel          = NULL,
	yLabel          = NULL,
	linesize        = 1.1,
	pointsize       = 2,
	num_legend_rows = 3,
	legend.position = "bottom",
	plot_title      = NULL,
	plot_subtitle   = NULL,
	...) {
	#
	# plot a verification score - can take any tibble generated from verifyMembers or verifyEPS
	#
	dots <- list(...)
	#
	# Add more meaningful for plotting data for leadtime and threshold columns (for faceting)
	#
	coloursSupplied <- !is.null(colourTable)
	plotData <- plotData %>% dplyr::mutate(leadtimeC = paste0("T+", leadtime, "h"))
	plotData$leadtimeC <- factor(plotData$leadtimeC, levels = unique(plotData$leadtimeC))
	if ("threshold" %in% colnames(plotData)) {
		if (stringr::str_detect(parameter, "T") && min(plotData$threshold > 200)) plotData$threshold = round(plotData$threshold - 273.15, digits = 2)
		plotData <- plotData %>% dplyr::mutate(thresholdC = paste(parameter, ">", threshold))
		plotData$thresholdC <- factor(plotData$thresholdC, levels = unique(plotData$thresholdC))
	}
	#
	if (!is.null(facetBy) && facetBy %in% c("leadtime", "threshold")) {
		facetBy <- paste0(facetBy, "C")
	}
	#
	# unnest the nested scores if being plotted and define the x and y axes
	#
	switch(tolower(Score),
		"reliability" = {
			xAxis         <- "prob"
			yAxis         <- "freq"
			plotData      <- plotData %>% tidyr::unnest(Reliability) %>% tidyr::unnest(prob, freq, prop)
		},
		"ecoval"      = {
			xAxis         <- "cl"
			yAxis         <- "value"
			plotData      <- plotData %>% tidyr::unnest(ecoval)
		},
		"roc"         = {
			xAxis         <- "FAR"
			yAxis         <- "HR"
			plotData      <- plotData %>% tidyr::unnest(roc)
		},
		"spread_skill" = {
			xAxis         <- xAxis
			yAxis         <- "Spread ; RMSE"
			plotData      <- plotData %>% tidyr::gather(rmse, spread, key = "component", value = "Spread ; RMSE")
			linetypeBy    <- "component"
		},
		"ss_ratio"     = {
			xAxis         <- xAxis
			yAxis         <- "Spread / Skill ratio"
			plotData      <- plotData %>% dplyr::mutate("Spread / Skill ratio" = spread / rmse)
		},
		"bs_decomp"    = {
			xAxis         <- xAxis
			yAxis         <- "score"
			plotData      <- plotData %>% tidyr::gather(BSrel, BSres, BSunc, key = "component", value = "score")
			linetypeBy    <- "component"
		},
		"rank_hist"    = {
			xAxis         <- "rank"
			yAxis         <- "rank_count"
			plotData      <- plotData %>% tidyr::unnest(rank, rank_count)
			plotData$rank <- formatC(plotData$rank, width = 2, flag = "0")
		},
		"rankHistAll" = {
			xAxis         <- "rank"
			yAxis         <- "rank_count"
			plotData      <- plotData %>% tidyr::unnest(rank, rank_count) %>% dplyr::group_by(mname, rank, parameter) %>%
				dplyr::summarise(rank_count = sum(rank_count))
			plotData$rank <- formatC(plotData$rank, width = 2, flag = "0")
		},
		"biasCorrSS"  = {
			xAxis         <- xAxis
			yAxis         <- "Spread ; Skill"
			plotData      <- plotData %>% dplyr::mutate(biasCorrectedSkill  = sqrt(SDE_m_squared),
				biasCorrectedSpread = sqrt(VARE)) %>%
				tidyr::gather(biasCorrectedSkill, biasCorrectedSpread,
					key = "component", value = "Spread ; Skill")
			linetypeBy    <- "component"
		},
		"biasCorrSSp" = {
			xAxis         <- xAxis
			yAxis         <- "Spread ; Skill"
			plotData      <- plotData %>% dplyr::mutate(biasCorrectedSkill  = sqrt(SDE_pm_squared),
				biasCorrectedSpread = sqrt(VARE_p)) %>%
				tidyr::gather(biasCorrectedSkill, biasCorrectedSpread,
					key = "component",value = "Spread ; Skill")
			linetypeBy    <- "component"
		},
		"uuiSS"       = {
			xAxis         <- xAxis
			yAxis         <- "Spread ; Skill"
			plotData      <- plotData %>% dplyr::mutate(uuiSkill  = sqrt(0.5 * SDE_p_squared),
				uuiSpread = sqrt(VARE_p)) %>%
				tidyr::gather(uuiSkill, uuiSpread,
					key="component", value="Spread ; Skill")
			linetypeBy    <- "component"
		},
		"uuiSSratio"  = {
			xAxis         <- xAxis
			yAxis         <- "UUI Spread / Skill ratio"
			plotData      <- plotData %>% dplyr::mutate(uuiSkill  = sqrt(0.5 * SDE_p_squared),
				uuiSpread = sqrt(VARE_p)) %>%
			dplyr::mutate("UUI Spread / Skill ratio" = uuiSpread / uuiSkill)
		},
		{
			xAxis    <- xAxis
			yAxis    <- Score
			plotData <- plotData
		})
	#
	# Colour table
  #
	if (coloursSupplied) {
	  if (!any(grepl(colourBy, names(colourTable)))) {
	    warning(
	      "colourBy = ", colourBy, ", but is not in colourTable\n",
	      "Plotting with default colours."
	    )
	    coloursSupplied = FALSE
	    break
	  }
	  colourTable <- dplyr::mutate_if(colourTable, is.factor, as.character)
		colourTable <- plotData %>%
			dplyr::inner_join(colourTable, by = colourBy) %>%
			dplyr::ungroup() %>%
			dplyr::transmute(!!colourBy := .data[[colourBy]], colour) %>%
			dplyr::group_by(!!colourBy := .data[[colourBy]]) %>%
			dplyr::summarise(colour = unique(colour)) %>%
			dplyr::ungroup()
		plotData[[colourBy]] <- factor(plotData[[colourBy]], levels = unique(colourTable[[colourBy]]))
	}
	#
	# Plot defaults
	#
	if (is.null(colourBy)) {
		gg <- ggplot2::ggplot(plotData, ggplot2::aes(get(xAxis), get(yAxis)))
	} else {
		gg <- ggplot2::ggplot(plotData, ggplot2::aes(get(xAxis), get(yAxis), group = get(colourBy), colour = get(colourBy)))
	}
	#
	if (is.null(xLabel)) xLabel <- xAxis
	if (is.null(yLabel)) yLabel <- yAxis
	if (is.null(plot_title)) plot_title <- Score
	if (is.null(plot_subtitle)) plot_subtitle <- parameter
	gg <- gg + ggplot2::theme_bw()
	gg <- gg + ggplot2::xlab(xLabel)
	gg <- gg + ggplot2::ylab(yLabel)
	gg <- gg + ggplot2::labs(title = plot_title,subtitle = plot_subtitle)
	gg <- gg + ggplot2::theme(legend.position = legend.position)
	gg <- gg + ggplot2::guides(colour   = ggplot2::guide_legend(title = NULL, nrow = num_legend_rows, byrow = TRUE),
		shape    = ggplot2::guide_legend(title = NULL, nrow = num_legend_rows, byrow = TRUE),
		fill     = ggplot2::guide_legend(title = NULL, nrow = num_legend_rows, byrow = TRUE),
		linetype = ggplot2::guide_legend(title = NULL))
	#
	# Plot options
	#
	if (grepl("rankHist", Score)) {
		#
		gg <- gg + ggplot2::geom_bar(ggplot2::aes(fill = get(colourBy)), stat = "identity", position = ggplot2::position_dodge(), colour = "black")
		if (coloursSupplied) gg <- gg + ggplot2::scale_fill_manual(values = colourTable$colour)
		#
	} else {
		#
		if (plotPoints) {
			if (Score == "Reliability") {
				gg <- gg + ggplot2::geom_point(aes(size = prop))
			} else {
				gg <- gg + ggplot2::geom_point(size = pointsize)
			}
		}
		#
		if (xAxis == "leadtime") gg <- gg + ggplot2::scale_x_continuous(breaks = seq(0, 180, 6))
		if (xAxis == "YearMonth") {
			gg <- gg + ggplot2::scale_x_continuous(breaks = pretty(plotData$YearMonth, 12))
			gg <- gg + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
		}
		if (xAxis == "leadtime" | xAxis == "threshold" | xAxis == "YearMonth") {
			if (includeZero) {
				minY <- min(plotData[[yAxis]], na.rm = TRUE)
				maxY <- max(plotData[[yAxis]], na.rm = TRUE)
				if (min(plotData[[yAxis]], na.rm = TRUE) > 0) {
					minY <- 0
					maxY <- ifelse(stringr::str_detect(Score, "ratio"), max(1, max(plotData[[yAxis]], na.rm = TRUE)), max(plotData[[yAxis]], na.rm = TRUE))
				}
				if (max(plotData[[yAxis]], na.rm = TRUE) < 0) {
					minY <- min(plotData[[yAxis]], na.rm = TRUE)
					maxY <- 0
				}
			} else {
				minY <- min(plotData[[yAxis]], na.rm = TRUE)
				maxY <- ifelse(stringr::str_detect(Score, "ratio"), max(1, max(plotData[[yAxis]], na.rm = TRUE)), max(plotData[[yAxis]], na.rm = TRUE))
			}
			if (!"scales" %in% names(dots)) {
				gg <- gg + ggplot2::coord_cartesian(ylim = c(minY, maxY))
			} else {
				if (dots$scales != "free_y" & dots$scales != "free") gg <- gg + ggplot2::coord_cartesian(ylim = c(minY, maxY))
			}
		}
		#
		if (Score %in% c("Reliability", "ecoval", "ROC")) gg <- gg + ggplot2::coord_fixed(1, c(0, 1), c(0, 1))
		#
		if (Score %in% c("Reliability", "ROC")) gg <- gg + ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey70", lty = 3)
		#
		if (is.null(linetypeBy)) {
			gg <- gg + ggplot2::geom_line(size = linesize)
		} else {
			gg <- gg + ggplot2::geom_line(size = linesize, ggplot2::aes(lty = get(linetypeBy)))
			gg <- gg + ggplot2::scale_linetype_manual(values = c("solid", "21", "11"))
		}
		#
		if (coloursSupplied) gg <- gg + ggplot2::scale_colour_manual(values = colourTable$colour)
		#
	}
	#
	if (!is.null(facetBy)) gg <- gg + ggplot2::facet_wrap(facetBy, ncol = ncol, ...)
	if (!is.null(plotCaption)) gg <- gg + ggplot2::labs(caption = plotCaption)
	#
	gg
	#
}
