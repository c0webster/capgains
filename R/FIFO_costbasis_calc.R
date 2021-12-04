FIFO_costbasis_calc <- function(qty_bought, price_bought,
                                timestamp_bought, qty_selloff) {
  running_qty <- 0
  running_cost <- 0
  n_rows_used <- 0
  qty_bought <- round(qty_bought, 8)
  qty_selloff <- round(qty_selloff, 8)
  max_rows <- length(qty_bought)
  for (i in 1:length(qty_bought)){
    if (round(running_qty + qty_bought[i], 8) < round(qty_selloff, 8)) {
      if (i == max_rows) {
        running_qty <- running_qty + qty_bought[i]
        running_cost <- running_cost + price_bought[i]
        n_rows_used <- n_rows_used + 1
        remaining_row <- data.table(qty_bought = 0,
                                    price_bought = 0,
                                    timestamp = timestamp_bought[i])
        final_timestamp <- timestamp_bought[i]
      } else {
        running_qty <- running_qty + qty_bought[i]
        running_cost <- running_cost + price_bought[i]
        n_rows_used <- n_rows_used + 1

      }
      print(running_cost)
    } else if (round(running_qty + qty_bought[i], 8) >= round(qty_selloff, 8)) {
      final_price_per <- price_bought[i] / qty_bought[i]
      remaining_qty <- qty_selloff - running_qty
      # save the leftover qty

      leftover_qty_in_trans <- qty_bought[i] + running_qty - qty_selloff
      leftover_price_in_trans <- price_bought[i] - remaining_qty * final_price_per
      # build a table for remaining row in buys

      remaining_row <- data.table(qty_bought = leftover_qty_in_trans,
                                  price_bought = leftover_price_in_trans,
                                  timestamp = timestamp_bought[i])

      running_cost <- running_cost + remaining_qty * final_price_per
      final_timestamp <- timestamp_bought[i]
      running_qty <- qty_selloff
      n_rows_used <- n_rows_used + 1
      break
    }

  }

  cost_basis <- running_cost
  return(list(cost_basis = cost_basis,
              remaining_row = remaining_row,
              n_rows_used = n_rows_used,
              final_timestamp = final_timestamp))

}

# FIFO_costbasis_calc(c(1, .5, .75), c(100, 25, 50), c(1, 2, 3), 1.75)
calc_costbasis_allsells <- function(sell_table, buy_table) {

  # sell_table <- copy(all_sells)
  # buy_table <- copy(all_gains)
  # sell_table <- sell_table[1]
  # sell_table[, qty_selloff := 4.5]
  for (i in 1:nrow(sell_table)) {
    # i <- 1
    # print(i)
    setkey(sell_table, timestamp)
    setkey(buy_table,  timestamp)
    time_sell <- sell_table[i, timestamp]
    qty_selloff <- sell_table[i, qty_selloff]
    # sell_table
    # buy_table
    # time_sell
    fifo_result <- FIFO_costbasis_calc(buy_table[timestamp <= time_sell, qty_bought],
                                       buy_table[timestamp <= time_sell, price_bought],
                                       buy_table[timestamp <= time_sell, timestamp],
                                       qty_selloff
    )
    # sell_table
    sell_table[i, cost_basis := fifo_result$cost_basis]
    sell_table[i, timestamp_bought := fifo_result$final_timestamp]
    buy_table <- buy_table[!1:fifo_result$n_rows_used]

    # add on the remainder of the row
    if (fifo_result$remaining_row$qty_bought != 0) {
      buy_table <- rbind(fifo_result$remaining_row, buy_table)
    }

    # buy_table <- buy_table[!round(qty_bought,8) == 0]
    # buy_table
  }
  return(sell_table)
}

calc_tax_per_asset <- function(asset_id, trans_table) {
  # asset_id <- "MEX"
  # print(asset_id)
  # trans_table <- data_test
  table <- trans_table[asset == asset_id]
  buys <- table[trans_type == "Buy",
                .(qty_bought = quantity, price_bought = usd_total, timestamp)]
  all_gains <- buys
  setkey(all_gains, timestamp)


  sells_away <- table[trans_type == "Sell",
                      .(qty_selloff = quantity, timestamp, price_sold = usd_total)]

  all_sells <- rbind(sells_away)
  if (all_sells[, .N] == 0) {
    return(NULL)
  }
  summed_sell <- all_sells[, sum(qty_selloff)]
  summed_gain <- all_gains[, sum(qty_bought)]
  if (round(summed_sell - summed_gain, 2) != 0) {
    print(str_c("Diff in asset ", asset_id, ":", round(summed_sell - summed_gain, 3)))
  }
  setkey(all_sells, timestamp)
  # a
  sell_taxes_result <- copy(calc_costbasis_allsells(all_sells, all_gains))
  sell_taxes_result[, cap_gains := price_sold - cost_basis]
  sell_taxes_result[, asset := asset_id]
  return(sell_taxes_result)
}
calc
# RUN STUFF ON DATA ------------------------------------------------------------
data_test
all_ids <- data_test[,unique(asset)]

all_sell_results <- map_dfr(all_ids, calc_tax_per_asset, trans_table = data_test)
all_sell_results[, sum(cap_gains)] * .25
all_sell_results[order(timestamp)]
