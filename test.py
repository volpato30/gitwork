import numpy as np
start = datetime(2011, 1, 1)
end   = datetime(2014, 5, 25)
benchmark = 'HS300'
universe  = set_universe('HS300')
capital_base = 1000000

pos_pieces = 10
enter_window = 20
exit_window = 10

def initialize(account):
    pass

def handle_data(account):
    highest_price = account.get_attribute_history('highPrice', enter_window)
    lowest_price  = account.get_attribute_history('lowPrice', exit_window)
    close_price  = account.get_attribute_history('closePrice', exit_window)
    close_price_300  = account.get_attribute_history('closePrice', 300)
    close_price_50  = account.get_attribute_history('closePrice', 50)
    close_price_20  = account.get_attribute_history('closePrice', 20)
    close_price_10  = account.get_attribute_history('closePrice', 10)
    turnover_vol = account.get_attribute_history('turnoverVol', enter_window)
    for stock in account.universe:
        cnt_price = close_price[stock][-1] 
        cnt_turnover = turnover_vol[stock][-1]
        if cnt_price > np.mean(close_price_10[stock]) and np.mean(close_price_10[stock])>np.mean(close_price_20[stock]) and np.mean(close_price_20[stock])>np.mean(close_price_50[stock]) and np.mean(close_price_50[stock])>np.mean(close_price_300[stock]) :#均线整齐排列
            order_to(stock, capital_base/pos_pieces/cnt_price)
        elif cnt_price <sum(lowest_price[stock])/len(lowest_price[stock]): #跌破10日平均低点卖出           
            order_to(stock, 0)