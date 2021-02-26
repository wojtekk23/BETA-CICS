import numpy as np
import pandas as pd
import random

# max_rows = pd.options.display.max_rows
# pd.set_option('display.max_rows', None)
 
def generate_data(no_days = 600,
                    beta = 1.2,
                    A_start_px = 10000,
                    B_start_px = 100,
                    A_volatility = 0.01,
                    A_expected_daily_ret = 0,
                    B_volatility = 0.1,
                    B_expected_daily_ret = 0) :
 
    days = range(1, no_days + 1)
    A_daily_returns = np.random.default_rng().normal(A_expected_daily_ret, A_volatility, no_days)
    B_daily_returns = np.random.default_rng().normal(A_daily_returns * beta + B_expected_daily_ret, B_volatility, no_days)
 
    A_cum_returns = np.cumprod(A_daily_returns + np.ones(no_days))
    B_cum_returns = np.cumprod(B_daily_returns + np.ones(no_days))
 
    A_prices = A_cum_returns * A_start_px
    B_prices = B_cum_returns * B_start_px
 
    df = pd.DataFrame({'day' : days, 'A_px' : A_prices, 'B_px' : B_prices, 'A_ret' : A_daily_returns, 'B_ret' : B_daily_returns})
    return df
 
def calculate_beta(df) :
    cov = np.sum(df['A_ret'] * df['B_ret'])
    A_var = np.sum(df['A_ret'] ** 2)
 
    beta = cov / A_var
    return beta
 
beta = random.uniform(-2, 2)
df = generate_data(beta=beta)

print("Day\tA Returns\t B Returns")
for _, row in df.iterrows():
    print("{:05}\t{:+011.6f}\t{:+011.6f}".format(int(row.loc['day']), row.loc['A_ret'], row.loc['B_ret']))

print('real beta', beta)
print('calculated beta', calculate_beta(df)) 
