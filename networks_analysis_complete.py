
# this code does the same stuff of network_analysis.py but extended to all the countries and all the years

import os
import matplotlib.pyplot as plt
import networkx as nx
import numpy as np
import pandas as pd
import seaborn as sns
from scipy.stats import linregress
import statsmodels.api as sm

# aus stuff
precision = 4
linewidth = 0.4
color1 = 'orangered'
color2 = 'royalblue'
cmap = plt.cm.managua # plasma maybe better

n = 45  # number of sectors to consider, constant for all countries and years

# list of countries
countries = {
    'SWE': 'Sweden',
    'POL': 'Poland',
    'ITA': 'Italy',
    'FRA': 'France',
    'FIN': 'Finland',
    'NOR': 'Norway',
    'CHE': 'Switzerland',
    'PRT': 'Portugal',
    'EST': 'Estonia',
    'LVA': 'Latvia',
    'CHN': 'China',
    'NZL': 'New Zealand',
}

def analyze_country(country_code, country_name):

    # ======================================================== setup 

    output_dir = "outputs/"+country_name
    os.makedirs(output_dir, exist_ok=True)

    # saving functions
    def save_plot(name):
        plt.savefig(os.path.join(output_dir, f"{name}.pdf"))
        plt.close()

    def save_dataframe(df, name):
        df.to_csv(os.path.join(output_dir, f"{name}.csv"))

    # This code saves all print output to a file instead of displaying it on the screen
    import sys
    sys.stdout = open(os.path.join(output_dir, "print_output.txt"), "w")

    # Load the data and calculate degrees
    degrees_per_year = {}
    for year in range(2009, 2020):
        data = pd.read_csv(f'../data/io_tables/{country_code}{year}ttl.csv', index_col=0)
        data = data.iloc[:n, :n] 
        data.index = data.index.str.replace("TTL_", "", regex=False)
        in_degree = data.sum(axis=0)
        W = data.div(data.sum(axis=0), axis=1).fillna(0) # column normrmalization (not necessary actually)
        first_order_degrees = W.sum(axis=1) # first order degree (out-degree)
        second_order_degrees = W.mul(first_order_degrees, axis=0).sum(axis=0) # second order degree
        first_order_degrees.name = 'First-Order Degree'
        second_order_degrees.name = 'Second-Order Degree'
        in_degree.name = 'In-Degree'
        degrees = pd.concat([first_order_degrees, second_order_degrees, in_degree], axis=1)
        degrees_per_year[year] = degrees

    # plot stuff
    years = [str(year) for year in degrees_per_year.keys()]
    colors = [cmap(i / (len(years) - 1)) for i in range(len(years))]

    # ======================================================== indegree analysis

    def plot_in_degree_density():
        plt.figure(figsize=(8, 5))

        #ax.axvline(mean-std, color=color1, linestyle='--')
        #ax.axvline(mean+std, color=color1, linestyle='--')
        #ax.axvline(mean, color='black', linestyle='-')

        plt.title('Empirical Density of Weighted In-Degrees - ' + country_name)
        plt.xlabel('Weighted In-Degree')
        plt.ylabel('Density')
        plt.grid(True, which="both", ls="--", linewidth=linewidth)
        for i, (year, degrees) in enumerate(degrees_per_year.items()):
            in_degrees = degrees['In-Degree']
            ax = sns.kdeplot(in_degrees, fill=False, color=colors[i], linewidth=0.8, label=str(year))
        plt.tight_layout()
        plt.legend(title='Year', frameon=False)
        save_plot("in_degree_density")


    # Compute in-degree statistics
    in_degree_stats_list = []
    for year, df in degrees_per_year.items():
        indegree = df['In-Degree']
        mean = indegree.mean()
        std = indegree.std()
        within_1_std = ((indegree >= mean - std) & (indegree <= mean + std)).sum()
        percentage_within_1_std = (within_1_std / len(indegree)) * 100
        in_degree_stats_list.append({
            'Year': year,
            'Mean': round(mean, precision),
            'Std': round(std, precision),
            'Percentage within 1 std': round(percentage_within_1_std, precision)
        })

    # Create DataFrame from the list
    in_degree_statistics = pd.DataFrame(in_degree_stats_list)
    in_degree_statistics.set_index('Year', inplace=True)
    save_dataframe(in_degree_statistics, "in_degree_statistics")
    del in_degree_stats_list
    plot_in_degree_density()

    # ======================================================== out-degrees plots

    # out-degrees (first and second order) visualization and analysis**
    def plot_output_degrees_density():
        fig, axes = plt.subplots(1, 2, figsize=(10, 5), sharey=True)

        # First-order degree plot
        for i, (year, degrees) in enumerate(degrees_per_year.items()):
            first_order_degrees = degrees['First-Order Degree']
            sns.kdeplot(first_order_degrees, fill=False, color=colors[i], label=str(year), linewidth=0.8, ax=axes[0])
        axes[0].set_title('First-Order Output Degrees')
        axes[0].set_xlabel('Degree Value')
        axes[0].set_ylabel('Density')
        axes[0].grid(True, which="both", ls="--", linewidth=linewidth)

        # Second-order degree plot
        for i, (year, degrees) in enumerate(degrees_per_year.items()):
            second_order_degrees = degrees['Second-Order Degree']
            sns.kdeplot(second_order_degrees, fill=False, color=colors[i], label=str(year), linewidth=0.8, ax=axes[1])
        axes[1].set_title('Second-Order Output Degrees')
        axes[1].set_xlabel('Degree Value')
        axes[1].grid(True, which="both", ls="--", linewidth=linewidth)

        plt.title('Empirical Density of Weighted Output Degrees - ' + country_name)
        plt.tight_layout()
        plt.legend(title='Year', frameon=False)
        save_plot("out_degree_density")

    plot_output_degrees_density()

    # ======================================================== top sectors

    # plot with rows = yeras, columns = top 5 sectors
    economic_sectors = {
    "A01_02": "Agriculture, hunting, forestry",
    "A03": "Fishing and aquaculture",
    "B05_06": "Mining and quarrying, energy producing products",
    "B07_08": "Mining and quarrying, non-energy producing products",
    "B09": "Mining support service activities",
    "C10T12": "Food products, beverages and tobacco",
    "C13T15": "Textiles, textile products, leather and footwear",
    "C16": "Wood and products of wood and cork",
    "C17_18": "Paper products and printing",
    "C19": "Coke and refined petroleum products",
    "C20": "Chemical and chemical products",
    "C21": "Pharmaceuticals, medicinal chemical and botanical products",
    "C22": "Rubber and plastics products",
    "C23": "Other non-metallic mineral products",
    "C24": "Basic metals",
    "C25": "Fabricated metal products",
    "C26": "Computer, electronic and optical equipment",
    "C27": "Electrical equipment",
    "C28": "Machinery and equipment, nec",
    "C29": "Motor vehicles, trailers and semi-trailers",
    "C30": "Other transport equipment",
    "C31T33": "Manufacturing nec; repair and installation of machinery and equipment",
    "D": "Electricity, gas, steam and air conditioning supply",
    "E": "Water supply; sewerage, waste management and remediation activities",
    "F": "Construction",
    "G": "Wholesale and retail trade; repair of motor vehicles",
    "H49": "Land transport and transport via pipelines",
    "H50": "Water transport",
    "H51": "Air transport",
    "H52": "Warehousing and support activities for transportation",
    "H53": "Postal and courier activities",
    "I": "Accommodation and food service activities",
    "J58T60": "Publishing, audiovisual and broadcasting activities",
    "J61": "Telecommunications",
    "J62_63": "IT and other information services",
    "K": "Financial and insurance activities",
    "L": "Real estate activities",
    "M": "Professional, scientific and technical activities",
    "N": "Administrative and support services",
    "O": "Public administration and defence; compulsory social security",
    "P": "Education",
    "Q": "Human health and social work activities",
    "R": "Arts, entertainment and recreation",
    "S": "Other service activities",
    "T": "Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use",
    "HFCE": "Final consumption expenditure of households",
    "NPISH": "Final consumption expenditure of non-profit institutions serving households",
    "GGFC": "Final consumption expenditure of general government",
    "GFCF": "Gross Fixed Capital Formation",
    "INVNT": "Changes in inventories",
    "DPABR": "Direct purchases abroad by residents (imports)",
    "CONS_NONRES": "Direct purchases by non-residents (exports)",
    "EXPO": "Exports (cross border)",
    "IMPO": "Imports (cross border)"
}
    
    from matplotlib.colors import ListedColormap
    from matplotlib.patches import Patch

    # Create a consistent mapping of sectors to indices and colors
    sector_keys = list(economic_sectors.keys())
    used_sectors = set()

    # First pass: find all top 5 sectors across years
    for year in degrees_per_year:
        top5 = degrees_per_year[year]['First-Order Degree'].sort_values(ascending=False).head(5).index.tolist()
        used_sectors.update(top5)

    used_sectors = sorted(used_sectors)  # consistent order

    # Assign unique indices and colors to ONLY the used sectors
    palette = sns.color_palette("managua", n_colors=len(used_sectors)) # palette for used sectors
    sector_to_index = {sector: i for i, sector in enumerate(used_sectors)} # map sector to an unique index
    sector_to_color = {sector: palette[i] for i, sector in enumerate(used_sectors)} # map sector to a color, via palette
    index_to_sector = {i: sector for sector, i in sector_to_index.items()} # this is the reverse mapping, for the legend

    color_map = ListedColormap([sector_to_color[sector] for sector in used_sectors])

    def plot_top_sectors():
        years = list(degrees_per_year.keys())
        plot = np.zeros((len(years), 5), dtype=int)
        for i, year in enumerate(years):
            top_sectors = degrees_per_year[year]['First-Order Degree'].sort_values(ascending=False).head(5).index.tolist()
            for j, sector in enumerate(top_sectors):
                plot[i, j] = sector_to_index[sector] # index of the corresponding sector

        fig, ax = plt.subplots(figsize=(8,  0.5*len(years)))
        im = ax.pcolormesh(plot, edgecolors='w', linewidth=3, cmap=color_map)

        # Remove black outer border
        for spine in ax.spines.values():
            spine.set_visible(False)

        # Set ticks and labels
        ax.set_xticks(np.arange(5)+0.5) # +0.5 to center the ticks
        ax.set_xticklabels([f'Top {i+1}' for i in range(5)])
        ax.set_yticks(np.arange(len(years))+0.5)
        ax.set_yticklabels(years)

        # Create legend
        legend_elements = [
            Patch(facecolor=color_map(i), label=economic_sectors[index_to_sector[i]])
            for i in range(len(used_sectors))
        ]
        ax.legend(handles=legend_elements, bbox_to_anchor=(1.05, 1), loc='upper left', frameon=False, fontsize=10)
        ax.set_title(f'Top 5 Sectors by First-Order Degree - {country_name}', pad=15, loc='left')
        plt.tight_layout()
        save_plot("top_sectors_output_degrees")
        plt.show()

    plot_top_sectors()

    # ======================================================== CCDF of outer degrees

    def compute_ccdf_direct(data):
        # Unique sorted values (ascending)
        values = np.sort(np.unique(data))
        n = len(data)

        # For each unique value, count how many data points are greater than it
        ccdf_y = np.array([np.sum(data > v) / n for v in values])
        return values, ccdf_y

    def plot_ccdf_outer_degrees():
    
        fig, axs = plt.subplots(1, 2, figsize=(10, 6), sharey=True)
        for i, (year, degrees) in enumerate(degrees_per_year.items()):
            x1, y1 = compute_ccdf_direct(degrees['First-Order Degree'])
            x2, y2 = compute_ccdf_direct(degrees['Second-Order Degree'])

            axs[0].loglog(x1, y1, marker='.', linestyle='-', color = colors[i], label=str(year), linewidth=0.8, alpha=0.5, mew=0.2)
            axs[1].loglog(x2, y2, marker='.', linestyle='-', color = colors[i], label=str(year), linewidth=0.8, alpha=0.5, mew=0.2)
            
        axs[0].set_title('First-order Outer Degree')
        axs[0].set_xlabel('Degree')
        axs[0].set_ylabel('1 - CDF (CCDF)')
        axs[0].grid(True, which="both", ls="--", linewidth=linewidth)
        axs[1].set_title('Second-order Outer Degree')
        axs[1].set_xlabel('Degree')
        #axs[1].set_ylabel('1 - CDF (CCDF)')
        axs[1].grid(True, which="both", ls="--", linewidth=linewidth)
        plt.legend([str(year) for year in degrees_per_year.keys()], title='Year', frameon=False)
        plt.suptitle('CCDF of Outer Degrees - ' + country_name)
        plt.tight_layout()
        save_plot("ccdf_outer_degrees")

    plot_ccdf_outer_degrees()

    # ======================================================== regression analysis

    # Function to estimate power-law using Gabaix–Ibragimov correction
    def estimate_power_law_gabaix(values, label=''):

        # Drop zeros and sort descending
        values = values[values > 0].sort_values(ascending=False)
        n = len(values)
        ranks = np.arange(1, n + 1)

        # Gabaix–Ibragimov correction: use rank - 0.5
        x = np.log(values.values)
        y = np.log(ranks - 0.5)

        # Run OLS: y = α - β x
        X = sm.add_constant(x)
        model = sm.OLS(y, X).fit()
        beta = -model.params[1]
        
        return x, y, model.predict(X), beta, model.rsquared

    aus = []
    for year, degrees in degrees_per_year.items():
        top_d = degrees['First-Order Degree'].nlargest(int(0.25 * len(degrees)))
        top_q = degrees['Second-Order Degree'].nlargest(int(0.25 * len(degrees)))

        x_d, y_d, y_pred_d, beta_d, r2_d = estimate_power_law_gabaix(top_d, label='First-Order Degree')
        x_q, y_q, y_pred_q, beta_q, r2_q = estimate_power_law_gabaix(top_q, label='Second-Order Degree')

        # Store results in DataFrame
        aus.append({
            'Year': year,
            'First-Order Degree β': round(beta_d, precision),
            'First-Order Degree R²': round(r2_d, precision),
            'Second-Order Degree ζ': round(beta_q, precision),
            'Second-Order Degree R²': round(r2_q, precision)
        })
    
    # Save the regression results
    regression_GI_results = pd.DataFrame(aus)
    regression_GI_results.set_index('Year', inplace=True)
    save_dataframe(regression_GI_results, "regression_GI_results")


    # # plotting the results
    # fig, axs = plt.subplots(1, 2, figsize=(12, 5))
    # axs[0].plot(x_d, y_d, 'o', markersize=4, alpha=0.6, label='Empirical')
    # axs[0].plot(x_d, y_pred_d, 'r-', label=f'Fit: β = {beta_d:.2f}')
    # axs[0].set_title('Power-law Fit - First-Order Degree')
    # axs[0].set_xlabel('log(Degree)')
    # axs[0].set_ylabel('log(Rank - 0.5)')
    # axs[0].legend()
    # axs[0].grid(True, which="both", ls="--", linewidth=linewidth)
    # axs[1].plot(x_q, y_q, 'o', markersize=4, alpha=0.6, label='Empirical')
    # axs[1].plot(x_q, y_pred_q, 'r-', label=f'Fit: β = {beta_q:.2f}')
    # axs[1].set_title('Power-law Fit - Second-Order Degree')
    # axs[1].set_xlabel('log(Degree)')
    # axs[1].legend()
    # axs[1].grid(True, which="both", ls="--", linewidth=linewidth)
    # plt.title('Power-law Fit of Outer Degrees - ' + country_name)
    # plt.tight_layout()
    # save_plot("estimated_power_law_params.png")

    # other linear regression, less reliable (basic model)
    def plot_ccdf_loglog(values):
        sorted_vals = np.sort(values)[::-1]
        n_tail = int(len(sorted_vals))
        x = np.log(sorted_vals[:n_tail])
        y = np.log(np.arange(1, n_tail + 1) - 0.5)
        slope, intercept, rvalue, _, _ = linregress(x, y)
        beta_hat = -slope
        return beta_hat, rvalue**2

    aus = []
    for year, degrees in degrees_per_year.items():
        top_d = degrees['First-Order Degree'].nlargest(int(0.25 * len(degrees)))
        top_q = degrees['Second-Order Degree'].nlargest(int(0.25 * len(degrees)))

        beta_d, r2_d = plot_ccdf_loglog(top_d)
        beta_q, r2_q = plot_ccdf_loglog(top_q)

        # Store results in DataFrame
        aus.append({
            'Year': year,
            'First-Order Degree β': round(beta_d,precision),
            'First-Order Degree R²': round(r2_d, precision),
            'Second-Order Degree ζ': round(beta_q, precision),
            'Second-Order Degree R²': round(r2_q, precision)
        })
    
    # Save the regression results
    regression_results = pd.DataFrame(aus)
    regression_results.set_index('Year', inplace=True)
    save_dataframe(regression_results, "regression_results")

    # ======================================================== coefficient of variation

    CV = []
    for year, degrees in degrees_per_year.items():
        aus = (1/np.mean(degrees['First-Order Degree']))*np.var(degrees['First-Order Degree'], ddof=1)
        print(f"in {year} aggregate volatility decays no faster than n^{(1+aus)/np.sqrt(n)}")
        CV.append(aus)
    CV = np.array(CV)
    fig = plt.figure(figsize=(8, 5))
    plt.plot(list(degrees_per_year.keys()), CV, marker='o', linestyle='-', markersize=4, color=colors[2], linewidth=1.2, alpha=0.7)
    plt.title('Coefficient of Variation of First-Order Degree - ' + country_name)
    plt.xlabel('Year')
    plt.xticks(list(degrees_per_year.keys()))
    plt.ylabel('Coefficient of Variation')
    plt.grid(True, which="both", ls="--", linewidth=linewidth)
    plt.tight_layout()
    save_plot("CV")



for country_code, country_name in countries.items():
    analyze_country(country_code, country_name)

