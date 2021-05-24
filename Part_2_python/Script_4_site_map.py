# Geospatial processing script for the Drone Allometry project
# Andrew Cunliffe <andrewmcunliffe@gmail.com>
# Script to create global map of sampling sites (pending and completed)


# Import modules
import sys
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
# from matplotlib.pyplot import figure, show


def main(df, loc_main_outputs):
    print("starting script 4 (global map)")
    # Program for master spreadsheet

    # Create lists of 'Collected' and 'Pending' site coordinates,
    lat_collected = list(df['SiteLatitude'])
    lon_collected = list(df['SiteLongitude'])

    # Make global map of sampled sites using cartopy package.
    # Use 'scatter' (not ax.plot) to add points & zorder to control layer order.
    fig = plt.figure()
    ax = fig.add_subplot(1, 1, 1, projection=ccrs.Robinson())
    ax.set_global()
    ax.coastlines(zorder=1)  # Add Natural Earth 1:110,000,000 scale coastline data.
    ax.scatter(lon_collected, lat_collected, color='red', s=20, transform=ccrs.Geodetic(), label='Study Site', zorder=3)
    ax.legend(loc='lower left', bbox_to_anchor=(0.53, 0.0), fancybox=True, framealpha=0.95)
    fig.savefig(loc_main_outputs + 'Global_map_of_sampled_sites - from master.png', dpi=400,
                bbox_inches='tight')  # Save global map w/o background
    # ax.stock_img()
    # fig.savefig(loc_main_outputs + 'Global_map_of_sampled_sites_colour.png', dpi=400, bbox_inches='tight')  # noqa # Save global map

    # Switch for interactive viewing of global map
    # show()

    # Close open plots
    plt.close(fig)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
