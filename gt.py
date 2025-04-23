import great_tables as gt
import pandas as pd

sectors = pd.read_csv("sectors.csv")

luca = sectors[
    (sectors["name"] == "Luca Shaw") & (sectors["event_name"] == "Les Gets")
].drop(columns=["name", "event_name"])

tbl = (
    gt.GT(data=luca, rowname_col="round_type")
    .sub_missing()
    .opt_row_striping()
    .fmt_number(columns=["1", "2", "3", "4", "5", "time"], decimals=2)
    .tab_spanner(label="Section", columns=["1", "2", "3", "4", "5", "time"])
    .tab_header(
        title=gt.md(
            "**Luca Shaw's fastest sections in Les Gets were spread across the event**"
        ),
        subtitle=gt.md("Fastest splits are highlighted in green"),
    )
    .data_color(columns="1", rows="4", palette="BrBG")
)

tbl.show()
