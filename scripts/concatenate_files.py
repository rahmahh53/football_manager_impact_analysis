import pandas as pd
import numpy as np

team_df = pd.read_csv("epl_team_match_data_clean.csv", parse_dates=["Date"])
mgr_raw = pd.read_excel("PREMIER LEAGUE STATISTICS.xlsx", sheet_name="Sheet1")

mgr = mgr_raw.copy()
mgr["TEAM"] = mgr["TEAM"].ffill()
mgr["MANAGERIAL START"] = pd.to_datetime(mgr["MANAGERIAL START"],
                                         dayfirst=True, errors="coerce")

mgr["MANAGERIAL END"] = mgr["MANAGERIAL END"].replace("Present", np.nan)
mgr["MANAGERIAL END"] = pd.to_datetime(mgr["MANAGERIAL END"],
                                       dayfirst=True, errors="coerce")
mgr["MANAGERIAL END"] = mgr["MANAGERIAL END"].fillna(pd.Timestamp("2100-01-01"))
mask = mgr["MANAGERIAL END"] < mgr["MANAGERIAL START"]
tmp = mgr.loc[mask, "MANAGERIAL START"].copy()
mgr.loc[mask, "MANAGERIAL START"] = mgr.loc[mask, "MANAGERIAL END"]
mgr.loc[mask, "MANAGERIAL END"] = tmp
min_date = team_df["Date"].min()
max_date = team_df["Date"].max()
mgr = mgr[(mgr["MANAGERIAL END"] >= min_date) &
          (mgr["MANAGERIAL START"] <= max_date)].copy()
mgr["TEAM_KEY"] = mgr["TEAM"]   

team_df2 = team_df.copy()
team_df2["TEAM_KEY"] = team_df2["Team"].str.upper()

map_override = {
    "MANCHESTER UTD":   "MANCHESTER UNITED",
    "NEWCASTLE UTD":    "NEWCASTLE UNITED",
    "NOTT'HAM FOREST":  "NOTTINGHAM FOREST",
    "SHEFFIELD UTD":    "SHEFFIELD UNITED",
}
team_df2["TEAM_KEY"] = team_df2["TEAM_KEY"].replace(map_override)
team_df2["Manager"] = pd.NA
team_df2["ManagerStart"] = pd.NaT
team_df2["ManagerEnd"] = pd.NaT

for t in team_df2["TEAM_KEY"].unique():
    matches = team_df2[team_df2["TEAM_KEY"] == t].sort_values("Date")
    spells = mgr[mgr["TEAM_KEY"] == t].sort_values("MANAGERIAL START")
    if spells.empty:
        continue

    for _, sp in spells.iterrows():
        mask_dates = (matches["Date"] >= sp["MANAGERIAL START"]) & \
                     (matches["Date"] <= sp["MANAGERIAL END"])
        idx = matches.index[mask_dates]
        if len(idx) == 0:
            continue
        team_df2.loc[idx, "Manager"] = sp["MANAGERS"]
        team_df2.loc[idx, "ManagerStart"] = sp["MANAGERIAL START"]
        team_df2.loc[idx, "ManagerEnd"] = sp["MANAGERIAL END"]
team_df2 = team_df2.sort_values(["Team", "Manager", "Date"])

def assign_matchnum(group):
    start = group["ManagerStart"].iloc[0]
    cutoff = pd.Timestamp("2014-08-01")
    base = 100 if (pd.notna(start) and start < cutoff) else 0
    group = group.copy()
    group["MatchNumMgr"] = base + np.arange(1, len(group) + 1)
    return group

team_df2 = team_df2.groupby(["Team", "Manager"], group_keys=False).apply(assign_matchnum)

team_df2["Window1"] = ((team_df2["MatchNumMgr"] >= 1) &
                       (team_df2["MatchNumMgr"] <= 3)).astype(int)
team_df2["Window2"] = ((team_df2["MatchNumMgr"] >= 4) &
                       (team_df2["MatchNumMgr"] <= 6)).astype(int)
team_df2["Window3"] = ((team_df2["MatchNumMgr"] >= 7) &
                       (team_df2["MatchNumMgr"] <= 10)).astype(int)
cols_out = ["Date","Season","Team","Opponent","HomeFlag","GF","GA",
            "Manager","ManagerStart","ManagerEnd","MatchNumMgr",
            "Window1","Window2","Window3"]

team_df2[cols_out].to_csv("epl_team_match_with_managers_windows.csv",
                          index=False)

