-- Seed: 4064053188694106063,16159265764638711791

entity ijzdypgb is
  port (ztfat : linkage time; uldbr : out integer; zerdglgex : buffer real);
end ijzdypgb;

architecture qxircs of ijzdypgb is
  
begin
  
end qxircs;

entity agg is
  port (tttcdrnoo : buffer real);
end agg;

architecture vqtc of agg is
  signal evn : integer;
  signal t : time;
  signal ynpbkjnjz : real;
  signal jbhwamny : integer;
  signal srywczcqg : time;
  signal voln : real;
  signal shpprdue : integer;
  signal tffk : time;
begin
  yyhxu : entity work.ijzdypgb
    port map (ztfat => tffk, uldbr => shpprdue, zerdglgex => voln);
  k : entity work.ijzdypgb
    port map (ztfat => srywczcqg, uldbr => jbhwamny, zerdglgex => ynpbkjnjz);
  pepj : entity work.ijzdypgb
    port map (ztfat => t, uldbr => evn, zerdglgex => tttcdrnoo);
end vqtc;

entity haqvzdh is
  port (dabxp : out real);
end haqvzdh;

architecture kvhokgsdug of haqvzdh is
  signal tzvf : real;
  signal ishi : real;
  signal zjmzegoe : real;
begin
  sosaeeu : entity work.agg
    port map (tttcdrnoo => zjmzegoe);
  zrsqxs : entity work.agg
    port map (tttcdrnoo => ishi);
  k : entity work.agg
    port map (tttcdrnoo => dabxp);
  uitktwuqd : entity work.agg
    port map (tttcdrnoo => tzvf);
end kvhokgsdug;

entity c is
  port (piqamd : inout integer; lhjau : out real);
end c;

architecture mzvlg of c is
  signal uleefrt : real;
  signal spdgcnkd : time;
  signal svct : real;
begin
  hjvmxj : entity work.haqvzdh
    port map (dabxp => svct);
  aqhriwpl : entity work.ijzdypgb
    port map (ztfat => spdgcnkd, uldbr => piqamd, zerdglgex => uleefrt);
  
  -- Single-driven assignments
  lhjau <= 2_4_2_2_2.21024;
end mzvlg;



-- Seed after: 6974746134706413827,16159265764638711791
