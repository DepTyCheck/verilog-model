-- Seed: 4320890737711725049,7332793847894666635



entity kcs is
  port (gyzpi : inout time; oo : linkage integer);
end kcs;



architecture oqtouuj of kcs is
  
begin
  
end oqtouuj;



entity mhaxt is
  port (qtczbreu : inout time; wiueu : in real; bgfxkuxsun : inout integer);
end mhaxt;



architecture phvbchcw of mhaxt is
  signal bqjjv : integer;
  signal z : time;
  signal w : integer;
  signal bs : time;
  signal zbketce : integer;
begin
  anelmjf : entity work.kcs
    port map (gyzpi => qtczbreu, oo => zbketce);
  lissshnznh : entity work.kcs
    port map (gyzpi => bs, oo => w);
  tsxihkyy : entity work.kcs
    port map (gyzpi => z, oo => bqjjv);
end phvbchcw;



entity hs is
  port (w : buffer character);
end hs;



architecture qzjqdbmwmj of hs is
  signal crm : integer;
  signal drmqry : time;
  signal wqwy : time;
  signal awhvxoopc : integer;
  signal yaqo : real;
  signal aobg : time;
begin
  l : entity work.mhaxt
    port map (qtczbreu => aobg, wiueu => yaqo, bgfxkuxsun => awhvxoopc);
  yfbqqiml : entity work.kcs
    port map (gyzpi => wqwy, oo => awhvxoopc);
  rnxi : entity work.kcs
    port map (gyzpi => drmqry, oo => crm);
end qzjqdbmwmj;



-- Seed after: 10466504200772002842,7332793847894666635
