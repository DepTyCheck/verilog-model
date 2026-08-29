-- Seed: 11589005587005734649,10463297573877745897

entity trgcledww is
  port (hojm : in real; wiqhyxah : buffer time_vector(2 to 2));
end trgcledww;

architecture mnia of trgcledww is
  
begin
  -- Single-driven assignments
  wiqhyxah <= (others => 2 ps);
end mnia;

entity gxpdcmdkt is
  port (qgulczk : linkage time; uqe : linkage real; fehjgs : linkage real; ysi : inout integer);
end gxpdcmdkt;

architecture vr of gxpdcmdkt is
  signal hovbaak : time_vector(2 to 2);
  signal kvjjb : time_vector(2 to 2);
  signal c : real;
  signal naocyh : time_vector(2 to 2);
  signal z : real;
  signal cso : time_vector(2 to 2);
  signal lolqzlq : real;
begin
  vnbtmghkn : entity work.trgcledww
    port map (hojm => lolqzlq, wiqhyxah => cso);
  zqtq : entity work.trgcledww
    port map (hojm => z, wiqhyxah => naocyh);
  al : entity work.trgcledww
    port map (hojm => c, wiqhyxah => kvjjb);
  wzj : entity work.trgcledww
    port map (hojm => c, wiqhyxah => hovbaak);
end vr;



-- Seed after: 11456331684467461198,10463297573877745897
