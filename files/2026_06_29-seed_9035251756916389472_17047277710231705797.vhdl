-- Seed: 9035251756916389472,17047277710231705797

entity muncdip is
  port (zhax : linkage time; g : linkage real; bgbrqesrf : linkage real);
end muncdip;

architecture xdrrh of muncdip is
  
begin
  
end xdrrh;

entity yfxb is
  port (y : buffer time; syz : out integer);
end yfxb;

architecture fcqurvo of yfxb is
  signal ctyntlet : real;
  signal pisyg : real;
  signal nqjswmw : time;
  signal rbpjmj : real;
  signal wkfq : real;
  signal zlgmi : time;
  signal praxy : real;
  signal ej : real;
  signal mzicxgto : time;
  signal yeqw : real;
  signal o : real;
begin
  gkmdhm : entity work.muncdip
    port map (zhax => y, g => o, bgbrqesrf => yeqw);
  pypkt : entity work.muncdip
    port map (zhax => mzicxgto, g => ej, bgbrqesrf => praxy);
  l : entity work.muncdip
    port map (zhax => zlgmi, g => wkfq, bgbrqesrf => rbpjmj);
  wikaitlf : entity work.muncdip
    port map (zhax => nqjswmw, g => pisyg, bgbrqesrf => ctyntlet);
  
  -- Single-driven assignments
  syz <= 2#0_1_1#;
end fcqurvo;



-- Seed after: 16498301347908127209,17047277710231705797
