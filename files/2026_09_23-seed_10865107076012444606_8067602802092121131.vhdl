-- Seed: 10865107076012444606,8067602802092121131

entity hjfslcgal is
  port (daiivsw : inout integer; rozw : buffer real; klrepiee : in real; yssbi : inout bit_vector(2 to 0));
end hjfslcgal;

architecture rmmtdn of hjfslcgal is
  
begin
  -- Single-driven assignments
  yssbi <= yssbi;
  daiivsw <= daiivsw;
  rozw <= klrepiee;
end rmmtdn;

entity gbctihtyyc is
  port (piconv : out time; bymk : out time_vector(0 to 2));
end gbctihtyyc;

architecture ybb of gbctihtyyc is
  signal brqtkulx : bit_vector(2 to 0);
  signal czoghmpbmx : real;
  signal spujtkpewr : integer;
  signal zt : bit_vector(2 to 0);
  signal acodm : real;
  signal tlns : real;
  signal syzyexrpmq : integer;
  signal pwhsytbj : bit_vector(2 to 0);
  signal i : real;
  signal gps : integer;
begin
  qv : entity work.hjfslcgal
    port map (daiivsw => gps, rozw => i, klrepiee => i, yssbi => pwhsytbj);
  ilpxa : entity work.hjfslcgal
    port map (daiivsw => syzyexrpmq, rozw => tlns, klrepiee => acodm, yssbi => zt);
  zanbgp : entity work.hjfslcgal
    port map (daiivsw => spujtkpewr, rozw => acodm, klrepiee => czoghmpbmx, yssbi => brqtkulx);
  
  -- Single-driven assignments
  czoghmpbmx <= i;
  piconv <= 8#007.2_3_7# ps;
  bymk <= (2#0_0_0_0_1# us, 0 min, 16#BD# ns);
end ybb;



-- Seed after: 12778339073597756145,8067602802092121131
