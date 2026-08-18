-- Seed: 14140934508846320633,5983430343285687595

entity iqdmzcq is
  port (yhkp : out integer; tk : buffer severity_level);
end iqdmzcq;

architecture cauaanmxg of iqdmzcq is
  
begin
  
end cauaanmxg;

library ieee;
use ieee.std_logic_1164.all;

entity cnkgyjsamb is
  port (mhgbh : inout time; cpqmsehcqw : out std_logic_vector(4 downto 2); rswpy : out integer; vam : buffer integer);
end cnkgyjsamb;

architecture xqlgmaycbx of cnkgyjsamb is
  signal bocpxygbpb : severity_level;
  signal ohajemhk : severity_level;
begin
  ixcbziks : entity work.iqdmzcq
    port map (yhkp => vam, tk => ohajemhk);
  uejhhacljz : entity work.iqdmzcq
    port map (yhkp => rswpy, tk => bocpxygbpb);
  
  -- Single-driven assignments
  mhgbh <= mhgbh;
  
  -- Multi-driven assignments
  cpqmsehcqw <= cpqmsehcqw;
end xqlgmaycbx;

entity xldkiihds is
  port (kitr : out real; yqxl : out integer; f : linkage time_vector(3 to 2); blswinfeih : in time);
end xldkiihds;

library ieee;
use ieee.std_logic_1164.all;

architecture qdr of xldkiihds is
  signal bgv : integer;
  signal bfplx : integer;
  signal jo : std_logic_vector(4 downto 2);
  signal cdgurxo : time;
  signal fwpsmzkqc : integer;
  signal lu : time;
  signal qxzerdqsos : integer;
  signal kporsmc : integer;
  signal cz : std_logic_vector(4 downto 2);
  signal mqa : time;
  signal dvq : severity_level;
  signal ukqpxqomca : integer;
begin
  sclehy : entity work.iqdmzcq
    port map (yhkp => ukqpxqomca, tk => dvq);
  ixlpxpy : entity work.cnkgyjsamb
    port map (mhgbh => mqa, cpqmsehcqw => cz, rswpy => kporsmc, vam => qxzerdqsos);
  yvxtogv : entity work.cnkgyjsamb
    port map (mhgbh => lu, cpqmsehcqw => cz, rswpy => fwpsmzkqc, vam => yqxl);
  vo : entity work.cnkgyjsamb
    port map (mhgbh => cdgurxo, cpqmsehcqw => jo, rswpy => bfplx, vam => bgv);
end qdr;



-- Seed after: 9561747524849084046,5983430343285687595
