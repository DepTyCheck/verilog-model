-- Seed: 5256915772873460294,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity jxuxd is
  port (sqlhpopty : buffer bit; tvv : linkage std_logic_vector(2 to 1));
end jxuxd;

architecture ujy of jxuxd is
  
begin
  -- Single-driven assignments
  sqlhpopty <= '0';
end ujy;

entity lbe is
  port (miza : linkage severity_level; nzunohvin : inout real; zmvn : out time);
end lbe;

library ieee;
use ieee.std_logic_1164.all;

architecture jiz of lbe is
  signal jjuruesgdu : std_logic_vector(2 to 1);
  signal wqkhrnrpub : bit;
  signal isilf : std_logic_vector(2 to 1);
  signal nykcv : bit;
  signal gmfyggovwq : std_logic_vector(2 to 1);
  signal zswx : bit;
begin
  xpvoi : entity work.jxuxd
    port map (sqlhpopty => zswx, tvv => gmfyggovwq);
  pelejkwwku : entity work.jxuxd
    port map (sqlhpopty => nykcv, tvv => isilf);
  ald : entity work.jxuxd
    port map (sqlhpopty => wqkhrnrpub, tvv => jjuruesgdu);
  
  -- Single-driven assignments
  zmvn <= zmvn;
  nzunohvin <= 133.3_1_0_2_4;
  
  -- Multi-driven assignments
  gmfyggovwq <= "";
  isilf <= "";
  jjuruesgdu <= (others => '0');
  isilf <= gmfyggovwq;
end jiz;



-- Seed after: 12587448337205266400,3316342841050048249
