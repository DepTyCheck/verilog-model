-- Seed: 17491893925716241555,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity ap is
  port (ney : out time; gphqtbsygd : out real; kpjgh : out std_logic);
end ap;

architecture yxguy of ap is
  
begin
  -- Single-driven assignments
  gphqtbsygd <= 8#3_5.140#;
  ney <= 1 hr;
  
  -- Multi-driven assignments
  kpjgh <= '0';
  kpjgh <= 'H';
  kpjgh <= 'W';
end yxguy;

library ieee;
use ieee.std_logic_1164.all;

entity oqbt is
  port (kvan : out time_vector(2 downto 1); rqwam : out real; mewmfgpmkc : buffer std_logic; b : buffer time_vector(1 to 4));
end oqbt;

architecture chad of oqbt is
  signal ls : time;
  signal ppzqfhiwkt : real;
  signal vnqhb : time;
begin
  zioajqest : entity work.ap
    port map (ney => vnqhb, gphqtbsygd => ppzqfhiwkt, kpjgh => mewmfgpmkc);
  bnjpntb : entity work.ap
    port map (ney => ls, gphqtbsygd => rqwam, kpjgh => mewmfgpmkc);
  
  -- Single-driven assignments
  b <= (2_1 ms, 2#0_1# ms, 10211 ps, 332.4340 us);
  kvan <= (16#6_6_2.F8D86# ms, 2_3_2_0.34 ns);
  
  -- Multi-driven assignments
  mewmfgpmkc <= 'W';
end chad;



-- Seed after: 4808371044858719704,17047277710231705797
