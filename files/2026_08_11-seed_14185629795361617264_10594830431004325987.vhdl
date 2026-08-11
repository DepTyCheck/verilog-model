-- Seed: 14185629795361617264,10594830431004325987

library ieee;
use ieee.std_logic_1164.all;

entity kemhq is
  port (xt : inout integer; tplwlez : inout std_logic_vector(3 downto 3); j : in real; hkuweoi : inout time_vector(4 to 3));
end kemhq;

architecture kjpikqobsi of kemhq is
  
begin
  -- Single-driven assignments
  xt <= xt;
  
  -- Multi-driven assignments
  tplwlez <= (others => 'W');
  tplwlez <= tplwlez;
  tplwlez <= (others => 'Z');
  tplwlez <= "X";
end kjpikqobsi;

entity yzjch is
  port (spkqrruhep : inout bit_vector(1 to 1));
end yzjch;

library ieee;
use ieee.std_logic_1164.all;

architecture nxfmq of yzjch is
  signal sczgv : time_vector(4 to 3);
  signal egprdajztv : real;
  signal ixepncgl : integer;
  signal k : time_vector(4 to 3);
  signal uvmnbpvx : std_logic_vector(3 downto 3);
  signal mlmdkghea : integer;
  signal d : time_vector(4 to 3);
  signal agqygj : real;
  signal cntp : std_logic_vector(3 downto 3);
  signal zggkssxvdc : integer;
begin
  botw : entity work.kemhq
    port map (xt => zggkssxvdc, tplwlez => cntp, j => agqygj, hkuweoi => d);
  o : entity work.kemhq
    port map (xt => mlmdkghea, tplwlez => uvmnbpvx, j => agqygj, hkuweoi => k);
  xwq : entity work.kemhq
    port map (xt => ixepncgl, tplwlez => cntp, j => egprdajztv, hkuweoi => sczgv);
  
  -- Single-driven assignments
  agqygj <= 2#1_0_1_1.0#;
  egprdajztv <= agqygj;
  spkqrruhep <= (others => '0');
  
  -- Multi-driven assignments
  cntp <= "0";
end nxfmq;



-- Seed after: 6360915764875134606,10594830431004325987
