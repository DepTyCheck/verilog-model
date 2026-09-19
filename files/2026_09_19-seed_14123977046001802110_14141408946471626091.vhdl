-- Seed: 14123977046001802110,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity ifi is
  port (tezkeej : linkage bit; vxv : inout bit; cwwrwmoab : linkage std_logic);
end ifi;

architecture mxdfx of ifi is
  
begin
  -- Single-driven assignments
  vxv <= '0';
end mxdfx;

library ieee;
use ieee.std_logic_1164.all;

entity eytgvjzgaw is
  port (xkd : buffer real_vector(3 downto 0); a : in std_logic; iro : in std_logic_vector(4 downto 3));
end eytgvjzgaw;

library ieee;
use ieee.std_logic_1164.all;

architecture nahvph of eytgvjzgaw is
  signal bfmxs : bit;
  signal rchkdjw : bit;
  signal tb : bit;
  signal qepz : bit;
  signal hhzqok : std_logic;
  signal r : bit;
  signal dbtmadtqst : bit;
  signal ldgvff : bit;
  signal bpcocpzonq : bit;
begin
  odqhcd : entity work.ifi
    port map (tezkeej => bpcocpzonq, vxv => ldgvff, cwwrwmoab => a);
  vjea : entity work.ifi
    port map (tezkeej => dbtmadtqst, vxv => r, cwwrwmoab => hhzqok);
  mtbir : entity work.ifi
    port map (tezkeej => qepz, vxv => tb, cwwrwmoab => a);
  vxspi : entity work.ifi
    port map (tezkeej => rchkdjw, vxv => bfmxs, cwwrwmoab => a);
  
  -- Single-driven assignments
  xkd <= (8#5_0_6_3.7#, 2.0203, 16#C_D_0_7.8C387#, 16#A5B.5_E#);
end nahvph;

entity zsqvnqdc is
  port (yfnhlyjaw : inout boolean);
end zsqvnqdc;

library ieee;
use ieee.std_logic_1164.all;

architecture s of zsqvnqdc is
  signal xng : std_logic_vector(4 downto 3);
  signal ly : real_vector(3 downto 0);
  signal lfliyloupx : std_logic;
  signal amzowsfa : bit;
  signal gmkcuibvh : bit;
begin
  d : entity work.ifi
    port map (tezkeej => gmkcuibvh, vxv => amzowsfa, cwwrwmoab => lfliyloupx);
  pwxk : entity work.eytgvjzgaw
    port map (xkd => ly, a => lfliyloupx, iro => xng);
  
  -- Single-driven assignments
  yfnhlyjaw <= FALSE;
  
  -- Multi-driven assignments
  lfliyloupx <= lfliyloupx;
  xng <= xng;
end s;



-- Seed after: 803975902766178247,14141408946471626091
