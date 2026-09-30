-- Seed: 18215286354412020842,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity fjkzvxyb is
  port (tqkhrj : inout std_logic_vector(1 downto 4));
end fjkzvxyb;

architecture hqbyo of fjkzvxyb is
  
begin
  -- Multi-driven assignments
  tqkhrj <= tqkhrj;
  tqkhrj <= tqkhrj;
  tqkhrj <= (others => '0');
  tqkhrj <= tqkhrj;
end hqbyo;

entity zczeefv is
  port (d : inout time);
end zczeefv;

library ieee;
use ieee.std_logic_1164.all;

architecture nyjfpa of zczeefv is
  signal obqjtnon : std_logic_vector(1 downto 4);
  signal krrpyqtlj : std_logic_vector(1 downto 4);
  signal xjmqltexyo : std_logic_vector(1 downto 4);
begin
  dtvv : entity work.fjkzvxyb
    port map (tqkhrj => xjmqltexyo);
  mpean : entity work.fjkzvxyb
    port map (tqkhrj => krrpyqtlj);
  iycfi : entity work.fjkzvxyb
    port map (tqkhrj => obqjtnon);
  mimayqqs : entity work.fjkzvxyb
    port map (tqkhrj => krrpyqtlj);
  
  -- Single-driven assignments
  d <= 2#0_1_0_0_0.00# us;
end nyjfpa;

library ieee;
use ieee.std_logic_1164.all;

entity vhtbftvxyj is
  port (lknpqf : out std_logic; g : out bit);
end vhtbftvxyj;

architecture vhcqbopbmn of vhtbftvxyj is
  signal xfrsvog : time;
begin
  ngdzk : entity work.zczeefv
    port map (d => xfrsvog);
  
  -- Multi-driven assignments
  lknpqf <= 'X';
end vhcqbopbmn;

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (t : in std_logic_vector(2 downto 3); rb : buffer std_logic);
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture shxdefjepj of o is
  signal e : std_logic_vector(1 downto 4);
  signal nxlxgikybz : time;
  signal czahaej : std_logic_vector(1 downto 4);
begin
  xqkm : entity work.fjkzvxyb
    port map (tqkhrj => czahaej);
  whuggo : entity work.zczeefv
    port map (d => nxlxgikybz);
  aacipurtx : entity work.fjkzvxyb
    port map (tqkhrj => czahaej);
  kdblzpcbqf : entity work.fjkzvxyb
    port map (tqkhrj => e);
  
  -- Multi-driven assignments
  rb <= 'W';
  rb <= rb;
  czahaej <= t;
end shxdefjepj;



-- Seed after: 17625132749019048529,12260394286515585877
