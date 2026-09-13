-- Seed: 14280112069660989180,10754487200446211253

entity zcmyzqjb is
  port (t : buffer real; niupbpvlww : linkage bit_vector(2 downto 3));
end zcmyzqjb;

architecture qetddix of zcmyzqjb is
  
begin
  -- Single-driven assignments
  t <= 2#1_1_0_1.0_0_1_0_0#;
end qetddix;

library ieee;
use ieee.std_logic_1164.all;

entity dc is
  port (rwaof : inout std_logic; c : inout bit);
end dc;

architecture xvroordamy of dc is
  signal fm : bit_vector(2 downto 3);
  signal w : real;
begin
  qsrrpaolo : entity work.zcmyzqjb
    port map (t => w, niupbpvlww => fm);
  
  -- Single-driven assignments
  c <= '0';
  
  -- Multi-driven assignments
  rwaof <= '-';
  rwaof <= rwaof;
  rwaof <= '1';
end xvroordamy;

library ieee;
use ieee.std_logic_1164.all;

entity ivvyyext is
  port (non : in integer; p : out real; lnqmoc : inout std_logic);
end ivvyyext;

library ieee;
use ieee.std_logic_1164.all;

architecture mp of ivvyyext is
  signal rms : bit_vector(2 downto 3);
  signal ye : bit_vector(2 downto 3);
  signal vhd : real;
  signal bxjr : bit;
  signal j : bit;
  signal belmrdrboz : std_logic;
begin
  ngobgauzk : entity work.dc
    port map (rwaof => belmrdrboz, c => j);
  kkandhbg : entity work.dc
    port map (rwaof => lnqmoc, c => bxjr);
  pdtxhnj : entity work.zcmyzqjb
    port map (t => vhd, niupbpvlww => ye);
  anhfn : entity work.zcmyzqjb
    port map (t => p, niupbpvlww => rms);
  
  -- Multi-driven assignments
  belmrdrboz <= '1';
  lnqmoc <= belmrdrboz;
  belmrdrboz <= lnqmoc;
end mp;

entity r is
  port (c : linkage bit);
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture kikksgvb of r is
  signal qn : bit;
  signal hwbm : std_logic;
  signal e : bit_vector(2 downto 3);
  signal hjg : real;
  signal xidhevfrof : bit;
  signal abq : std_logic;
  signal di : bit_vector(2 downto 3);
  signal dsnqsep : real;
begin
  uhng : entity work.zcmyzqjb
    port map (t => dsnqsep, niupbpvlww => di);
  jwgjuppui : entity work.dc
    port map (rwaof => abq, c => xidhevfrof);
  hgo : entity work.zcmyzqjb
    port map (t => hjg, niupbpvlww => e);
  zimmdyu : entity work.dc
    port map (rwaof => hwbm, c => qn);
end kikksgvb;



-- Seed after: 9153537991224964434,10754487200446211253
