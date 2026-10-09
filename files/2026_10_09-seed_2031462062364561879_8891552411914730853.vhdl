-- Seed: 2031462062364561879,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity iued is
  port (rjcxrkwbja : inout real_vector(3 downto 1); xdhppafxo : buffer std_logic; fp : out std_logic; imza : inout boolean);
end iued;

architecture xwkpyeb of iued is
  
begin
  
end xwkpyeb;

library ieee;
use ieee.std_logic_1164.all;

entity crer is
  port (ebthcoxklh : in time; nqmcl : buffer std_logic; opfynmxs : out std_logic; ahrsqtfqbl : out time);
end crer;

architecture vscwkyepw of crer is
  signal enexewwtnb : boolean;
  signal rqjckezs : real_vector(3 downto 1);
begin
  ffngyepguc : entity work.iued
    port map (rjcxrkwbja => rqjckezs, xdhppafxo => nqmcl, fp => opfynmxs, imza => enexewwtnb);
  
  -- Multi-driven assignments
  opfynmxs <= opfynmxs;
  opfynmxs <= '-';
  nqmcl <= opfynmxs;
  opfynmxs <= '0';
end vscwkyepw;

entity nqmgelnf is
  port (s : buffer time; hg : inout boolean);
end nqmgelnf;

library ieee;
use ieee.std_logic_1164.all;

architecture qowteoda of nqmgelnf is
  signal atlogcrg : std_logic;
begin
  mnmkqnd : entity work.crer
    port map (ebthcoxklh => s, nqmcl => atlogcrg, opfynmxs => atlogcrg, ahrsqtfqbl => s);
  
  -- Single-driven assignments
  hg <= hg;
end qowteoda;



-- Seed after: 1284541169292128453,8891552411914730853
