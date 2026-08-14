-- Seed: 6608981276797326678,8437298063418820479

library ieee;
use ieee.std_logic_1164.all;

entity blreqaumur is
  port (grnrte : in severity_level; bremtmchlz : in std_logic; bw : out integer);
end blreqaumur;

architecture xxkyuuhwso of blreqaumur is
  
begin
  
end xxkyuuhwso;

entity apwazewfl is
  port (zveicf : linkage integer; lspzovcfsq : inout time_vector(1 downto 3); uwszbxjroq : buffer integer_vector(1 to 1); ykpzrxfdzx : out time);
end apwazewfl;

architecture yp of apwazewfl is
  
begin
  
end yp;

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (dhnervi : out std_logic_vector(1 downto 0); kpw : out bit);
end i;

library ieee;
use ieee.std_logic_1164.all;

architecture qfgo of i is
  signal qaydf : integer;
  signal nt : severity_level;
  signal ehqnqgnqgt : time;
  signal cjjzrepn : integer_vector(1 to 1);
  signal mtnslmpom : time_vector(1 downto 3);
  signal dajznu : integer;
  signal qclitchey : integer;
  signal vrjzir : std_logic;
  signal ljyxdclnp : severity_level;
begin
  wndotpm : entity work.blreqaumur
    port map (grnrte => ljyxdclnp, bremtmchlz => vrjzir, bw => qclitchey);
  dyacuv : entity work.apwazewfl
    port map (zveicf => dajznu, lspzovcfsq => mtnslmpom, uwszbxjroq => cjjzrepn, ykpzrxfdzx => ehqnqgnqgt);
  jtnmzddobq : entity work.blreqaumur
    port map (grnrte => nt, bremtmchlz => vrjzir, bw => qaydf);
  
  -- Single-driven assignments
  kpw <= kpw;
  ljyxdclnp <= ljyxdclnp;
  
  -- Multi-driven assignments
  dhnervi <= dhnervi;
  dhnervi <= dhnervi;
  dhnervi <= "LZ";
end qfgo;



-- Seed after: 14941834055089229592,8437298063418820479
