-- Seed: 5452878271681667334,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity nfifvoslh is
  port (v : inout std_logic; ucg : in boolean_vector(0 downto 3); ngb : buffer boolean);
end nfifvoslh;

architecture yx of nfifvoslh is
  
begin
  -- Multi-driven assignments
  v <= v;
  v <= 'L';
end yx;

entity cizs is
  port (zzavptsmp : linkage integer; oxn : out boolean; ehxrznnhq : buffer real; ef : buffer time);
end cizs;

library ieee;
use ieee.std_logic_1164.all;

architecture ypxxsoj of cizs is
  signal uy : boolean;
  signal uxpc : std_logic;
  signal ciyboroo : boolean;
  signal v : boolean;
  signal sy : boolean_vector(0 downto 3);
  signal fqvswnw : std_logic;
begin
  jmkphryxob : entity work.nfifvoslh
    port map (v => fqvswnw, ucg => sy, ngb => v);
  wntwlsrq : entity work.nfifvoslh
    port map (v => fqvswnw, ucg => sy, ngb => oxn);
  sddsvm : entity work.nfifvoslh
    port map (v => fqvswnw, ucg => sy, ngb => ciyboroo);
  iok : entity work.nfifvoslh
    port map (v => uxpc, ucg => sy, ngb => uy);
  
  -- Single-driven assignments
  ehxrznnhq <= 8#3.63#;
  sy <= sy;
  ef <= ef;
  
  -- Multi-driven assignments
  fqvswnw <= 'U';
end ypxxsoj;



-- Seed after: 16848961533608347556,8891552411914730853
