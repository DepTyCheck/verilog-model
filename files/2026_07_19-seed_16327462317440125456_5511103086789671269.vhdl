-- Seed: 16327462317440125456,5511103086789671269

entity onvaivpdw is
  port (rafq : linkage time; rrx : inout boolean; pxmukjbpy : in time);
end onvaivpdw;

architecture wliypyv of onvaivpdw is
  
begin
  -- Single-driven assignments
  rrx <= FALSE;
end wliypyv;

library ieee;
use ieee.std_logic_1164.all;

entity gyxc is
  port (rolhq : out std_logic; ezkuwrxdeh : in boolean);
end gyxc;

architecture pvt of gyxc is
  signal zmdm : boolean;
  signal ufvpyln : time;
  signal wwn : boolean;
  signal ggqfjm : boolean;
  signal uri : time;
  signal xmze : time;
  signal u : boolean;
  signal mdzq : time;
begin
  mrldlkxvq : entity work.onvaivpdw
    port map (rafq => mdzq, rrx => u, pxmukjbpy => xmze);
  a : entity work.onvaivpdw
    port map (rafq => uri, rrx => ggqfjm, pxmukjbpy => mdzq);
  dodjnsaw : entity work.onvaivpdw
    port map (rafq => xmze, rrx => wwn, pxmukjbpy => ufvpyln);
  vil : entity work.onvaivpdw
    port map (rafq => ufvpyln, rrx => zmdm, pxmukjbpy => xmze);
  
  -- Multi-driven assignments
  rolhq <= rolhq;
end pvt;

library ieee;
use ieee.std_logic_1164.all;

entity drfexr is
  port (sfh : buffer string(5 downto 3); qcsg : out bit; yl : out std_logic_vector(0 to 0));
end drfexr;

architecture szjiw of drfexr is
  signal fvbtpfw : boolean;
  signal gurvqgosx : time;
  signal qomcbejro : boolean;
  signal n : time;
begin
  expcd : entity work.onvaivpdw
    port map (rafq => n, rrx => qomcbejro, pxmukjbpy => n);
  yjbpgupcux : entity work.onvaivpdw
    port map (rafq => gurvqgosx, rrx => fvbtpfw, pxmukjbpy => n);
  
  -- Single-driven assignments
  sfh <= sfh;
  qcsg <= qcsg;
  
  -- Multi-driven assignments
  yl <= yl;
  yl <= (others => '1');
end szjiw;



-- Seed after: 8343249440889754170,5511103086789671269
