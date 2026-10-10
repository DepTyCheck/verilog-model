-- Seed: 3065280851406603920,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity vnfg is
  port (tqshryoi : buffer real; mqfgr : out std_logic_vector(3 to 2); fxt : in real; j : linkage bit);
end vnfg;

architecture ectyh of vnfg is
  
begin
  -- Single-driven assignments
  tqshryoi <= fxt;
  
  -- Multi-driven assignments
  mqfgr <= "";
end ectyh;

entity a is
  port (fcawj : buffer time; dhw : linkage integer);
end a;

library ieee;
use ieee.std_logic_1164.all;

architecture ttytnho of a is
  signal pxdx : bit;
  signal ouhsqxihkq : real;
  signal bhnpvvx : bit;
  signal nmdcdfc : real;
  signal xpl : bit;
  signal yxfvmlo : real;
  signal nksm : real;
  signal kounyo : bit;
  signal tvasizun : std_logic_vector(3 to 2);
  signal pn : real;
begin
  wjbu : entity work.vnfg
    port map (tqshryoi => pn, mqfgr => tvasizun, fxt => pn, j => kounyo);
  wsii : entity work.vnfg
    port map (tqshryoi => nksm, mqfgr => tvasizun, fxt => yxfvmlo, j => xpl);
  gncrj : entity work.vnfg
    port map (tqshryoi => yxfvmlo, mqfgr => tvasizun, fxt => nmdcdfc, j => bhnpvvx);
  dehskbfjv : entity work.vnfg
    port map (tqshryoi => ouhsqxihkq, mqfgr => tvasizun, fxt => ouhsqxihkq, j => pxdx);
  
  -- Single-driven assignments
  nmdcdfc <= 8#712.5_0_1_2#;
  fcawj <= fcawj;
  
  -- Multi-driven assignments
  tvasizun <= (others => '0');
  tvasizun <= tvasizun;
end ttytnho;

library ieee;
use ieee.std_logic_1164.all;

entity bem is
  port (bt : inout std_logic; bxhi : out std_logic_vector(2 downto 4));
end bem;

architecture bxgbwngu of bem is
  signal o : integer;
  signal pwbdq : time;
begin
  qljmal : entity work.a
    port map (fcawj => pwbdq, dhw => o);
end bxgbwngu;

library ieee;
use ieee.std_logic_1164.all;

entity tm is
  port (asl : buffer std_logic; od : out real);
end tm;

architecture vp of tm is
  
begin
  -- Single-driven assignments
  od <= 16#A_A_0_F_E.AA#;
  
  -- Multi-driven assignments
  asl <= 'W';
  asl <= 'L';
end vp;



-- Seed after: 15445425418982729729,511364357853360275
