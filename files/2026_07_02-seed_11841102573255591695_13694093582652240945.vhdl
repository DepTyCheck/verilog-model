-- Seed: 11841102573255591695,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity ruzioiv is
  port (gwih : in bit; zwhaenx : linkage std_logic_vector(0 downto 3));
end ruzioiv;

architecture qrkamjstw of ruzioiv is
  
begin
  
end qrkamjstw;

library ieee;
use ieee.std_logic_1164.all;

entity uggjlpnkd is
  port (votguem : buffer severity_level; esfbwimdq : buffer boolean; d : out real; wash : inout std_logic_vector(3 to 4));
end uggjlpnkd;

library ieee;
use ieee.std_logic_1164.all;

architecture sebzmr of uggjlpnkd is
  signal rmkizqkvu : std_logic_vector(0 downto 3);
  signal sggotqtyz : bit;
  signal i : bit;
  signal eqh : std_logic_vector(0 downto 3);
  signal jkgxuiry : bit;
begin
  ra : entity work.ruzioiv
    port map (gwih => jkgxuiry, zwhaenx => eqh);
  xynfyaz : entity work.ruzioiv
    port map (gwih => i, zwhaenx => eqh);
  pbx : entity work.ruzioiv
    port map (gwih => sggotqtyz, zwhaenx => rmkizqkvu);
end sebzmr;

library ieee;
use ieee.std_logic_1164.all;

entity rlqrafxro is
  port (c : inout integer; gw : linkage std_logic; ojzb : in bit_vector(4 to 3));
end rlqrafxro;

library ieee;
use ieee.std_logic_1164.all;

architecture ifevf of rlqrafxro is
  signal menpv : std_logic_vector(0 downto 3);
  signal kuc : bit;
  signal amn : std_logic_vector(3 to 4);
  signal wiziylcusm : real;
  signal cydtrqbxwi : boolean;
  signal xgqtxw : severity_level;
  signal ttkbmemh : std_logic_vector(0 downto 3);
  signal uwxlu : bit;
begin
  dtubij : entity work.ruzioiv
    port map (gwih => uwxlu, zwhaenx => ttkbmemh);
  ixykvhdmw : entity work.uggjlpnkd
    port map (votguem => xgqtxw, esfbwimdq => cydtrqbxwi, d => wiziylcusm, wash => amn);
  zjir : entity work.ruzioiv
    port map (gwih => uwxlu, zwhaenx => ttkbmemh);
  wzp : entity work.ruzioiv
    port map (gwih => kuc, zwhaenx => menpv);
  
  -- Single-driven assignments
  kuc <= '1';
  
  -- Multi-driven assignments
  ttkbmemh <= "";
  ttkbmemh <= "";
end ifevf;



-- Seed after: 14850971034955154583,13694093582652240945
