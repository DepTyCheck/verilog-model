-- Seed: 6529288742626795352,7198033922882419595

library ieee;
use ieee.std_logic_1164.all;

entity gfoskqdm is
  port (wqyc : linkage std_logic; zptobaxawz : inout std_logic_vector(0 to 1));
end gfoskqdm;

architecture hfvlhd of gfoskqdm is
  
begin
  -- Multi-driven assignments
  zptobaxawz <= ('-', '0');
  zptobaxawz <= "X0";
  zptobaxawz <= ('L', 'Z');
  zptobaxawz <= "ZH";
end hfvlhd;

library ieee;
use ieee.std_logic_1164.all;

entity lsptaaue is
  port (esrw : linkage integer; nelxxdi : inout std_logic);
end lsptaaue;

library ieee;
use ieee.std_logic_1164.all;

architecture sroxlw of lsptaaue is
  signal ywjsp : std_logic_vector(0 to 1);
  signal vuxnxdosim : std_logic;
begin
  y : entity work.gfoskqdm
    port map (wqyc => vuxnxdosim, zptobaxawz => ywjsp);
end sroxlw;

library ieee;
use ieee.std_logic_1164.all;

entity kjbnoloii is
  port (rtqxwrstec : out time; bcw : in real_vector(0 downto 3); um : buffer std_logic_vector(2 downto 0); ipmsqsp : out std_logic);
end kjbnoloii;

architecture klqhqs of kjbnoloii is
  
begin
  -- Single-driven assignments
  rtqxwrstec <= rtqxwrstec;
  
  -- Multi-driven assignments
  ipmsqsp <= ipmsqsp;
  ipmsqsp <= ipmsqsp;
  ipmsqsp <= ipmsqsp;
  ipmsqsp <= ipmsqsp;
end klqhqs;

library ieee;
use ieee.std_logic_1164.all;

entity vti is
  port (ty : buffer std_logic; v : linkage std_logic; rjesrtnpb : buffer integer; jytl : linkage integer);
end vti;

library ieee;
use ieee.std_logic_1164.all;

architecture lp of vti is
  signal nytvs : std_logic_vector(0 to 1);
  signal otlku : std_logic;
  signal dqbleydc : std_logic;
  signal dssop : integer;
begin
  ct : entity work.lsptaaue
    port map (esrw => dssop, nelxxdi => dqbleydc);
  eimc : entity work.lsptaaue
    port map (esrw => jytl, nelxxdi => otlku);
  xuhea : entity work.gfoskqdm
    port map (wqyc => v, zptobaxawz => nytvs);
  ebpghtaw : entity work.lsptaaue
    port map (esrw => rjesrtnpb, nelxxdi => otlku);
  
  -- Multi-driven assignments
  otlku <= 'X';
  ty <= ty;
end lp;



-- Seed after: 455187478881770171,7198033922882419595
