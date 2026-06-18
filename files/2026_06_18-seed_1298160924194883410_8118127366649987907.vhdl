-- Seed: 1298160924194883410,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity pf is
  port (nr : out integer; zq : inout character; cwzoiit : out string(5 downto 2); lusm : inout std_logic);
end pf;

architecture udugszoxg of pf is
  
begin
  -- Single-driven assignments
  cwzoiit <= ('z', 'r', 'p', 'd');
  
  -- Multi-driven assignments
  lusm <= 'H';
  lusm <= 'H';
  lusm <= 'Z';
  lusm <= 'U';
end udugszoxg;

library ieee;
use ieee.std_logic_1164.all;

entity tl is
  port (omzbd : buffer severity_level; vet : inout severity_level; suro : in std_logic; gwyj : buffer string(2 to 1));
end tl;

library ieee;
use ieee.std_logic_1164.all;

architecture ubtpv of tl is
  signal wkv : string(5 downto 2);
  signal qtmbrvmoxq : character;
  signal tyufran : integer;
  signal vki : string(5 downto 2);
  signal s : character;
  signal dnye : integer;
  signal dwv : string(5 downto 2);
  signal ixm : character;
  signal qpbm : integer;
  signal ntg : std_logic;
  signal jvnpnrkh : string(5 downto 2);
  signal drth : character;
  signal id : integer;
begin
  hyac : entity work.pf
    port map (nr => id, zq => drth, cwzoiit => jvnpnrkh, lusm => ntg);
  pbf : entity work.pf
    port map (nr => qpbm, zq => ixm, cwzoiit => dwv, lusm => ntg);
  yhvzme : entity work.pf
    port map (nr => dnye, zq => s, cwzoiit => vki, lusm => ntg);
  vq : entity work.pf
    port map (nr => tyufran, zq => qtmbrvmoxq, cwzoiit => wkv, lusm => ntg);
  
  -- Multi-driven assignments
  ntg <= '0';
  ntg <= '0';
end ubtpv;



-- Seed after: 14433153627626155271,8118127366649987907
