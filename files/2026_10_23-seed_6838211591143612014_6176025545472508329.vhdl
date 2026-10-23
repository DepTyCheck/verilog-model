-- Seed: 6838211591143612014,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity kuzvcnets is
  port (e : inout std_logic; kmvblks : in std_logic_vector(1 downto 3));
end kuzvcnets;

architecture n of kuzvcnets is
  
begin
  -- Multi-driven assignments
  e <= 'Z';
  e <= e;
  e <= 'Z';
end n;

entity rprvmibsm is
  port (toouwldat : out integer);
end rprvmibsm;

architecture pbkecjxzl of rprvmibsm is
  
begin
  -- Single-driven assignments
  toouwldat <= 8#2_6_1_0_2#;
end pbkecjxzl;

library ieee;
use ieee.std_logic_1164.all;

entity clpfyv is
  port (qgl : linkage string(5 downto 4); iefmuk : buffer std_logic_vector(0 downto 3); najyytktq : in std_logic);
end clpfyv;

library ieee;
use ieee.std_logic_1164.all;

architecture s of clpfyv is
  signal fmbavpnuz : std_logic_vector(1 downto 3);
  signal sisryss : std_logic;
  signal kyjuzrp : integer;
begin
  n : entity work.rprvmibsm
    port map (toouwldat => kyjuzrp);
  ioadx : entity work.kuzvcnets
    port map (e => sisryss, kmvblks => fmbavpnuz);
  kfb : entity work.kuzvcnets
    port map (e => sisryss, kmvblks => iefmuk);
  
  -- Multi-driven assignments
  sisryss <= najyytktq;
  iefmuk <= (others => '0');
end s;

entity wzofiqihh is
  port (nruvhtwr : inout time);
end wzofiqihh;

library ieee;
use ieee.std_logic_1164.all;

architecture cbehadqen of wzofiqihh is
  signal xvkqc : std_logic_vector(1 downto 3);
  signal slpamie : std_logic;
  signal yqye : std_logic_vector(1 downto 3);
  signal vc : std_logic;
begin
  nlzza : entity work.kuzvcnets
    port map (e => vc, kmvblks => yqye);
  nulfbw : entity work.kuzvcnets
    port map (e => vc, kmvblks => yqye);
  jax : entity work.kuzvcnets
    port map (e => slpamie, kmvblks => xvkqc);
  
  -- Single-driven assignments
  nruvhtwr <= 444.01223 fs;
  
  -- Multi-driven assignments
  vc <= 'W';
  vc <= 'X';
end cbehadqen;



-- Seed after: 12051460492336445261,6176025545472508329
