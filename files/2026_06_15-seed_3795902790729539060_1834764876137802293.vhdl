-- Seed: 3795902790729539060,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity zfsp is
  port (kuoh : out std_logic);
end zfsp;

architecture pzvnwhbco of zfsp is
  
begin
  -- Multi-driven assignments
  kuoh <= '1';
  kuoh <= 'L';
  kuoh <= '1';
  kuoh <= 'X';
end pzvnwhbco;

entity kogmiyr is
  port (m : out severity_level);
end kogmiyr;

library ieee;
use ieee.std_logic_1164.all;

architecture kvw of kogmiyr is
  signal qtake : std_logic;
  signal l : std_logic;
begin
  r : entity work.zfsp
    port map (kuoh => l);
  avpkya : entity work.zfsp
    port map (kuoh => l);
  dudm : entity work.zfsp
    port map (kuoh => qtake);
  yk : entity work.zfsp
    port map (kuoh => l);
  
  -- Single-driven assignments
  m <= ERROR;
  
  -- Multi-driven assignments
  l <= '-';
  qtake <= 'W';
  qtake <= '1';
  l <= 'Z';
end kvw;

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (sswza : buffer std_logic_vector(3 downto 4); fnriimkytl : linkage real; k : inout integer);
end b;

library ieee;
use ieee.std_logic_1164.all;

architecture pn of b is
  signal qefmlub : std_logic;
begin
  oqfpfbg : entity work.zfsp
    port map (kuoh => qefmlub);
  jss : entity work.zfsp
    port map (kuoh => qefmlub);
  
  -- Single-driven assignments
  k <= 16#DFDE#;
  
  -- Multi-driven assignments
  qefmlub <= '0';
  sswza <= (others => '0');
end pn;

entity szcxcnywia is
  port (rhsxxa : out real_vector(4 downto 3));
end szcxcnywia;

library ieee;
use ieee.std_logic_1164.all;

architecture too of szcxcnywia is
  signal cyvwtynpdn : std_logic;
  signal wpy : integer;
  signal puaotln : real;
  signal gdzeughvqn : integer;
  signal klyih : real;
  signal xbpvlpj : integer;
  signal qpxnpzwu : real;
  signal bbnmqmfjz : std_logic_vector(3 downto 4);
begin
  unstytr : entity work.b
    port map (sswza => bbnmqmfjz, fnriimkytl => qpxnpzwu, k => xbpvlpj);
  zmchl : entity work.b
    port map (sswza => bbnmqmfjz, fnriimkytl => klyih, k => gdzeughvqn);
  ovbwamefet : entity work.b
    port map (sswza => bbnmqmfjz, fnriimkytl => puaotln, k => wpy);
  ewgfbz : entity work.zfsp
    port map (kuoh => cyvwtynpdn);
  
  -- Single-driven assignments
  rhsxxa <= (16#1C6B.E_9#, 16#1_E_5_3_4.6ABE5#);
  
  -- Multi-driven assignments
  bbnmqmfjz <= (others => '0');
  bbnmqmfjz <= (others => '0');
end too;



-- Seed after: 17179786503470895124,1834764876137802293
