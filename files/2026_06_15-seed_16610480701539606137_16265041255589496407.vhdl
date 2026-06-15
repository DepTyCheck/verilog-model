-- Seed: 16610480701539606137,16265041255589496407

library ieee;
use ieee.std_logic_1164.all;

entity xfy is
  port (gmn : in std_logic_vector(0 downto 4); zx : out std_logic; bavclbsijk : linkage real; ozuovzw : out std_logic_vector(0 downto 3));
end xfy;



architecture fhngawzm of xfy is
  
begin
  
end fhngawzm;

library ieee;
use ieee.std_logic_1164.all;

entity ggxwxbxrny is
  port (tvfmaz : linkage integer; drebdjancv : linkage std_logic_vector(2 to 3); pyzmrwmcv : inout time_vector(1 to 3));
end ggxwxbxrny;

library ieee;
use ieee.std_logic_1164.all;

architecture sqjro of ggxwxbxrny is
  signal punbrih : real;
  signal syjmyx : std_logic_vector(0 downto 4);
  signal pl : std_logic_vector(0 downto 3);
  signal ioqdsdmv : std_logic;
  signal mqg : std_logic_vector(0 downto 4);
  signal j : real;
  signal l : std_logic;
  signal xkobjys : std_logic_vector(0 downto 3);
begin
  qqkbwnn : entity work.xfy
    port map (gmn => xkobjys, zx => l, bavclbsijk => j, ozuovzw => xkobjys);
  dxcwhea : entity work.xfy
    port map (gmn => mqg, zx => ioqdsdmv, bavclbsijk => j, ozuovzw => pl);
  ioikxyl : entity work.xfy
    port map (gmn => pl, zx => l, bavclbsijk => j, ozuovzw => xkobjys);
  eqxsrgin : entity work.xfy
    port map (gmn => syjmyx, zx => ioqdsdmv, bavclbsijk => punbrih, ozuovzw => pl);
end sqjro;

library ieee;
use ieee.std_logic_1164.all;

entity zsoxpdkme is
  port (scbpp : inout integer_vector(3 downto 1); qbmcieudq : out std_logic);
end zsoxpdkme;

library ieee;
use ieee.std_logic_1164.all;

architecture pciehgt of zsoxpdkme is
  signal xjg : std_logic;
  signal mzdjlj : std_logic_vector(0 downto 4);
  signal jtpyue : std_logic_vector(0 downto 3);
  signal ml : real;
  signal fqspl : std_logic_vector(0 downto 3);
begin
  mvjqwzuamu : entity work.xfy
    port map (gmn => fqspl, zx => qbmcieudq, bavclbsijk => ml, ozuovzw => jtpyue);
  uk : entity work.xfy
    port map (gmn => mzdjlj, zx => xjg, bavclbsijk => ml, ozuovzw => fqspl);
end pciehgt;



entity jxtdgfh is
  port (jpdctzgjbd : buffer integer; enialsrx : inout real);
end jxtdgfh;

library ieee;
use ieee.std_logic_1164.all;

architecture nusknwfbf of jxtdgfh is
  signal trwhbqff : time_vector(1 to 3);
  signal stqcetyvp : std_logic_vector(2 to 3);
  signal th : std_logic_vector(0 downto 3);
  signal cgem : std_logic;
  signal ftp : std_logic_vector(0 downto 4);
  signal wj : std_logic;
  signal noanc : integer_vector(3 downto 1);
begin
  luilnir : entity work.zsoxpdkme
    port map (scbpp => noanc, qbmcieudq => wj);
  pxk : entity work.xfy
    port map (gmn => ftp, zx => cgem, bavclbsijk => enialsrx, ozuovzw => th);
  svxcfoyqdg : entity work.ggxwxbxrny
    port map (tvfmaz => jpdctzgjbd, drebdjancv => stqcetyvp, pyzmrwmcv => trwhbqff);
end nusknwfbf;



-- Seed after: 3463454830204949933,16265041255589496407
