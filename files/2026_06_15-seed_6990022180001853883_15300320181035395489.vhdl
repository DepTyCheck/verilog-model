-- Seed: 6990022180001853883,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity aiv is
  port (oa : linkage std_logic_vector(0 to 2); sibc : out std_logic_vector(4 to 3));
end aiv;

architecture vkhsafovo of aiv is
  
begin
  -- Multi-driven assignments
  sibc <= (others => '0');
  sibc <= "";
  sibc <= "";
end vkhsafovo;

library ieee;
use ieee.std_logic_1164.all;

entity dgib is
  port (rbhv : in severity_level; uti : in integer; svzqc : in std_logic; owe : buffer time);
end dgib;

library ieee;
use ieee.std_logic_1164.all;

architecture ymgmjpjbar of dgib is
  signal vkngz : std_logic_vector(0 to 2);
  signal ribpefa : std_logic_vector(4 to 3);
  signal ahjrmfta : std_logic_vector(0 to 2);
begin
  xypmf : entity work.aiv
    port map (oa => ahjrmfta, sibc => ribpefa);
  tx : entity work.aiv
    port map (oa => ahjrmfta, sibc => ribpefa);
  apy : entity work.aiv
    port map (oa => vkngz, sibc => ribpefa);
  
  -- Single-driven assignments
  owe <= 2 sec;
  
  -- Multi-driven assignments
  ribpefa <= (others => '0');
  ribpefa <= (others => '0');
end ymgmjpjbar;

entity bw is
  port (nzpez : in time);
end bw;

library ieee;
use ieee.std_logic_1164.all;

architecture youbmjh of bw is
  signal ir : time;
  signal njxhjvpp : std_logic;
  signal dkbptd : integer;
  signal ub : severity_level;
  signal gu : time;
  signal ilytu : std_logic;
  signal zmx : integer;
  signal pim : time;
  signal qcfldbrlz : std_logic;
  signal wm : integer;
  signal fd : severity_level;
begin
  vmjdhmh : entity work.dgib
    port map (rbhv => fd, uti => wm, svzqc => qcfldbrlz, owe => pim);
  jmlc : entity work.dgib
    port map (rbhv => fd, uti => zmx, svzqc => ilytu, owe => gu);
  hmssj : entity work.dgib
    port map (rbhv => ub, uti => dkbptd, svzqc => njxhjvpp, owe => ir);
  
  -- Single-driven assignments
  fd <= FAILURE;
  zmx <= 102;
  wm <= 1_3;
  dkbptd <= 2#0_1_1_1#;
  
  -- Multi-driven assignments
  qcfldbrlz <= 'X';
  qcfldbrlz <= 'U';
  ilytu <= 'X';
end youbmjh;



-- Seed after: 6695427821769284200,15300320181035395489
