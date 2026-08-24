-- Seed: 9489044293517819146,16159265764638711791

library ieee;
use ieee.std_logic_1164.all;

entity ejpstk is
  port (uezschz : in std_logic_vector(3 to 2); g : inout integer; nzimymov : out std_logic_vector(0 to 2); kdt : buffer time);
end ejpstk;

architecture b of ejpstk is
  
begin
  -- Multi-driven assignments
  nzimymov <= ('X', 'U', 'H');
  nzimymov <= "HWZ";
end b;

entity souyoaq is
  port (k : linkage integer_vector(2 downto 3));
end souyoaq;

architecture bvxkorygly of souyoaq is
  
begin
  
end bvxkorygly;

library ieee;
use ieee.std_logic_1164.all;

entity rdsfd is
  port (abe : linkage std_logic; lz : out real);
end rdsfd;

library ieee;
use ieee.std_logic_1164.all;

architecture mrmmonalx of rdsfd is
  signal ggf : time;
  signal gwkdirqmdk : integer;
  signal mrbv : std_logic_vector(3 to 2);
  signal nahxbaqer : time;
  signal irhpl : integer;
  signal hnkjrkjq : std_logic_vector(3 to 2);
  signal dacf : time;
  signal ykzeapfjh : std_logic_vector(0 to 2);
  signal ugvh : integer;
  signal vnzdtoolff : std_logic_vector(3 to 2);
begin
  uzrrjbqol : entity work.ejpstk
    port map (uezschz => vnzdtoolff, g => ugvh, nzimymov => ykzeapfjh, kdt => dacf);
  mcsgb : entity work.ejpstk
    port map (uezschz => hnkjrkjq, g => irhpl, nzimymov => ykzeapfjh, kdt => nahxbaqer);
  plgcc : entity work.ejpstk
    port map (uezschz => mrbv, g => gwkdirqmdk, nzimymov => ykzeapfjh, kdt => ggf);
  
  -- Single-driven assignments
  lz <= lz;
end mrmmonalx;

library ieee;
use ieee.std_logic_1164.all;

entity gjspl is
  port (db : inout real; wwxhztmmtd : out std_logic; kmwdgz : in bit);
end gjspl;

library ieee;
use ieee.std_logic_1164.all;

architecture bbpotlks of gjspl is
  signal twrxgcj : std_logic;
  signal mmzixovj : real;
  signal s : real;
  signal sjfbbzm : std_logic;
  signal emorp : time;
  signal fvj : std_logic_vector(0 to 2);
  signal dzsbeib : integer;
  signal plwaky : std_logic_vector(3 to 2);
begin
  cruq : entity work.ejpstk
    port map (uezschz => plwaky, g => dzsbeib, nzimymov => fvj, kdt => emorp);
  cssgth : entity work.rdsfd
    port map (abe => sjfbbzm, lz => s);
  ulzn : entity work.rdsfd
    port map (abe => wwxhztmmtd, lz => mmzixovj);
  yfg : entity work.rdsfd
    port map (abe => twrxgcj, lz => db);
  
  -- Multi-driven assignments
  fvj <= fvj;
end bbpotlks;



-- Seed after: 12948056039746666635,16159265764638711791
