-- Seed: 5462893396412877284,4122021602305298647

library ieee;
use ieee.std_logic_1164.all;

entity lwtsxivm is
  port (orutvem : in time; thcqagdr : in std_logic; bukry : in integer);
end lwtsxivm;

architecture hbxr of lwtsxivm is
  
begin
  
end hbxr;

library ieee;
use ieee.std_logic_1164.all;

entity qszqcxmmd is
  port (nka : inout integer; nhoghuhifw : in time_vector(4 downto 1); wq : linkage std_logic; cpv : linkage time);
end qszqcxmmd;

library ieee;
use ieee.std_logic_1164.all;

architecture x of qszqcxmmd is
  signal ahriwrz : std_logic;
  signal wpnjwgs : time;
begin
  g : entity work.lwtsxivm
    port map (orutvem => wpnjwgs, thcqagdr => ahriwrz, bukry => nka);
  
  -- Multi-driven assignments
  ahriwrz <= ahriwrz;
  ahriwrz <= ahriwrz;
  ahriwrz <= ahriwrz;
end x;

library ieee;
use ieee.std_logic_1164.all;

entity oragdychf is
  port (ndkyuqax : out time; wi : inout real; kraj : inout std_logic_vector(3 downto 4));
end oragdychf;

library ieee;
use ieee.std_logic_1164.all;

architecture wpszxsl of oragdychf is
  signal er : std_logic;
  signal tm : time;
  signal ebmgfzqjkp : time_vector(4 downto 1);
  signal pgiipfez : integer;
  signal w : std_logic;
begin
  l : entity work.lwtsxivm
    port map (orutvem => ndkyuqax, thcqagdr => w, bukry => pgiipfez);
  zpgwt : entity work.qszqcxmmd
    port map (nka => pgiipfez, nhoghuhifw => ebmgfzqjkp, wq => w, cpv => ndkyuqax);
  rrezg : entity work.lwtsxivm
    port map (orutvem => tm, thcqagdr => er, bukry => pgiipfez);
  
  -- Single-driven assignments
  wi <= 2_0_1_0_0.311;
  
  -- Multi-driven assignments
  er <= w;
  kraj <= kraj;
  kraj <= kraj;
end wpszxsl;



-- Seed after: 11558126481842941741,4122021602305298647
