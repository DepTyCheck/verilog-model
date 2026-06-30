-- Seed: 5185051619092395192,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity mbzr is
  port (vmbsyczjw : inout std_logic; kcodk : inout real; x : out integer_vector(4 to 1));
end mbzr;

architecture ornz of mbzr is
  
begin
  -- Multi-driven assignments
  vmbsyczjw <= 'L';
end ornz;

entity xxwickej is
  port (t : linkage time);
end xxwickej;

architecture cfyvra of xxwickej is
  
begin
  
end cfyvra;

library ieee;
use ieee.std_logic_1164.all;

entity agpazod is
  port (qpivmvu : inout std_logic);
end agpazod;

architecture pca of agpazod is
  signal asircmemv : integer_vector(4 to 1);
  signal rniyoaforv : real;
  signal vfubvqn : time;
begin
  ffraiwem : entity work.xxwickej
    port map (t => vfubvqn);
  hwqucwto : entity work.mbzr
    port map (vmbsyczjw => qpivmvu, kcodk => rniyoaforv, x => asircmemv);
  
  -- Multi-driven assignments
  qpivmvu <= 'H';
  qpivmvu <= 'L';
  qpivmvu <= 'U';
  qpivmvu <= 'H';
end pca;

library ieee;
use ieee.std_logic_1164.all;

entity pwwfpjnx is
  port (kbxd : linkage time; ztpco : in std_logic);
end pwwfpjnx;

library ieee;
use ieee.std_logic_1164.all;

architecture zyci of pwwfpjnx is
  signal zxrohgllul : std_logic;
begin
  pxfqqqsfz : entity work.agpazod
    port map (qpivmvu => zxrohgllul);
  
  -- Multi-driven assignments
  zxrohgllul <= 'L';
end zyci;



-- Seed after: 14301179135744435089,14629254427735353553
