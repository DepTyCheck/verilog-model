-- Seed: 7911552909424440426,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity rdcvacl is
  port (dgpqmq : in std_logic; tofkf : linkage std_logic_vector(3 downto 0));
end rdcvacl;

architecture o of rdcvacl is
  
begin
  
end o;

library ieee;
use ieee.std_logic_1164.all;

entity jmda is
  port (gaxvzn : out time; vyiqwexn : inout std_logic_vector(2 to 4); juwglxqbk : in time);
end jmda;

library ieee;
use ieee.std_logic_1164.all;

architecture pw of jmda is
  signal di : std_logic_vector(3 downto 0);
  signal sajqgry : std_logic;
  signal koiuv : std_logic;
  signal qntgqjfsh : std_logic_vector(3 downto 0);
  signal d : std_logic;
begin
  dy : entity work.rdcvacl
    port map (dgpqmq => d, tofkf => qntgqjfsh);
  cdxltypns : entity work.rdcvacl
    port map (dgpqmq => koiuv, tofkf => qntgqjfsh);
  bcmrl : entity work.rdcvacl
    port map (dgpqmq => koiuv, tofkf => qntgqjfsh);
  lclxd : entity work.rdcvacl
    port map (dgpqmq => sajqgry, tofkf => di);
  
  -- Single-driven assignments
  gaxvzn <= juwglxqbk;
  
  -- Multi-driven assignments
  di <= qntgqjfsh;
  di <= ('U', 'Z', 'U', 'X');
  sajqgry <= '-';
end pw;

entity zmxhs is
  port (xozu : in boolean_vector(3 to 0); okcwi : buffer integer);
end zmxhs;

library ieee;
use ieee.std_logic_1164.all;

architecture mlsb of zmxhs is
  signal rwsrgea : std_logic_vector(3 downto 0);
  signal nrpaq : std_logic_vector(3 downto 0);
  signal cqauzrqzy : std_logic;
begin
  ytn : entity work.rdcvacl
    port map (dgpqmq => cqauzrqzy, tofkf => nrpaq);
  dzjlflmsfc : entity work.rdcvacl
    port map (dgpqmq => cqauzrqzy, tofkf => rwsrgea);
  enzxxrts : entity work.rdcvacl
    port map (dgpqmq => cqauzrqzy, tofkf => nrpaq);
  
  -- Single-driven assignments
  okcwi <= 420;
  
  -- Multi-driven assignments
  nrpaq <= nrpaq;
  cqauzrqzy <= cqauzrqzy;
  rwsrgea <= "X-1-";
end mlsb;

entity dyhv is
  port (tzoyeobpy : buffer time);
end dyhv;

library ieee;
use ieee.std_logic_1164.all;

architecture rzqx of dyhv is
  signal mvua : integer;
  signal nsx : boolean_vector(3 to 0);
  signal vgfeo : std_logic_vector(3 downto 0);
  signal vypyuuuzhm : std_logic;
begin
  rh : entity work.rdcvacl
    port map (dgpqmq => vypyuuuzhm, tofkf => vgfeo);
  nk : entity work.zmxhs
    port map (xozu => nsx, okcwi => mvua);
  yihnkab : entity work.rdcvacl
    port map (dgpqmq => vypyuuuzhm, tofkf => vgfeo);
  
  -- Single-driven assignments
  tzoyeobpy <= 2 sec;
  nsx <= nsx;
  
  -- Multi-driven assignments
  vgfeo <= "0ZX-";
  vypyuuuzhm <= 'L';
  vypyuuuzhm <= 'W';
end rzqx;



-- Seed after: 13612010469694976568,13788294582928374589
