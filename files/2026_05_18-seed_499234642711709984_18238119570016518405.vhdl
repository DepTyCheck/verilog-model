-- Seed: 499234642711709984,18238119570016518405

library ieee;
use ieee.std_logic_1164.all;

entity xsiguq is
  port (xtsynda : out std_logic; fydv : out time_vector(4 downto 2));
end xsiguq;



architecture bckyyo of xsiguq is
  
begin
  
end bckyyo;

library ieee;
use ieee.std_logic_1164.all;

entity mknk is
  port (rdta : linkage real; woyfybih : in integer; ngyq : linkage time; kfp : in std_logic_vector(1 downto 3));
end mknk;

library ieee;
use ieee.std_logic_1164.all;

architecture gbh of mknk is
  signal mlxa : time_vector(4 downto 2);
  signal s : time_vector(4 downto 2);
  signal x : time_vector(4 downto 2);
  signal apvs : std_logic;
  signal eeyo : time_vector(4 downto 2);
  signal kvipume : std_logic;
begin
  tzkdo : entity work.xsiguq
    port map (xtsynda => kvipume, fydv => eeyo);
  yjnn : entity work.xsiguq
    port map (xtsynda => apvs, fydv => x);
  izdnl : entity work.xsiguq
    port map (xtsynda => kvipume, fydv => s);
  ncpzqj : entity work.xsiguq
    port map (xtsynda => kvipume, fydv => mlxa);
end gbh;

library ieee;
use ieee.std_logic_1164.all;

entity ksiuxdvpqg is
  port (pclslast : buffer std_logic_vector(1 to 3); zimdrypi : inout severity_level; unh : in time; bahn : in std_logic_vector(1 downto 1));
end ksiuxdvpqg;

library ieee;
use ieee.std_logic_1164.all;

architecture jsbd of ksiuxdvpqg is
  signal budlvwoxus : time_vector(4 downto 2);
  signal twf : std_logic;
  signal icmewheuh : std_logic_vector(1 downto 3);
  signal uvrct : integer;
  signal aelc : real;
begin
  uelcx : entity work.mknk
    port map (rdta => aelc, woyfybih => uvrct, ngyq => unh, kfp => icmewheuh);
  smwufzs : entity work.xsiguq
    port map (xtsynda => twf, fydv => budlvwoxus);
end jsbd;



-- Seed after: 2296311481989821028,18238119570016518405
