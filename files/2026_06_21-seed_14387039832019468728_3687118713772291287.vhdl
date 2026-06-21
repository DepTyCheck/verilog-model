-- Seed: 14387039832019468728,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity bdkrqjr is
  port (tkpwdfgiiw : linkage std_logic_vector(4 downto 1); lrsmyxd : in std_logic; dnhci : out std_logic_vector(2 downto 0));
end bdkrqjr;

architecture fkgndbzng of bdkrqjr is
  
begin
  -- Multi-driven assignments
  dnhci <= "WL1";
  dnhci <= ('1', '0', 'Z');
  dnhci <= "Z11";
end fkgndbzng;

entity gqrdwe is
  port (kppo : in boolean);
end gqrdwe;

library ieee;
use ieee.std_logic_1164.all;

architecture uyjmqlmhr of gqrdwe is
  signal qvoakyqvf : std_logic_vector(2 downto 0);
  signal kdwjm : std_logic_vector(2 downto 0);
  signal ptilsvj : std_logic;
  signal kehdsem : std_logic_vector(4 downto 1);
  signal kd : std_logic;
  signal jshsknvak : std_logic_vector(4 downto 1);
  signal seu : std_logic_vector(2 downto 0);
  signal rpxmz : std_logic;
  signal fosipyfchs : std_logic_vector(4 downto 1);
begin
  irwoidv : entity work.bdkrqjr
    port map (tkpwdfgiiw => fosipyfchs, lrsmyxd => rpxmz, dnhci => seu);
  bgm : entity work.bdkrqjr
    port map (tkpwdfgiiw => jshsknvak, lrsmyxd => kd, dnhci => seu);
  ovzxld : entity work.bdkrqjr
    port map (tkpwdfgiiw => kehdsem, lrsmyxd => ptilsvj, dnhci => kdwjm);
  zbp : entity work.bdkrqjr
    port map (tkpwdfgiiw => fosipyfchs, lrsmyxd => rpxmz, dnhci => qvoakyqvf);
end uyjmqlmhr;

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (rbgoczrdd : buffer std_logic_vector(4 downto 0); jtnadjk : in real; ql : in time; axidhjnid : linkage real);
end m;

architecture jhrrbkxikr of m is
  signal fvwbmac : boolean;
  signal nag : boolean;
begin
  dmvv : entity work.gqrdwe
    port map (kppo => nag);
  nmolsvyr : entity work.gqrdwe
    port map (kppo => fvwbmac);
  
  -- Single-driven assignments
  nag <= FALSE;
  fvwbmac <= FALSE;
end jhrrbkxikr;



-- Seed after: 14952072520331563644,3687118713772291287
