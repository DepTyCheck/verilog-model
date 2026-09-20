-- Seed: 4910833878386310309,18037650846010261179

library ieee;
use ieee.std_logic_1164.all;

entity dosdtgf is
  port (dbawtxn : linkage real; obrcvp : inout std_logic_vector(0 downto 0); jqes : out bit; laefwxfc : inout time_vector(1 downto 3));
end dosdtgf;

architecture qhmtvvlx of dosdtgf is
  
begin
  
end qhmtvvlx;

library ieee;
use ieee.std_logic_1164.all;

entity lberj is
  port (ejvewgd : inout std_logic; mh : buffer real);
end lberj;

library ieee;
use ieee.std_logic_1164.all;

architecture sb of lberj is
  signal fcwxfnlms : time_vector(1 downto 3);
  signal ifuhtlqt : bit;
  signal zxtzhob : real;
  signal hgskhbkhqb : time_vector(1 downto 3);
  signal khjjbdkkdz : bit;
  signal u : std_logic_vector(0 downto 0);
  signal ua : real;
begin
  udsaqatwnv : entity work.dosdtgf
    port map (dbawtxn => ua, obrcvp => u, jqes => khjjbdkkdz, laefwxfc => hgskhbkhqb);
  bj : entity work.dosdtgf
    port map (dbawtxn => zxtzhob, obrcvp => u, jqes => ifuhtlqt, laefwxfc => fcwxfnlms);
  
  -- Multi-driven assignments
  u <= (others => 'H');
  ejvewgd <= 'L';
end sb;

library ieee;
use ieee.std_logic_1164.all;

entity mjumitrtjq is
  port (khxzpwuv : inout std_logic_vector(0 downto 3); xlalm : inout integer);
end mjumitrtjq;

library ieee;
use ieee.std_logic_1164.all;

architecture ybir of mjumitrtjq is
  signal ymflraffr : real;
  signal igsid : std_logic;
  signal j : time_vector(1 downto 3);
  signal pkdg : bit;
  signal wj : std_logic_vector(0 downto 0);
  signal oeg : real;
  signal zqxv : time_vector(1 downto 3);
  signal sjgc : bit;
  signal a : std_logic_vector(0 downto 0);
  signal yfxpzou : real;
begin
  ykwjdkq : entity work.dosdtgf
    port map (dbawtxn => yfxpzou, obrcvp => a, jqes => sjgc, laefwxfc => zqxv);
  sdzowhh : entity work.dosdtgf
    port map (dbawtxn => oeg, obrcvp => wj, jqes => pkdg, laefwxfc => j);
  ozecegqvee : entity work.lberj
    port map (ejvewgd => igsid, mh => ymflraffr);
  
  -- Single-driven assignments
  xlalm <= xlalm;
  
  -- Multi-driven assignments
  khxzpwuv <= khxzpwuv;
  wj <= (others => '-');
end ybir;



-- Seed after: 1920485119967661144,18037650846010261179
