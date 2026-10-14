-- Seed: 4355613922790162398,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity tyjnguoi is
  port (a : inout std_logic_vector(0 downto 2));
end tyjnguoi;

architecture yjgvjshlbx of tyjnguoi is
  
begin
  -- Multi-driven assignments
  a <= (others => '0');
  a <= a;
  a <= (others => '0');
  a <= a;
end yjgvjshlbx;

library ieee;
use ieee.std_logic_1164.all;

entity bwmk is
  port (ru : buffer time; mrut : linkage std_logic_vector(2 downto 4); ga : buffer severity_level; gnti : inout std_logic_vector(4 downto 0));
end bwmk;

library ieee;
use ieee.std_logic_1164.all;

architecture qjzkjoax of bwmk is
  signal bvjt : std_logic_vector(0 downto 2);
  signal fzrwmxmz : std_logic_vector(0 downto 2);
begin
  exkznl : entity work.tyjnguoi
    port map (a => fzrwmxmz);
  hvwejj : entity work.tyjnguoi
    port map (a => fzrwmxmz);
  jkdlgffa : entity work.tyjnguoi
    port map (a => bvjt);
  yytxjsh : entity work.tyjnguoi
    port map (a => fzrwmxmz);
  
  -- Single-driven assignments
  ga <= ga;
end qjzkjoax;

entity qoksrs is
  port (hcbruubgz : buffer real);
end qoksrs;

library ieee;
use ieee.std_logic_1164.all;

architecture mngdnnnlq of qoksrs is
  signal cvmvxkqth : std_logic_vector(0 downto 2);
begin
  ghitpub : entity work.tyjnguoi
    port map (a => cvmvxkqth);
  
  -- Single-driven assignments
  hcbruubgz <= 100.2;
  
  -- Multi-driven assignments
  cvmvxkqth <= cvmvxkqth;
end mngdnnnlq;

library ieee;
use ieee.std_logic_1164.all;

entity hjfkifqnbv is
  port (uheleqoo : buffer real; qu : in time; oghp : out std_logic);
end hjfkifqnbv;

library ieee;
use ieee.std_logic_1164.all;

architecture bflfk of hjfkifqnbv is
  signal krahdxjt : std_logic_vector(4 downto 0);
  signal ynosmgkc : severity_level;
  signal uqbbakydz : time;
  signal rkbbz : severity_level;
  signal whutqqfge : std_logic_vector(2 downto 4);
  signal pcpwxlfw : time;
  signal tq : std_logic_vector(4 downto 0);
  signal uq : severity_level;
  signal zprflt : std_logic_vector(2 downto 4);
  signal ufxfswdsmd : time;
begin
  kjlayghopm : entity work.bwmk
    port map (ru => ufxfswdsmd, mrut => zprflt, ga => uq, gnti => tq);
  tnfgpuqb : entity work.bwmk
    port map (ru => pcpwxlfw, mrut => whutqqfge, ga => rkbbz, gnti => tq);
  flqdzj : entity work.qoksrs
    port map (hcbruubgz => uheleqoo);
  lpelqblh : entity work.bwmk
    port map (ru => uqbbakydz, mrut => zprflt, ga => ynosmgkc, gnti => krahdxjt);
  
  -- Multi-driven assignments
  zprflt <= zprflt;
end bflfk;



-- Seed after: 17854330717720106895,2287986438313874379
