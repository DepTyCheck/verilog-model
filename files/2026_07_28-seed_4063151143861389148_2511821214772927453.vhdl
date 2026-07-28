-- Seed: 4063151143861389148,2511821214772927453

library ieee;
use ieee.std_logic_1164.all;

entity fghttxtmr is
  port (jzt : in time; n : in boolean_vector(0 downto 3); sldgplu : inout std_logic; zksslixxle : in std_logic_vector(0 downto 0));
end fghttxtmr;

architecture xocfs of fghttxtmr is
  
begin
  -- Multi-driven assignments
  sldgplu <= '0';
end xocfs;

library ieee;
use ieee.std_logic_1164.all;

entity uqwfygpxy is
  port (kfhv : inout std_logic; xftjzfz : out std_logic_vector(2 downto 4));
end uqwfygpxy;

library ieee;
use ieee.std_logic_1164.all;

architecture cc of uqwfygpxy is
  signal rjncmszzq : std_logic;
  signal le : std_logic_vector(0 downto 0);
  signal aldnhm : std_logic;
  signal ik : std_logic_vector(0 downto 0);
  signal zvc : std_logic;
  signal ybch : time;
  signal i : std_logic_vector(0 downto 0);
  signal z : std_logic;
  signal tyn : boolean_vector(0 downto 3);
  signal ofovzd : time;
begin
  pxhhacfrpf : entity work.fghttxtmr
    port map (jzt => ofovzd, n => tyn, sldgplu => z, zksslixxle => i);
  bj : entity work.fghttxtmr
    port map (jzt => ybch, n => tyn, sldgplu => zvc, zksslixxle => ik);
  ytwhccrkuq : entity work.fghttxtmr
    port map (jzt => ybch, n => tyn, sldgplu => aldnhm, zksslixxle => le);
  npeatdjgu : entity work.fghttxtmr
    port map (jzt => ofovzd, n => tyn, sldgplu => rjncmszzq, zksslixxle => ik);
  
  -- Single-driven assignments
  ofovzd <= 2#1_0_0_1# fs;
  tyn <= (others => TRUE);
  
  -- Multi-driven assignments
  xftjzfz <= (others => '0');
  rjncmszzq <= 'U';
  ik <= (others => 'X');
end cc;

library ieee;
use ieee.std_logic_1164.all;

entity nhqv is
  port (ny : out integer; clanygucj : buffer std_logic_vector(4 downto 4));
end nhqv;

library ieee;
use ieee.std_logic_1164.all;

architecture eoimtlvpw of nhqv is
  signal mdp : std_logic_vector(2 downto 4);
  signal oa : std_logic_vector(2 downto 4);
  signal nbyjjprrkz : std_logic;
begin
  izcwbqqs : entity work.uqwfygpxy
    port map (kfhv => nbyjjprrkz, xftjzfz => oa);
  pqzknc : entity work.uqwfygpxy
    port map (kfhv => nbyjjprrkz, xftjzfz => mdp);
  
  -- Single-driven assignments
  ny <= ny;
  
  -- Multi-driven assignments
  nbyjjprrkz <= '1';
end eoimtlvpw;



-- Seed after: 864932289940800977,2511821214772927453
