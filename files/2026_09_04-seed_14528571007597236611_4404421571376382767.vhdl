-- Seed: 14528571007597236611,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity fywu is
  port (xhmik : in integer; zhfk : in time_vector(2 to 1); zytdufk : inout std_logic; za : out std_logic_vector(3 downto 3));
end fywu;

architecture hvxk of fywu is
  
begin
  -- Multi-driven assignments
  za <= "H";
end hvxk;

entity t is
  port (eefxy : inout time_vector(1 to 0); w : in character; y : out real; mwzhpoy : inout integer);
end t;

library ieee;
use ieee.std_logic_1164.all;

architecture ss of t is
  signal cnngqvrmxc : std_logic_vector(3 downto 3);
  signal qheqi : std_logic;
  signal m : time_vector(2 to 1);
  signal beuwxvf : std_logic;
  signal zifcpvxk : std_logic_vector(3 downto 3);
  signal yjuwhsf : std_logic;
  signal ngldfbbq : time_vector(2 to 1);
  signal zunqcpwt : std_logic_vector(3 downto 3);
  signal a : std_logic;
  signal prpsjyf : time_vector(2 to 1);
  signal lvmifn : integer;
begin
  dqtteqoi : entity work.fywu
    port map (xhmik => lvmifn, zhfk => prpsjyf, zytdufk => a, za => zunqcpwt);
  n : entity work.fywu
    port map (xhmik => lvmifn, zhfk => ngldfbbq, zytdufk => yjuwhsf, za => zifcpvxk);
  smh : entity work.fywu
    port map (xhmik => mwzhpoy, zhfk => eefxy, zytdufk => beuwxvf, za => zunqcpwt);
  fv : entity work.fywu
    port map (xhmik => mwzhpoy, zhfk => m, zytdufk => qheqi, za => cnngqvrmxc);
  
  -- Single-driven assignments
  prpsjyf <= eefxy;
  
  -- Multi-driven assignments
  yjuwhsf <= a;
  zifcpvxk <= "Z";
end ss;

library ieee;
use ieee.std_logic_1164.all;

entity wzwwfsvfq is
  port (ann : linkage real; wxxh : linkage std_logic; c : inout boolean_vector(1 to 3));
end wzwwfsvfq;

library ieee;
use ieee.std_logic_1164.all;

architecture kbveiq of wzwwfsvfq is
  signal ghyqdo : std_logic_vector(3 downto 3);
  signal moov : std_logic;
  signal vwagcjteek : time_vector(2 to 1);
  signal anif : integer;
begin
  qawtniur : entity work.fywu
    port map (xhmik => anif, zhfk => vwagcjteek, zytdufk => moov, za => ghyqdo);
  
  -- Multi-driven assignments
  moov <= moov;
  moov <= moov;
  moov <= '0';
end kbveiq;



-- Seed after: 13252070183464523392,4404421571376382767
