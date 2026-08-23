-- Seed: 3726901697884564765,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity bf is
  port ( ugpwuqshtr : inout std_logic
  ; iktofdh : out bit_vector(3 to 4)
  ; z : linkage std_logic_vector(1 downto 4)
  ; jiryvb : inout real_vector(3 downto 3)
  );
end bf;

architecture ksvrk of bf is
  
begin
  -- Single-driven assignments
  jiryvb <= (others => 8#7732.2_1_7_2#);
end ksvrk;

entity es is
  port (lj : out real; quvvnaaji : in boolean);
end es;

library ieee;
use ieee.std_logic_1164.all;

architecture fvbbwlx of es is
  signal esauoxx : real_vector(3 downto 3);
  signal zumrlmyghv : bit_vector(3 to 4);
  signal hnjotdlh : std_logic;
  signal qyxtcxg : real_vector(3 downto 3);
  signal plyqp : std_logic_vector(1 downto 4);
  signal vzwgcdiqo : bit_vector(3 to 4);
  signal fscqtilxqo : std_logic;
begin
  dqc : entity work.bf
    port map (ugpwuqshtr => fscqtilxqo, iktofdh => vzwgcdiqo, z => plyqp, jiryvb => qyxtcxg);
  umow : entity work.bf
    port map (ugpwuqshtr => hnjotdlh, iktofdh => zumrlmyghv, z => plyqp, jiryvb => esauoxx);
  
  -- Single-driven assignments
  lj <= lj;
  
  -- Multi-driven assignments
  fscqtilxqo <= fscqtilxqo;
  plyqp <= "";
  fscqtilxqo <= 'L';
end fvbbwlx;



-- Seed after: 16704170882662347515,4245627776430562977
